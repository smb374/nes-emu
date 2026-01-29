mod palette;
pub mod registers;

use std::{cell::RefCell, rc::Rc};

use crate::{
    cartridge::Rom,
    mapper::Mirroring,
    ppu::registers::{ControlRegister, InternalRegisters, MaskRegister, StatusRegister},
};

pub struct PPU {
    pub rom: Rc<RefCell<Rom>>,
    pub ctrl: ControlRegister,
    pub mask: MaskRegister,
    pub status: StatusRegister,
    pub internal: InternalRegisters,
    pub vram: [u8; 0x800],

    pub oam_addr: u8,
    pub oam_data: [u8; 0x100],
    pub palette_table: [u8; 0x20],
    internal_data_buf: u8,

    pub nmi_interrupt: Option<u8>,
    scanline: u16,
    cycles: usize,

    pub frame_buffer: [u8; 256 * 240 * 3],
    bg_scanline: [u8; 256],
    sprite_scanline: [Option<(u8, bool, bool)>; 256],
}

impl PPU {
    pub fn new(rom: Rc<RefCell<Rom>>) -> Self {
        Self {
            rom,
            ctrl: ControlRegister::new(),
            mask: MaskRegister::new(),
            status: StatusRegister::new(),
            vram: [0; 0x800],
            oam_addr: 0,
            oam_data: [0xFF; 0x100],
            palette_table: [0; 0x20],
            internal_data_buf: 0,
            nmi_interrupt: None,
            scanline: 0,
            cycles: 0,
            internal: InternalRegisters::new(),
            frame_buffer: [0; 256 * 240 * 3],
            bg_scanline: [0; 256],
            sprite_scanline: [None; 256],
        }
    }

    fn increment_vram_addr(&mut self) {
        self.internal.increment_v(self.ctrl.vram_addr_increment()); // Use new register system
    }

    // Render a single scanline into the frame buffer
    fn render_scanline(&mut self) {
        // Clear scanline buffers
        self.bg_scanline.fill(0);
        self.sprite_scanline.fill(None);

        // Render background
        if self.mask.show_background() && self.scanline < 240 {
            let bank = self.ctrl.bknd_pattern_addr();
            let fine_y = self.internal.fine_y();
            let fine_x = self.internal.fine_x();
            let coarse_y = self.internal.coarse_y() as usize;
            let nametable_bits = self.internal.nametable();

            let nametable_base = match (self.mirroring(), nametable_bits) {
                (Mirroring::Vertical, 0)
                | (Mirroring::Vertical, 2)
                | (Mirroring::Horizontal, 0)
                | (Mirroring::Horizontal, 1) => 0,
                (Mirroring::Vertical, 1)
                | (Mirroring::Vertical, 3)
                | (Mirroring::Horizontal, 2)
                | (Mirroring::Horizontal, 3) => 0x400,
                _ => 0,
            };

            for tile_idx in 0..33 {
                let coarse_x = ((self.internal.coarse_x() as usize + tile_idx) & 0x1F) as usize;
                let mut current_nt_base = nametable_base;
                if tile_idx >= 32 - self.internal.coarse_x() as usize {
                    current_nt_base ^= 0x400;
                }

                let nt_offset = coarse_y * 32 + coarse_x;
                let tile_num = if current_nt_base + nt_offset < self.vram.len() {
                    self.vram[current_nt_base + nt_offset]
                } else {
                    0
                };

                let attr_offset = 0x3C0 + (coarse_y / 4) * 8 + (coarse_x / 4);
                let attr_byte = if current_nt_base + attr_offset < self.vram.len() {
                    self.vram[current_nt_base + attr_offset]
                } else {
                    0
                };

                let palette_idx = match (coarse_x % 4 / 2, coarse_y % 4 / 2) {
                    (0, 0) => attr_byte & 0b11,
                    (1, 0) => (attr_byte >> 2) & 0b11,
                    (0, 1) => (attr_byte >> 4) & 0b11,
                    (1, 1) => (attr_byte >> 6) & 0b11,
                    _ => 0,
                };

                let tile_addr = bank + (tile_num as u16) * 16 + fine_y as u16;
                let tile_low = self.rom.borrow().read_chr(tile_addr);
                let tile_high = self.rom.borrow().read_chr(tile_addr + 8);

                for pixel in 0..8 {
                    let bit = 7 - pixel;
                    let color_bits = ((tile_high >> bit) & 1) << 1 | ((tile_low >> bit) & 1);
                    let screen_x = (tile_idx * 8 + pixel).wrapping_sub(fine_x as usize);

                    if screen_x < 256 {
                        if color_bits == 0 {
                            self.bg_scanline[screen_x] = 0;
                        } else {
                            self.bg_scanline[screen_x] = palette_idx * 4 + color_bits;
                        }
                    }
                }
            }
        }

        // Render sprites
        if self.mask.show_sprites() && self.scanline < 240 {
            let sprite_size = self.ctrl.sprite_size(); // Usually returns 8 or 16

            for sprite_idx in (0..64).rev() {
                let oam_offset = sprite_idx * 4;
                let sprite_y = self.oam_data[oam_offset] as u16;
                let tile_num = self.oam_data[oam_offset + 1];
                let attributes = self.oam_data[oam_offset + 2];
                let sprite_x = self.oam_data[oam_offset + 3] as usize;

                if self.scanline < sprite_y || self.scanline >= sprite_y + sprite_size as u16 {
                    continue;
                }

                let palette_idx = attributes & 0b11;
                let priority = (attributes >> 5) & 1 == 1;
                let flip_h = (attributes >> 6) & 1 == 1;
                let flip_v = (attributes >> 7) & 1 == 1;
                let is_sprite_0 = sprite_idx == 0;

                let mut y_offset = self.scanline - sprite_y;
                if flip_v {
                    y_offset = sprite_size as u16 - 1 - y_offset;
                }

                // Handle 8x16 sprites correctly
                let tile_addr = if sprite_size == 16 {
                    // Bit 0 selects the pattern table bank ($0000 or $1000)
                    let bank = (tile_num as u16 & 0x01) * 0x1000;
                    // Bits 7-1 are the tile index; top half is tile, bottom half is tile + 1
                    let tile_index = (tile_num & 0xFE) as u16 + (y_offset / 8);
                    bank + (tile_index * 16) + (y_offset % 8)
                } else {
                    // Standard 8x8 behavior
                    let bank = self.ctrl.sprt_pattern_addr();
                    bank + (tile_num as u16 * 16) + y_offset
                };

                let tile_low = self.rom.borrow().read_chr(tile_addr);
                let tile_high = self.rom.borrow().read_chr(tile_addr + 8);

                for pixel in 0..8 {
                    let bit = if flip_h { pixel } else { 7 - pixel };
                    let color_bits = ((tile_high >> bit) & 1) << 1 | ((tile_low >> bit) & 1);

                    if color_bits == 0 {
                        continue;
                    }

                    let screen_x = sprite_x + pixel;
                    if screen_x < 256 && self.sprite_scanline[screen_x].is_none() {
                        let palette_value = 16 + palette_idx * 4 + color_bits;
                        self.sprite_scanline[screen_x] =
                            Some((palette_value, priority, is_sprite_0));
                    }
                }
            }
        }

        // Merge buffers and detect sprite 0 hit
        let mut sprite_0_hit = false;
        for x in 0..256 {
            let bg_palette_idx = self.bg_scanline[x];
            let sprite_pixel = self.sprite_scanline[x];

            let final_palette_idx = match sprite_pixel {
                Some((sprite_pal_idx, priority, is_sprite_0)) => {
                    if is_sprite_0 && bg_palette_idx != 0 && sprite_pal_idx != 0 {
                        if x != 255 && self.mask.show_background() && self.mask.show_sprites() {
                            sprite_0_hit = true;
                        }
                    }

                    if bg_palette_idx == 0 {
                        sprite_pal_idx
                    } else if priority {
                        bg_palette_idx
                    } else {
                        sprite_pal_idx
                    }
                }
                None => bg_palette_idx,
            };

            let rgb = if final_palette_idx == 0 {
                palette::SYSTEM_PALETTE[self.palette_table[0] as usize]
            } else {
                palette::SYSTEM_PALETTE[self.palette_table[final_palette_idx as usize] as usize]
            };

            let offset = (self.scanline as usize * 256 + x) * 3;
            if offset + 2 < self.frame_buffer.len() {
                self.frame_buffer[offset] = rgb.0;
                self.frame_buffer[offset + 1] = rgb.1;
                self.frame_buffer[offset + 2] = rgb.2;
            }
        }

        if sprite_0_hit && !self.status.contains(StatusRegister::SPRITE_ZERO_HIT) {
            self.status.set(StatusRegister::SPRITE_ZERO_HIT, true);
        }
    }

    pub fn tick(&mut self, cycles: u16) -> bool {
        self.cycles += cycles as usize;
        // Check if we've completed a scanline
        if self.cycles >= 341 {
            // Handle end-of-scanline operations for visible scanlines
            if self.scanline < 240 {
                if self.mask.show_background() || self.mask.show_sprites() {
                    // Increment X 32 times (once per tile)
                    for _ in 0..32 {
                        self.internal.increment_x();
                    }
                    // Increment Y at end of visible scanline
                    self.internal.increment_y();
                    // Copy horizontal scroll from t to v
                    self.internal.copy_horizontal();
                }

                self.rom.borrow_mut().ppu_tick(0x0000); // A12=0 (pattern table $0xxx)
                self.rom.borrow_mut().ppu_tick(0x1000); // A12=1 (pattern table $1xxx)

                // Render the completed scanline
                self.render_scanline();
            }
            // Pre-render scanline (261) - copy vertical scroll
            if self.scanline == 261 {
                if self.mask.show_background() || self.mask.show_sprites() {
                    // Increment X 32 times on pre-render scanline too
                    for _ in 0..32 {
                        self.internal.increment_x();
                    }
                    // Copy vertical scroll from t to v
                    self.internal.copy_vertical();

                    // Notify mapper of PPU address changes on pre-render scanline too
                    for _ in 0..42 {
                        self.rom.borrow_mut().ppu_tick(0x0000);
                        self.rom.borrow_mut().ppu_tick(0x1000);
                    }
                }
                // Clear VBlank and sprite 0 hit flags
                self.status.remove(StatusRegister::VBLANK_STARTED);
                self.status.set(StatusRegister::SPRITE_ZERO_HIT, false);
            }
            self.cycles -= 341;
            self.scanline += 1;
            // Scanline 241: Enter VBlank
            if self.scanline == 241 {
                self.status.set(StatusRegister::VBLANK_STARTED, true);
                if self.ctrl.contains(ControlRegister::GENERATE_NMI) {
                    self.nmi_interrupt = Some(1);
                }
            }
            // Scanline 262: Wrap to scanline 0
            if self.scanline >= 262 {
                self.scanline = 0;
                self.nmi_interrupt = None;
                return true;
            }
        }
        false
    }

    pub fn poll_nmi_interrupt(&mut self) -> Option<u8> {
        self.nmi_interrupt.take()
    }

    pub fn write_to_ctrl(&mut self, value: u8) {
        let before_nmi_status = self.ctrl.contains(ControlRegister::GENERATE_NMI);
        self.ctrl.update(value);
        // Update nametable select in t register
        self.internal.update_nametable_from_ctrl(value);
        if !before_nmi_status
            && self.ctrl.contains(ControlRegister::GENERATE_NMI)
            && self.status.contains(StatusRegister::VBLANK_STARTED)
        {
            self.nmi_interrupt = Some(1);
        }
    }

    pub fn read_mask(&self) -> u8 {
        self.mask.bits()
    }

    pub fn write_to_mask(&mut self, value: u8) {
        self.mask.update(value);
    }

    pub fn read_status(&mut self) -> u8 {
        let data = self.status.bits();
        self.status.remove(StatusRegister::VBLANK_STARTED);
        self.internal.reset_latch(); // Reset new register latch too
        data
    }

    pub fn write_to_oam_addr(&mut self, value: u8) {
        self.oam_addr = value;
    }

    pub fn write_to_oam_data(&mut self, value: u8) {
        self.oam_data[self.oam_addr as usize] = value;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    pub fn read_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    pub fn write_to_scroll(&mut self, value: u8) {
        self.internal.write_scroll(value); // Use new register system
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.internal.write_addr(value); // Use new register system
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.internal.get_v();
        self.increment_vram_addr();

        match addr {
            0..=0x3EFF => {
                // Buffered read for CHR/nametables
                let result = self.internal_data_buf;
                self.internal_data_buf = self.read_vram(addr);
                result
            }
            0x3F00..=0x3FFF => {
                // Palette reads are immediate
                self.internal_data_buf = self.read_vram(addr - 0x1000);
                self.read_vram(addr)
            }
            _ => 0,
        }
    }

    pub fn write_data(&mut self, val: u8) {
        let addr = self.internal.get_v();
        self.write_vram(addr, val);
        self.increment_vram_addr();
    }

    pub fn write_oam_dma(&mut self, data: &[u8; 256]) {
        for x in data.iter() {
            self.oam_data[self.oam_addr as usize] = *x;
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    // Internal VRAM/CHR access
    fn read_vram(&self, addr: u16) -> u8 {
        let addr = addr & 0x3FFF; // Mirror down to 14-bit address space

        match addr {
            0x0000..=0x1FFF => {
                // CHR ROM/RAM
                self.rom.borrow().read_chr(addr)
            }
            0x2000..=0x2FFF => {
                // Nametables (with mirroring)
                let mirrored_addr = self.mirror_vram_addr(addr);
                self.vram[mirrored_addr as usize]
            }
            0x3000..=0x3EFF => {
                // Mirror of 0x2000-0x2EFF
                let mirrored_addr = self.mirror_vram_addr(addr - 0x1000);
                self.vram[mirrored_addr as usize]
            }
            0x3F00..=0x3FFF => {
                // Palette RAM
                let palette_addr = (addr - 0x3F00) & 0x1F;
                // Handle palette mirroring
                let palette_addr = if palette_addr == 0x10
                    || palette_addr == 0x14
                    || palette_addr == 0x18
                    || palette_addr == 0x1C
                {
                    palette_addr - 0x10
                } else {
                    palette_addr
                };
                self.palette_table[palette_addr as usize]
            }
            _ => 0,
        }
    }

    fn write_vram(&mut self, addr: u16, val: u8) {
        let addr = addr & 0x3FFF;

        match addr {
            0x0000..=0x1FFF => {
                // CHR ROM/RAM
                self.rom.borrow_mut().write_chr(addr, val);
            }
            0x2000..=0x2FFF => {
                let mirrored_addr = self.mirror_vram_addr(addr);
                self.vram[mirrored_addr as usize] = val;
            }
            0x3000..=0x3EFF => {
                let mirrored_addr = self.mirror_vram_addr(addr - 0x1000);
                self.vram[mirrored_addr as usize] = val;
            }

            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1c => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3f00) as usize] = val;
            }
            0x3F00..=0x3FFF => {
                self.palette_table[(addr - 0x3F00) as usize] = val;
            }
            _ => {}
        }
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        use crate::mapper::Mirroring;

        let mirroring = self.rom.borrow().mirroring();
        let vram_index = (addr & 0x2FFF) - 0x2000; // 0x2000-0x2FFF -> 0x0000-0x0FFF
        let nametable = vram_index / 0x0400; // Which nametable (0-3)

        match mirroring {
            Mirroring::Horizontal => {
                // 0,0; 1,0
                // 0,1; 1,1
                match nametable {
                    0 | 1 => vram_index & 0x03FF,
                    2 | 3 => (vram_index & 0x03FF) + 0x0400,
                    _ => unreachable!(),
                }
            }
            Mirroring::Vertical => {
                // 0,0; 0,1
                // 1,0; 1,1
                vram_index & 0x07FF
            }
            Mirroring::SingleScreenLower => vram_index & 0x03FF,
            Mirroring::SingleScreenUpper => (vram_index & 0x03FF) + 0x0400,
            Mirroring::FourScreen => vram_index, // Not supported in 2KB VRAM
        }
    }

    pub fn get_current_vram_addr(&self) -> u16 {
        self.internal.get_v()
    }

    pub fn mirroring(&self) -> Mirroring {
        self.rom.borrow().mirroring()
    }
}
