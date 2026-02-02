mod palette;
pub mod registers;

use crate::{
    cartridge::Rom,
    mapper::Mirroring,
    ppu::registers::{ControlRegister, InternalRegisters, MaskRegister, StatusRegister},
};

pub struct PPU {
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
    // Buffer to store sprite data for the current scanline (calculated at start of line)
    sprite_scanline: [Option<(u8, bool, bool)>; 256],

    // Background rendering shift registers
    bg_pattern_lo: u16,
    bg_pattern_hi: u16,
    bg_attrib_lo: u16,
    bg_attrib_hi: u16,

    // Latches for the next tile
    bg_nt_id: u8,
    bg_nt_attrib: u8,
    bg_nt_lsb: u8,
    bg_nt_msb: u8,

    // A12 IRQ state
    a12_state: bool,
    a12lo_period: usize,

    // Odd frame state
    odd_frame: bool,
}

impl PPU {
    pub fn new() -> Self {
        Self {
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
            sprite_scanline: [None; 256],

            bg_pattern_lo: 0,
            bg_pattern_hi: 0,
            bg_attrib_lo: 0,
            bg_attrib_hi: 0,
            bg_nt_id: 0,
            bg_nt_attrib: 0,
            bg_nt_lsb: 0,
            bg_nt_msb: 0,

            a12_state: false,
            a12lo_period: 0,

            odd_frame: false,
        }
    }

    pub fn tick(&mut self, rom: &mut Rom, cycles: u8) {
        for _ in 0..cycles {
            let rendering_enabled = self.mask.show_background() || self.mask.show_sprites();
            let visible_scanline = self.scanline < 240;
            let pre_render_scanline = self.scanline == 261;

            if pre_render_scanline && self.cycles == 1 {
                self.status.remove(StatusRegister::VBLANK_STARTED);
                self.status.remove(StatusRegister::SPRITE_ZERO_HIT);
                self.status.remove(StatusRegister::SPRITE_OVERFLOW);
            }

            if self.scanline == 241 && self.cycles == 1 {
                self.status.set(StatusRegister::VBLANK_STARTED, true);
                if self.ctrl.contains(ControlRegister::GENERATE_NMI) {
                    self.nmi_interrupt = Some(1);
                }
            }

            if rendering_enabled {
                if visible_scanline || pre_render_scanline {
                    // Emulate sprite evaluation
                    if visible_scanline && self.cycles == 0 {
                        self.evaluate_sprites(rom);
                    }

                    // Background Rendering & Shifting
                    if (self.cycles >= 1 && self.cycles <= 256)
                        || (self.cycles >= 321 && self.cycles <= 336)
                    {
                        // Shift registers every cycle
                        self.update_shifters();

                        // Perform Fetch Pipeline (Every 8 cycles)
                        match (self.cycles - 1) % 8 {
                            0 => {
                                // Load the shifters with the previous tile's data
                                self.load_bg_shifters();

                                // Fetch Nametable Byte
                                let v = self.internal.get_v();
                                let addr = 0x2000 | (v & 0x0FFF);
                                self.bg_nt_id = self.peek_vram(rom, addr);
                            }
                            2 => {
                                // Fetch Attribute Byte
                                let v = self.internal.get_v();
                                let addr =
                                    0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07);
                                let shift = ((v >> 4) & 4) | (v & 2);
                                self.bg_nt_attrib = (self.peek_vram(rom, addr) >> shift) & 3;
                            }
                            4 => {
                                // Fetch Pattern Table Low
                                let addr = self.ctrl.bknd_pattern_addr()
                                    + (self.bg_nt_id as u16 * 16)
                                    + self.internal.fine_y() as u16;
                                self.bg_nt_lsb = rom.read_chr(addr);
                            }
                            6 => {
                                // Fetch Pattern Table High
                                let addr = self.ctrl.bknd_pattern_addr()
                                    + (self.bg_nt_id as u16 * 16)
                                    + self.internal.fine_y() as u16
                                    + 8;
                                self.bg_nt_msb = rom.read_chr(addr);
                            }
                            7 => {
                                // Increment Coarse X
                                if rendering_enabled {
                                    self.internal.increment_x();
                                }
                            }
                            _ => {}
                        }
                    }

                    // Increment Y
                    if self.cycles == 256 {
                        self.internal.increment_y();
                    }

                    // Reset X (Copy horizontal scroll bits)
                    if self.cycles == 257 {
                        self.load_bg_shifters();
                        self.internal.copy_horizontal();
                    }

                    // Pre-render Only: Reset Y (Copy vertical scroll bits)
                    if pre_render_scanline && self.cycles >= 280 && self.cycles <= 304 {
                        self.internal.copy_vertical();
                    }

                    if (self.cycles >= 1 && self.cycles <= 256)
                        || (self.cycles >= 321 && self.cycles <= 336)
                    {
                        // BG fetches
                        self.handle_a12_check(rom, self.ctrl.bknd_pattern_addr());
                    } else if self.cycles >= 257 && self.cycles <= 320 {
                        // Sprite fetches
                        self.handle_a12_check(rom, self.ctrl.sprt_pattern_addr());
                    }
                }
            }

            if visible_scanline && self.cycles >= 1 && self.cycles <= 256 {
                self.draw_pixel_cycle((self.cycles - 1) as usize, rom);
            }

            self.cycles += 1;
            if self.scanline == 261
                && self.cycles == 339
                && self.odd_frame
                && self.mask.show_background()
            {
                self.cycles = 340; // Skip cycle 339, jump straight to end
            }

            if self.cycles >= 341 {
                self.cycles = 0;
                self.scanline += 1;

                if self.scanline >= 262 {
                    self.scanline = 0;
                    self.nmi_interrupt = None;
                    self.odd_frame = !self.odd_frame; // Toggle frame parity
                }
            }
        }
    }

    fn load_bg_shifters(&mut self) {
        self.bg_pattern_lo = (self.bg_pattern_lo & 0xFF00) | self.bg_nt_lsb as u16;
        self.bg_pattern_hi = (self.bg_pattern_hi & 0xFF00) | self.bg_nt_msb as u16;

        let attr = self.bg_nt_attrib;
        self.bg_attrib_lo = (self.bg_attrib_lo & 0xFF00) | (if (attr & 1) > 0 { 0xFF } else { 0 });
        self.bg_attrib_hi = (self.bg_attrib_hi & 0xFF00) | (if (attr & 2) > 0 { 0xFF } else { 0 });
    }

    fn update_shifters(&mut self) {
        if self.mask.show_background() {
            self.bg_pattern_lo <<= 1;
            self.bg_pattern_hi <<= 1;
            self.bg_attrib_lo <<= 1;
            self.bg_attrib_hi <<= 1;
        }
    }

    fn draw_pixel_cycle(&mut self, x: usize, _rom: &mut Rom) {
        let mut bg_pixel = 0;
        let mut bg_palette = 0;

        // 1. Resolve Background Pixel
        if self.mask.show_background() {
            let bit_mux = 0x8000 >> self.internal.fine_x();

            let p0 = (self.bg_pattern_lo & bit_mux) > 0;
            let p1 = (self.bg_pattern_hi & bit_mux) > 0;
            bg_pixel = (if p1 { 2 } else { 0 }) | (if p0 { 1 } else { 0 });

            let pal0 = (self.bg_attrib_lo & bit_mux) > 0;
            let pal1 = (self.bg_attrib_hi & bit_mux) > 0;
            bg_palette = (if pal1 { 2 } else { 0 }) | (if pal0 { 1 } else { 0 });
        }

        // Left-most 8px masking
        if x < 8 && !self.mask.show_leftmost_background() {
            bg_pixel = 0;
        }

        // 2. Resolve Sprite Pixel (from pre-calculated buffer)
        let sprite_data = if self.mask.show_sprites() {
            if x < 8 && !self.mask.show_leftmost_sprites() {
                None
            } else {
                self.sprite_scanline[x]
            }
        } else {
            None
        };

        // 3. Priority Mux & Sprite 0 Hit
        let (final_pixel, final_palette) = match sprite_data {
            Some((spr_pal, priority, is_sprite_0)) => {
                // Check Sprite 0 Hit: Opaque BG + Opaque Sprite 0 + Rendering Enabled + Not x=255
                if is_sprite_0 && bg_pixel != 0 {
                    if x != 255 && self.mask.show_background() && self.mask.show_sprites() {
                        self.status.set(StatusRegister::SPRITE_ZERO_HIT, true);
                    }
                }

                if priority {
                    // Priority 1 = Behind BG
                    if bg_pixel != 0 {
                        (bg_pixel, bg_palette)
                    } else {
                        (spr_pal & 0x03, (spr_pal >> 2) & 0x03)
                    }
                } else {
                    // Priority 0 = In front of BG
                    if (spr_pal & 0x03) != 0 {
                        (spr_pal & 0x03, (spr_pal >> 2) & 0x03)
                    } else {
                        (bg_pixel, bg_palette)
                    }
                }
            }
            None => (bg_pixel, bg_palette),
        };

        // 4. Write to Framebuffer
        let final_color_idx = if final_pixel == 0 {
            self.read_palette(0) // Universal background
        } else {
            let base =
                if sprite_data.is_some() && (sprite_data.unwrap().1 == false || bg_pixel == 0) {
                    0x10 // Sprite base
                } else {
                    0x00 // BG base
                };
            self.read_palette(base + final_palette * 4 + final_pixel)
        };

        let rgb = palette::SYSTEM_PALETTE[final_color_idx as usize];
        let offset = (self.scanline as usize * 256 + x) * 3;
        if offset + 2 < self.frame_buffer.len() {
            self.frame_buffer[offset] = rgb.0;
            self.frame_buffer[offset + 1] = rgb.1;
            self.frame_buffer[offset + 2] = rgb.2;
        }
    }

    fn evaluate_sprites(&mut self, rom: &mut Rom) {
        self.sprite_scanline.fill(None);
        let sprite_size = self.ctrl.sprite_size();
        let mut sprite_count = 0;

        for sprite_idx in (0..64).rev() {
            let oam_offset = sprite_idx * 4;
            let sprite_y = self.oam_data[oam_offset] as u16;

            // Check if sprite is on this scanline
            if self.scanline < sprite_y + 1 || self.scanline >= sprite_y + 1 + sprite_size as u16 {
                continue;
            }

            sprite_count += 1;
            if sprite_count > 8 {
                self.status.insert(StatusRegister::SPRITE_OVERFLOW);
                break;
            }

            let tile_num = self.oam_data[oam_offset + 1];
            let attributes = self.oam_data[oam_offset + 2];
            let sprite_x = self.oam_data[oam_offset + 3] as usize;

            let palette_idx = attributes & 0b11;
            let priority = (attributes >> 5) & 1 == 1; // 1 = Behind BG
            let flip_h = (attributes >> 6) & 1 == 1;
            let flip_v = (attributes >> 7) & 1 == 1;
            let is_sprite_0 = sprite_idx == 0;

            let mut y_offset = self.scanline - (sprite_y + 1);
            if flip_v {
                y_offset = sprite_size as u16 - 1 - y_offset;
            }

            // Address calc
            let tile_addr = if sprite_size == 16 {
                let bank = (tile_num as u16 & 0x01) * 0x1000;
                let tile_index = (tile_num & 0xFE) as u16 + (y_offset / 8);
                bank + (tile_index * 16) + (y_offset % 8)
            } else {
                let bank = self.ctrl.sprt_pattern_addr();
                bank + (tile_num as u16 * 16) + y_offset
            };

            let tile_low = rom.read_chr(tile_addr);
            let tile_high = rom.read_chr(tile_addr + 8);

            for pixel in 0..8 {
                let bit = if flip_h { pixel } else { 7 - pixel };
                let color_bits = ((tile_high >> bit) & 1) << 1 | ((tile_low >> bit) & 1);

                if color_bits == 0 {
                    continue;
                }

                let screen_x = sprite_x + pixel;
                if screen_x < 256 {
                    let val = 0x10 | (palette_idx << 2) | color_bits;
                    self.sprite_scanline[screen_x] = Some((val, priority, is_sprite_0));
                }
            }
        }
    }

    fn read_palette(&self, idx: u8) -> u8 {
        let addr = 0x3F00 + (idx as u16);
        let mirror = if (addr & 0x0003) == 0 {
            addr & 0x3F0F
        } else {
            addr
        };
        let table_idx = match mirror {
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => mirror - 0x3F10,
            _ => (mirror - 0x3F00) & 0x1F,
        };
        self.palette_table[table_idx as usize]
    }

    fn handle_a12_check(&mut self, rom: &mut Rom, addr: u16) {
        self.update_a12(addr, rom);
    }

    pub fn update_a12(&mut self, addr: u16, rom: &mut Rom) {
        let a12 = (addr & 0x1000) != 0;
        if a12 != self.a12_state {
            if !a12 {
                self.a12lo_period = 0;
            } else {
                if self.a12lo_period >= 15 {
                    rom.clock_scanline_irq();
                }
            }
        } else if !a12 {
            self.a12lo_period += 2;
        }
        self.a12_state = a12;
    }

    pub fn write_to_ctrl(&mut self, value: u8) {
        let before_nmi_status = self.ctrl.contains(ControlRegister::GENERATE_NMI);
        self.ctrl.update(value);
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
        self.internal.reset_latch();
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
        self.internal.write_scroll(value);
    }
    pub fn write_to_ppu_addr(&mut self, _rom: &mut Rom, value: u8) {
        self.internal.write_addr(value);
    }

    pub fn read_data(&mut self, rom: &mut Rom) -> u8 {
        let addr = self.internal.get_v();
        self.internal.increment_v(self.ctrl.vram_addr_increment());
        match addr {
            0..=0x3EFF => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.read_vram(rom, addr);
                result
            }
            0x3F00..=0x3FFF => {
                self.internal_data_buf = self.read_vram(rom, addr - 0x1000);
                self.read_vram(rom, addr)
            }
            _ => 0,
        }
    }

    pub fn write_data(&mut self, rom: &mut Rom, val: u8) {
        let addr = self.internal.get_v();
        self.write_vram(rom, addr, val);
        self.internal.increment_v(self.ctrl.vram_addr_increment());
    }

    pub fn write_oam_dma(&mut self, data: &[u8; 256]) {
        for x in data.iter() {
            self.oam_data[self.oam_addr as usize] = *x;
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    fn read_vram(&mut self, rom: &mut Rom, addr: u16) -> u8 {
        let addr = addr & 0x3FFF;
        self.handle_a12_check(rom, addr);

        match addr {
            0x0000..=0x1FFF => rom.read_chr(addr),
            0x2000..=0x2FFF => {
                let mirrored_addr = self.mirror_vram_addr(addr, rom.mirroring());
                self.vram[mirrored_addr as usize]
            }
            0x3000..=0x3EFF => {
                let mirrored_addr = self.mirror_vram_addr(addr - 0x1000, rom.mirroring());
                self.vram[mirrored_addr as usize]
            }
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3F00) as usize]
            }
            0x3F00..=0x3FFF => self.palette_table[(addr - 0x3F00) as usize],
            _ => 0,
        }
    }

    fn write_vram(&mut self, rom: &mut Rom, addr: u16, val: u8) {
        let addr = addr & 0x3FFF;
        match addr {
            0x0000..=0x1FFF => {
                rom.write_chr(addr, val);
            }
            0x2000..=0x2FFF => {
                let mirrored_addr = self.mirror_vram_addr(addr, rom.mirroring());
                self.vram[mirrored_addr as usize] = val;
            }
            0x3000..=0x3EFF => {
                let mirrored_addr = self.mirror_vram_addr(addr - 0x1000, rom.mirroring());
                self.vram[mirrored_addr as usize] = val;
            }
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3F00) as usize] = val;
            }
            0x3F00..=0x3FFF => {
                self.palette_table[(addr - 0x3F00) as usize] = val;
            }
            _ => {}
        }
    }

    fn mirror_vram_addr(&self, addr: u16, mirroring: Mirroring) -> u16 {
        let vram_index = (addr & 0x2FFF) - 0x2000;
        let nametable = vram_index / 0x0400;

        match mirroring {
            Mirroring::Horizontal => match nametable {
                0 | 1 => vram_index & 0x03FF,
                2 | 3 => (vram_index & 0x03FF) + 0x0400,
                _ => unreachable!(),
            },
            Mirroring::Vertical => match nametable {
                0 | 2 => vram_index & 0x03FF,
                1 | 3 => (vram_index & 0x03FF) + 0x0400,
                _ => unreachable!(),
            },
            Mirroring::SingleScreenLower => vram_index & 0x03FF,
            Mirroring::SingleScreenUpper => (vram_index & 0x03FF) + 0x0400,
            Mirroring::FourScreen => vram_index,
        }
    }

    fn peek_vram(&self, rom: &Rom, addr: u16) -> u8 {
        let addr = addr & 0x3FFF;
        match addr {
            0x0000..=0x1FFF => rom.read_chr(addr),
            0x2000..=0x2FFF => {
                let mirrored_addr = self.mirror_vram_addr(addr, rom.mirroring());
                self.vram[mirrored_addr as usize]
            }
            0x3000..=0x3EFF => {
                let mirrored_addr = self.mirror_vram_addr(addr - 0x1000, rom.mirroring());
                self.vram[mirrored_addr as usize]
            }
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3F00) as usize]
            }
            0x3F00..=0x3FFF => self.palette_table[(addr - 0x3F00) as usize],
            _ => 0,
        }
    }
}
