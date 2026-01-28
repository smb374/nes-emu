mod registers;

use std::{cell::RefCell, rc::Rc};

use crate::{
    cartridge::Rom,
    mapper::Mirroring,
    ppu::registers::{AddrRegister, ControlRegister, MaskRegister, ScrollRegister, StatusRegister},
};

pub struct PPU {
    pub rom: Rc<RefCell<Rom>>,
    pub ctrl: ControlRegister,
    pub mask: MaskRegister,
    pub status: StatusRegister,
    pub scroll: ScrollRegister,
    pub addr: AddrRegister,
    pub vram: [u8; 0x800],

    pub oam_addr: u8,
    pub oam_data: [u8; 0x100],
    pub palette_table: [u8; 0x20],
    internal_data_buf: u8,

    pub nmi_interrupt: Option<u8>,
    scanline: u16,
    cycles: usize,
}

impl PPU {
    pub fn new(rom: Rc<RefCell<Rom>>) -> Self {
        Self {
            rom,
            ctrl: ControlRegister::new(),
            mask: MaskRegister::new(),
            status: StatusRegister::new(),
            scroll: ScrollRegister::new(),
            addr: AddrRegister::new(),
            vram: [0; 0x800],
            oam_addr: 0,
            oam_data: [0xFF; 0x100],
            palette_table: [0; 0x20],
            internal_data_buf: 0,
            nmi_interrupt: None,
            scanline: 0,
            cycles: 0,
        }
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }

    pub fn tick(&mut self, cycles: u16) -> bool {
        self.cycles += cycles as usize;
        if self.cycles >= 341 {
            if self.is_sprite_0_hit(self.cycles) {
                self.status.set(StatusRegister::SPRITE_ZERO_HIT, true);
            }

            self.cycles = self.cycles - 341;
            self.scanline += 1;

            if self.scanline == 241 {
                self.status.set(StatusRegister::VBLANK_STARTED, true);
                self.status.set(StatusRegister::SPRITE_ZERO_HIT, false);
                if self.ctrl.contains(ControlRegister::GENERATE_NMI) {
                    self.nmi_interrupt = Some(1);
                }
            }

            if self.scanline >= 262 {
                self.scanline = 0;
                self.nmi_interrupt = None;
                self.status.set(StatusRegister::SPRITE_ZERO_HIT, false);
                self.status.remove(StatusRegister::VBLANK_STARTED);
                return true;
            }
        }
        return false;
    }

    fn is_sprite_0_hit(&self, cycle: usize) -> bool {
        let y = self.oam_data[0] as usize;
        let x = self.oam_data[3] as usize;
        (y == self.scanline as usize) && x <= cycle && self.mask.show_sprites()
    }

    pub fn poll_nmi_interrupt(&mut self) -> Option<u8> {
        self.nmi_interrupt.take()
    }

    pub fn write_to_ctrl(&mut self, value: u8) {
        let before_nmi_status = self.ctrl.contains(ControlRegister::GENERATE_NMI);
        self.ctrl.update(value);
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
        self.addr.reset_latch();
        self.scroll.reset_latch();
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
        self.scroll.write(value);
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
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
        let addr = self.addr.get();
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
        self.addr.get()
    }

    pub fn mirroring(&self) -> Mirroring {
        self.rom.borrow().mirroring()
    }
}
