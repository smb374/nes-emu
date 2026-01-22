#![allow(dead_code)]

use bitflags::bitflags;

// 1st write  2nd write
// 15 bit  8  7  bit  0
// ---- ----  ---- ----
// ..AA AAAA  AAAA AAAA
//   || ||||  |||| ||||
//   ++-++++--++++-++++- VRAM address
pub struct AddrRegister {
    value: (u8, u8),
    latch: bool,
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister {
            value: (0, 0), // high byte first, lo byte second
            latch: true,
        }
    }

    fn set(&mut self, data: u16) {
        self.value.0 = (data >> 8) as u8;
        self.value.1 = (data & 0xff) as u8;
    }

    pub fn update(&mut self, data: u8) {
        if self.latch {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }

        if self.get() > 0x3fff {
            //mirror down addr above 0x3fff
            self.set(self.get() & 0b11111111111111);
        }

        self.latch = !self.latch;
    }

    pub fn increment(&mut self, inc: u8) {
        let lo = self.value.1;
        self.value.1 = self.value.1.wrapping_add(inc);
        if lo > self.value.1 {
            self.value.0 = self.value.0.wrapping_add(1);
        }
        if self.get() > 0x3fff {
            self.set(self.get() & 0b11111111111111); //mirror down addr above 0x3fff
        }
    }

    pub fn reset_latch(&mut self) {
        self.latch = true;
    }

    pub fn get(&self) -> u16 {
        ((self.value.0 as u16) << 8) | (self.value.1 as u16)
    }
}

bitflags! {
    // 7  bit  0
    // ---- ----
    // VPHB SINN
    // |||| ||||
    // |||| ||++- Base nametable address
    // |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    // |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    // |||| |     (0: add 1, going across; 1: add 32, going down)
    // |||| +---- Sprite pattern table address for 8x8 sprites
    // ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    // |||+------ Background pattern table address (0: $0000; 1: $1000)
    // ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
    // |+-------- PPU master/slave select
    // |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    // +--------- Generate an NMI at the start of the
    //            vertical blanking interval (0: off; 1: on)
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct ControlRegister: u8 {
        const NAMETABLE1              = 0b00000001;
        const NAMETABLE2              = 0b00000010;
        const VRAM_ADD_INCREMENT      = 0b00000100;
        const SPRITE_PATTERN_ADDR     = 0b00001000;
        const BACKROUND_PATTERN_ADDR  = 0b00010000;
        const SPRITE_SIZE             = 0b00100000;
        const MASTER_SLAVE_SELECT     = 0b01000000;
        const GENERATE_NMI            = 0b10000000;
    }
}

impl ControlRegister {
    pub fn new() -> Self {
        Self::from_bits_truncate(0b00000000)
    }

    pub fn nametable_addr(&self) -> u16 {
        match self.bits() & 0b11 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2c00,
            _ => panic!("not possible"),
        }
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if !self.contains(Self::VRAM_ADD_INCREMENT) {
            1
        } else {
            32
        }
    }

    pub fn sprt_pattern_addr(&self) -> u16 {
        if !self.contains(Self::SPRITE_PATTERN_ADDR) {
            0
        } else {
            0x1000
        }
    }

    pub fn bknd_pattern_addr(&self) -> u16 {
        if !self.contains(Self::BACKROUND_PATTERN_ADDR) {
            0
        } else {
            0x1000
        }
    }

    pub fn sprite_size(&self) -> u8 {
        if !self.contains(Self::SPRITE_SIZE) {
            8
        } else {
            16
        }
    }

    pub fn master_slave_select(&self) -> u8 {
        if !self.contains(Self::SPRITE_SIZE) {
            0
        } else {
            1
        }
    }

    pub fn update(&mut self, data: u8) {
        *self = Self::from_bits_retain(data);
    }
}

bitflags! {
    // 7  bit  0
    // ---- ----
    // BGRs bMmG
    // |||| ||||
    // |||| |||+- Greyscale (0: normal color, 1: greyscale)
    // |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    // |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    // |||| +---- 1: Enable background rendering
    // |||+------ 1: Enable sprite rendering
    // ||+------- Emphasize red (green on PAL/Dendy)
    // |+-------- Emphasize green (red on PAL/Dendy)
    // +--------- Emphasize blue
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct MaskRegister: u8 {
        const GREYSCALE         = 0b00000001;
        const LEFTMOST_8PXL_BG  = 0b00000010;
        const LEFTMOST_8PXL_SP  = 0b00000100;
        const SHOW_BACKGROUND   = 0b00001000;
        const SHOW_SPRITE       = 0b00010000;
        const EMPHASISE_RED     = 0b00100000;
        const EMPHASISE_GREEN   = 0b01000000;
        const EMPHASISE_BLUE    = 0b10000000;
    }
}

impl MaskRegister {
    pub fn new() -> Self {
        Self::from_bits_truncate(0b00000000)
    }

    pub fn show_background(&self) -> bool {
        self.contains(Self::SHOW_BACKGROUND)
    }

    pub fn show_sprites(&self) -> bool {
        self.contains(Self::SHOW_SPRITE)
    }

    pub fn emphasise(&self) -> (bool, bool, bool) {
        (
            self.contains(Self::EMPHASISE_RED),
            self.contains(Self::EMPHASISE_GREEN),
            self.contains(Self::EMPHASISE_BLUE),
        )
    }

    pub fn update(&mut self, data: u8) {
        *self = Self::from_bits_retain(data);
    }
}

bitflags! {
    // 7  bit  0
    // ---- ----
    // VSOx xxxx
    // |||| ||||
    // |||+-++++- (PPU open bus or 2C05 PPU identifier)
    // ||+------- Sprite overflow flag
    // |+-------- Sprite 0 hit flag
    // +--------- Vblank flag, cleared on read. Unreliable; see below.
    pub struct StatusRegister: u8 {
        const SPRITE_OVERFLOW = 0b00100000;
        const SPRITE_ZERO_HIT = 0b01000000;
        const VBLANK_STARTED  = 0b10000000;

        const _ = !0;
    }
}

impl StatusRegister {
    pub fn new() -> Self {
        Self::from_bits_truncate(0b00000000)
    }
}

// 1st write
// 7  bit  0
// ---- ----
// XXXX XXXX
// |||| ||||
// ++++-++++- X scroll bits 7-0 (bit 8 in PPUCTRL bit 0)
//
// 2nd write
// 7  bit  0
// ---- ----
// YYYY YYYY
// |||| ||||
// ++++-++++- Y scroll bits 7-0 (bit 8 in PPUCTRL bit 1)
pub struct ScrollRegister {
    pub scroll_x: u8,
    pub scroll_y: u8,
    pub latch: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            scroll_x: 0,
            scroll_y: 0,
            latch: false,
        }
    }

    pub fn write(&mut self, data: u8) {
        if !self.latch {
            self.scroll_x = data;
        } else {
            self.scroll_y = data;
        }
        self.latch = !self.latch;
    }

    pub fn reset_latch(&mut self) {
        self.latch = false;
    }
}
