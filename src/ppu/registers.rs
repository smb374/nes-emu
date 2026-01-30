#![allow(dead_code)]

use bitflags::bitflags;

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

    pub fn show_leftmost_background(&self) -> bool {
        self.contains(Self::LEFTMOST_8PXL_BG)
    }

    pub fn show_leftmost_sprites(&self) -> bool {
        self.contains(Self::LEFTMOST_8PXL_SP)
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

// PPU internal registers
// The PPU has internal registers that are shared between PPUSCROLL and PPUADDR writes:
//
// v: Current VRAM address (15 bits)
// t: Temporary VRAM address (15 bits); can be thought of as the address of the top left onscreen tile
// x: Fine X scroll (3 bits)
// w: Write latch (1 bit); determines whether the next write is the first or second write
//
// Register format:
// yyy NN YYYYY XXXXX
// ||| || ||||| +++++-- coarse X scroll (tile column: 0-31)
// ||| || +++++-------- coarse Y scroll (tile row: 0-29)
// ||| ++-------------- nametable select (0-3)
// +++----------------- fine Y scroll (pixel row within tile: 0-7)
pub struct InternalRegisters {
    v: u16,     // Current VRAM address
    t: u16,     // Temporary VRAM address
    fine_x: u8, // Fine X scroll (3 bits)
    w: bool,    // Write latch
}

impl InternalRegisters {
    pub fn new() -> Self {
        InternalRegisters {
            v: 0,
            t: 0,
            fine_x: 0,
            w: false,
        }
    }

    // PPUSCROLL write
    pub fn write_scroll(&mut self, data: u8) {
        if !self.w {
            // First write: X scroll
            // t: ....... ...XXXXX <- data[7:3]
            // x:              XXX <- data[2:0]
            self.t = (self.t & 0xFFE0) | ((data as u16) >> 3);
            self.fine_x = data & 0x07;
        } else {
            // Second write: Y scroll
            // t: XXX..YY YYY..... <- data[7:0]
            self.t = (self.t & 0x8FFF) | (((data as u16) & 0x07) << 12); // Fine Y
            self.t = (self.t & 0xFC1F) | (((data as u16) & 0xF8) << 2); // Coarse Y
        }
        self.w = !self.w;
    }

    // PPUADDR write
    pub fn write_addr(&mut self, data: u8) {
        if !self.w {
            // First write: high byte
            // t: .CDEFGH ........ <- data[5:0]
            // t: X...... ........ <- 0
            self.t = (self.t & 0x00FF) | (((data as u16) & 0x3F) << 8);
        } else {
            // Second write: low byte
            // t: ....... ABCDEFGH <- data[7:0]
            // v: <...t...>        <- t
            self.t = (self.t & 0xFF00) | (data as u16);
            self.v = self.t;
        }
        self.w = !self.w;
    }

    // Reset write latch (on PPUSTATUS read)
    pub fn reset_latch(&mut self) {
        self.w = false;
    }

    // Get current VRAM address
    pub fn get_v(&self) -> u16 {
        self.v & 0x3FFF
    }

    // Set VRAM address directly (for increment)
    pub fn set_v(&mut self, addr: u16) {
        self.v = addr & 0x3FFF;
    }

    // Increment VRAM address
    pub fn increment_v(&mut self, inc: u8) {
        self.v = self.v.wrapping_add(inc as u16) & 0x3FFF;
    }

    // Update nametable bits from PPUCTRL
    pub fn update_nametable_from_ctrl(&mut self, ctrl_bits: u8) {
        // t: ....BA.. ........ <- ctrl[1:0]
        self.t = (self.t & 0xF3FF) | (((ctrl_bits as u16) & 0x03) << 10);
    }

    // Coarse X increment (during rendering)
    pub fn increment_x(&mut self) {
        if (self.v & 0x001F) == 31 {
            // Wrap coarse X and switch horizontal nametable
            self.v &= !0x001F;
            self.v ^= 0x0400;
        } else {
            self.v += 1;
        }
    }

    // Y increment (at end of scanline during rendering)
    pub fn increment_y(&mut self) {
        if (self.v & 0x7000) != 0x7000 {
            // Increment fine Y
            self.v += 0x1000;
        } else {
            // Fine Y overflow
            self.v &= !0x7000;
            let mut y = (self.v & 0x03E0) >> 5;
            if y == 29 {
                y = 0;
                self.v ^= 0x0800; // Switch vertical nametable
            } else if y == 31 {
                y = 0;
            } else {
                y += 1;
            }
            self.v = (self.v & !0x03E0) | (y << 5);
        }
    }

    // Copy horizontal bits from t to v
    pub fn copy_horizontal(&mut self) {
        // v: .....N.. ...XXXXX <- t: .....N.. ...XXXXX
        self.v = (self.v & 0xFBE0) | (self.t & 0x041F);
    }

    // Copy vertical bits from t to v
    pub fn copy_vertical(&mut self) {
        // v: .YYYNN.Y YYY..... <- t: .YYYNN.Y YYY.....
        self.v = (self.v & 0x841F) | (self.t & 0x7BE0);
    }

    // Extract components for rendering
    pub fn coarse_x(&self) -> u8 {
        (self.v & 0x001F) as u8
    }

    pub fn coarse_y(&self) -> u8 {
        ((self.v & 0x03E0) >> 5) as u8
    }

    pub fn fine_x(&self) -> u8 {
        self.fine_x
    }

    pub fn fine_y(&self) -> u8 {
        ((self.v & 0x7000) >> 12) as u8
    }

    pub fn nametable(&self) -> u8 {
        ((self.v & 0x0C00) >> 10) as u8
    }

    pub fn nametable_addr(&self) -> u16 {
        0x2000 | (self.v & 0x0FFF)
    }
}
