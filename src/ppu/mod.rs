mod palette;
pub mod registers;

use crate::{
    cartridge::Rom,
    mapper::Mirroring,
    ppu::registers::{ControlRegister, InternalRegisters, MaskRegister, StatusRegister},
};

// Secondary OAM entry for sprite evaluation
#[derive(Clone, Copy, Default)]
struct SecondaryOamEntry {
    y: u8,
    tile: u8,
    attr: u8,
    x: u8,
    sprite_index: u8, // Original OAM index (for sprite 0 detection)
}

// Sprite rendering data after pattern fetch
#[derive(Clone, Copy, Default)]
struct SpriteRenderData {
    pattern_lo: u8,
    pattern_hi: u8,
    attr: u8,
    x_counter: u8,
    sprite_index: u8,
}

#[derive(Clone, Copy, Default)]
struct SpriteEvalState {
    n: u8,         // Primary OAM index (0-63)
    m: u8,         // Byte within sprite (0-3)
    phase: u8,     // 0=clear secondary, 1=evaluation, 2=done
    latch: u8,     // Temporary latch for sprite Y
    write_idx: u8, // Write index into secondary OAM
}

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
    pub scanline: u16,
    pub cycles: usize,
    pub frames: usize,

    pub frame_buffer: [u8; 256 * 240 * 3],

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

    // Secondary OAM (8 sprites for next scanline)
    secondary_oam: [SecondaryOamEntry; 8],
    secondary_oam_count: u8,

    // Sprite evaluation state
    sprite_eval: SpriteEvalState,

    // Sprite render data (after fetches, ready for rendering)
    sprite_render: [SpriteRenderData; 8],
    sprite_render_count: u8,

    // A12 IRQ state
    a12_state: bool,
    a12lo_period: usize,

    // Odd frame state
    odd_frame: bool,

    suppress_vbl: bool,
    start_nmi: bool,
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
            frames: 0,
            internal: InternalRegisters::new(),
            frame_buffer: [0; 256 * 240 * 3],

            bg_pattern_lo: 0,
            bg_pattern_hi: 0,
            bg_attrib_lo: 0,
            bg_attrib_hi: 0,
            bg_nt_id: 0,
            bg_nt_attrib: 0,
            bg_nt_lsb: 0,
            bg_nt_msb: 0,

            secondary_oam: [SecondaryOamEntry::default(); 8],
            secondary_oam_count: 0,
            sprite_eval: SpriteEvalState::default(),

            sprite_render: [SpriteRenderData::default(); 8],
            sprite_render_count: 0,

            a12_state: false,
            a12lo_period: 0,

            odd_frame: false,
            suppress_vbl: false,
            start_nmi: false,
        }
    }

    pub fn tick(&mut self, rom: &mut Rom, cycles: u16) {
        for _ in 0..cycles {
            let rendering_enabled = self.mask.show_background() || self.mask.show_sprites();

            self.advance_cycle(rendering_enabled);
            match self.scanline {
                // Visible scanlines (0-239)
                0..=239 => self.tick_visible_scanline(rom, rendering_enabled),

                // Post-render scanline (240) - idle
                240 => {}

                // VBlank scanlines (241-260)
                241 => match self.cycles {
                    1 => {
                        if !self.suppress_vbl {
                            self.status.set(StatusRegister::VBLANK_STARTED, true);
                            self.start_nmi = self.ctrl.contains(ControlRegister::GENERATE_NMI);
                        }
                        self.suppress_vbl = false;
                    }
                    2 if self.start_nmi => {
                        self.nmi_interrupt = Some(1);
                        self.start_nmi = false;
                        log::info!(
                            "{: >3},{: >3}: Fire NMI @ frame {}",
                            self.scanline,
                            self.cycles,
                            self.frames
                        );
                    }
                    _ => {}
                },

                // Pre-render scanline (261)
                261 => self.tick_prerender_scanline(rom, rendering_enabled),

                _ => {}
            }
        }
    }

    fn tick_prerender_scanline(&mut self, rom: &mut Rom, rendering_enabled: bool) {
        // Cycle 1: Clear status flags
        if self.cycles == 1 {
            self.status.remove(StatusRegister::VBLANK_STARTED);
            self.status.remove(StatusRegister::SPRITE_ZERO_HIT);
            self.status.remove(StatusRegister::SPRITE_OVERFLOW);
        }

        if !rendering_enabled {
            return;
        }

        match self.cycles {
            // Cycle 0: Initialize sprite evaluation for scanline 0
            0 => {
                self.sprite_eval.n = 0;
                self.sprite_eval.m = 0;
                self.sprite_eval.phase = 0;
                self.sprite_eval.write_idx = 0;
                self.secondary_oam_count = 0;
                // Clear secondary OAM
                for i in 0..8 {
                    self.secondary_oam[i] = SecondaryOamEntry::default();
                    self.secondary_oam[i].y = 0xFF;
                    self.secondary_oam[i].tile = 0xFF;
                    self.secondary_oam[i].attr = 0xFF;
                    self.secondary_oam[i].x = 0xFF;
                }
            }

            // Cycles 1-256: BG fetches (no pixel output) + sprite evaluation for scanline 0
            1..=64 => {
                self.fetch_background(rom);
                self.update_a12(rom, self.ctrl.bknd_pattern_addr());
            }

            65..=256 => {
                self.fetch_background(rom);
                self.update_a12(rom, self.ctrl.bknd_pattern_addr());

                // Sprite evaluation for scanline 0
                self.tick_sprite_evaluation_prerender();

                if self.cycles == 251 {
                    self.internal.increment_y();
                }
            }

            // Cycle 257: Copy horizontal bits
            257 => {
                self.load_bg_shifters();
                self.internal.copy_horizontal();
                self.update_a12(rom, self.ctrl.sprt_pattern_addr());
            }

            // Cycles 258-320: Sprite pattern fetches for scanline 0
            258..=320 => {
                self.fetch_sprite_patterns_prerender(rom);
                self.update_a12(rom, self.ctrl.sprt_pattern_addr());

                // Cycles 280-304: Copy vertical bits repeatedly
                if (280..=304).contains(&self.cycles) {
                    self.internal.copy_vertical();
                }
            }

            // Cycles 321-336: Fetch first 2 BG tiles for scanline 0
            321..=336 => {
                self.fetch_background(rom);
                self.update_a12(rom, self.ctrl.bknd_pattern_addr());
            }

            // Cycles 337-340: Unused NT fetches
            337..=340 => {
                if (self.cycles - 337) % 2 == 0 {
                    let v = self.internal.get_v();
                    let addr = 0x2000 | (v & 0x0FFF);
                    self.peek_vram(rom, addr);
                    self.update_a12(rom, self.ctrl.bknd_pattern_addr());
                }
            }

            _ => {}
        }
    }

    fn tick_sprite_evaluation_prerender(&mut self) {
        // Sprite evaluation for scanline 0 (next scanline after pre-render)
        // Same logic as regular evaluation but checking for scanline 0

        if self.secondary_oam_count >= 8 && self.sprite_eval.n >= 64 {
            return;
        }

        let is_odd_cycle = self.cycles % 2 == 1;

        if is_odd_cycle {
            if self.sprite_eval.n < 64 {
                let oam_offset = (self.sprite_eval.n as usize) * 4 + (self.sprite_eval.m as usize);
                self.sprite_eval.latch = self.oam_data[oam_offset];
            }
        } else {
            if self.sprite_eval.n >= 64 {
                return;
            }

            if self.secondary_oam_count < 8 {
                if self.sprite_eval.m == 0 {
                    let sprite_y = self.sprite_eval.latch as u16;
                    let sprite_size = self.ctrl.sprite_size() as u16;
                    // For pre-render, next scanline is 0
                    let next_scanline: u16 = 0;

                    if next_scanline >= sprite_y.wrapping_add(1)
                        && next_scanline < sprite_y.wrapping_add(1).wrapping_add(sprite_size)
                    {
                        let idx = self.secondary_oam_count as usize;
                        self.secondary_oam[idx].y = self.sprite_eval.latch;
                        self.secondary_oam[idx].sprite_index = self.sprite_eval.n;
                        self.sprite_eval.m = 1;
                    } else {
                        self.sprite_eval.n += 1;
                    }
                } else {
                    let idx = self.secondary_oam_count as usize;
                    match self.sprite_eval.m {
                        1 => self.secondary_oam[idx].tile = self.sprite_eval.latch,
                        2 => self.secondary_oam[idx].attr = self.sprite_eval.latch,
                        3 => {
                            self.secondary_oam[idx].x = self.sprite_eval.latch;
                            self.secondary_oam_count += 1;
                            self.sprite_eval.n += 1;
                            self.sprite_eval.m = 0;
                            return;
                        }
                        _ => {}
                    }
                    self.sprite_eval.m += 1;
                }
            } else {
                // Check for sprite overflow (same buggy behavior)
                if self.sprite_eval.m == 0 {
                    let sprite_y = self.sprite_eval.latch as u16;
                    let sprite_size = self.ctrl.sprite_size() as u16;
                    let next_scanline: u16 = 0;

                    if next_scanline >= sprite_y.wrapping_add(1)
                        && next_scanline < sprite_y.wrapping_add(1).wrapping_add(sprite_size)
                    {
                        self.status.insert(StatusRegister::SPRITE_OVERFLOW);
                        self.sprite_eval.m = (self.sprite_eval.m + 1) & 3;
                    }
                    self.sprite_eval.n += 1;
                } else {
                    self.sprite_eval.m = (self.sprite_eval.m + 1) & 3;
                    if self.sprite_eval.m == 0 {
                        self.sprite_eval.n += 1;
                    }
                }
            }
        }
    }

    fn fetch_sprite_patterns_prerender(&mut self, rom: &mut Rom) {
        // Same as regular sprite pattern fetch but for scanline 0
        let sprite_cycle = self.cycles - 258;
        let sprite_idx = sprite_cycle / 8;
        let cycle_in_sprite = sprite_cycle % 8;

        if self.cycles == 258 {
            self.sprite_render_count = self.secondary_oam_count;
            for i in 0..8 {
                self.sprite_render[i] = SpriteRenderData::default();
            }
        }

        if sprite_idx < 8 {
            match cycle_in_sprite {
                0 | 2 => {
                    let v = self.internal.get_v();
                    let addr = 0x2000 | (v & 0x0FFF);
                    self.peek_vram(rom, addr);
                }
                4 => {
                    if (sprite_idx as u8) < self.secondary_oam_count {
                        let entry = &self.secondary_oam[sprite_idx];
                        let addr = self.calc_sprite_pattern_addr_for_scanline(entry, true, 0);
                        let pattern = rom.read_chr(addr);
                        self.sprite_render[sprite_idx].pattern_lo = pattern;
                        self.sprite_render[sprite_idx].attr = entry.attr;
                        self.sprite_render[sprite_idx].x_counter = entry.x;
                        self.sprite_render[sprite_idx].sprite_index = entry.sprite_index;
                    } else {
                        let addr = self.ctrl.sprt_pattern_addr() + 0xFF * 16;
                        let _ = rom.read_chr(addr);
                    }
                }
                6 => {
                    if (sprite_idx as u8) < self.secondary_oam_count {
                        let entry = &self.secondary_oam[sprite_idx];
                        let addr = self.calc_sprite_pattern_addr_for_scanline(entry, false, 0);
                        let pattern = rom.read_chr(addr);
                        self.sprite_render[sprite_idx].pattern_hi = pattern;
                    } else {
                        let addr = self.ctrl.sprt_pattern_addr() + 0xFF * 16 + 8;
                        let _ = rom.read_chr(addr);
                    }
                }
                _ => {}
            }
        }
    }

    fn calc_sprite_pattern_addr_for_scanline(
        &self,
        entry: &SecondaryOamEntry,
        is_low: bool,
        target_scanline: u16,
    ) -> u16 {
        let sprite_size = self.ctrl.sprite_size();
        let tile_num = entry.tile;
        let attr = entry.attr;
        let flip_v = (attr >> 7) & 1 == 1;

        let sprite_y = entry.y as u16;
        let mut y_offset = target_scanline.wrapping_sub(sprite_y.wrapping_add(1));

        if flip_v {
            y_offset = sprite_size as u16 - 1 - y_offset;
        }

        let tile_addr = if sprite_size == 16 {
            let bank = (tile_num as u16 & 0x01) * 0x1000;
            let tile_index = (tile_num & 0xFE) as u16 + (y_offset / 8);
            bank + (tile_index * 16) + (y_offset % 8)
        } else {
            let bank = self.ctrl.sprt_pattern_addr();
            bank + (tile_num as u16 * 16) + y_offset
        };

        if is_low { tile_addr } else { tile_addr + 8 }
    }

    fn tick_visible_scanline(&mut self, rom: &mut Rom, rendering_enabled: bool) {
        if !rendering_enabled {
            return;
        }

        match self.cycles {
            // Cycle 0: Idle, but initialize sprite evaluation
            0 => {
                // Initialize sprite evaluation state for this scanline
                self.sprite_eval.n = 0;
                self.sprite_eval.m = 0;
                self.sprite_eval.phase = 0; // Start with secondary OAM clear
                self.sprite_eval.write_idx = 0;
                self.secondary_oam_count = 0;
            }

            // Cycles 1-64: Clear secondary OAM (writes 0xFF)
            1..=64 => {
                self.fetch_background(rom);
                self.update_a12(rom, self.ctrl.bknd_pattern_addr());

                // Clear secondary OAM: on odd cycles read, on even cycles write
                if self.cycles % 2 == 0 {
                    let idx = ((self.cycles - 1) / 2) as usize;
                    if idx < 32 {
                        // 8 sprites × 4 bytes = 32 bytes
                        let sprite_idx = idx / 4;
                        let byte_idx = idx % 4;
                        match byte_idx {
                            0 => self.secondary_oam[sprite_idx].y = 0xFF,
                            1 => self.secondary_oam[sprite_idx].tile = 0xFF,
                            2 => self.secondary_oam[sprite_idx].attr = 0xFF,
                            3 => self.secondary_oam[sprite_idx].x = 0xFF,
                            _ => {}
                        }
                    }
                }

                // Draw pixel for cycles 1-256
                self.draw_pixel_cycle((self.cycles - 1) as usize, rom);
            }

            // Cycles 65-256: Sprite evaluation + BG fetch + pixel output
            65..=256 => {
                self.fetch_background(rom);
                self.update_a12(rom, self.ctrl.bknd_pattern_addr());

                // Sprite evaluation happens on odd cycles (read) and even cycles (write)
                self.tick_sprite_evaluation();

                if self.cycles == 251 {
                    self.internal.increment_y();
                }

                // Draw pixel
                self.draw_pixel_cycle((self.cycles - 1) as usize, rom);
            }

            // Cycle 257: Copy horizontal bits + start sprite fetches
            257 => {
                self.load_bg_shifters();
                self.internal.copy_horizontal();
                self.update_a12(rom, self.ctrl.sprt_pattern_addr());
            }

            // Cycles 258-320: Fetch sprite patterns for next scanline
            // Each sprite takes 8 cycles: garbage NT, garbage NT, pattern lo, pattern hi
            258..=320 => {
                self.fetch_sprite_patterns(rom);
                self.update_a12(rom, self.ctrl.sprt_pattern_addr());
            }

            // Cycles 321-336: Fetch first 2 BG tiles for next scanline
            321..=336 => {
                self.fetch_background(rom);
                self.update_a12(rom, self.ctrl.bknd_pattern_addr());
            }

            // Cycles 337-340: Unused NT fetches
            337..=340 => {
                if (self.cycles - 337) % 2 == 0 {
                    let v = self.internal.get_v();
                    let addr = 0x2000 | (v & 0x0FFF);
                    self.peek_vram(rom, addr);
                    self.update_a12(rom, self.ctrl.bknd_pattern_addr());
                }
            }

            _ => {}
        }
    }

    fn tick_sprite_evaluation(&mut self) {
        // Sprite evaluation occurs during cycles 65-256
        // On odd cycles: read from primary OAM
        // On even cycles: write to secondary OAM (if applicable)

        if self.secondary_oam_count >= 8 && self.sprite_eval.n >= 64 {
            // Done with evaluation
            return;
        }

        let is_odd_cycle = self.cycles % 2 == 1;

        if is_odd_cycle {
            // Read from primary OAM
            if self.sprite_eval.n < 64 {
                let oam_offset = (self.sprite_eval.n as usize) * 4 + (self.sprite_eval.m as usize);
                self.sprite_eval.latch = self.oam_data[oam_offset];
            }
        } else {
            // Even cycle: potentially write to secondary OAM
            if self.sprite_eval.n >= 64 {
                return;
            }

            if self.secondary_oam_count < 8 {
                // Normal evaluation
                if self.sprite_eval.m == 0 {
                    // Check if sprite is on next scanline
                    let sprite_y = (self.sprite_eval.latch as u16).wrapping_add(1);
                    let sprite_size = self.ctrl.sprite_size() as u16;
                    let next_scanline = self.scanline + 1;

                    // Sprite Y is the line BEFORE the sprite appears
                    // So sprite is visible if: Y+1 <= scanline < Y+1+height
                    // For evaluation, we check for the NEXT scanline
                    if next_scanline >= sprite_y
                        && next_scanline < sprite_y.wrapping_add(sprite_size)
                    {
                        // Sprite is in range, start copying
                        let idx = self.secondary_oam_count as usize;
                        self.secondary_oam[idx].y = self.sprite_eval.latch;
                        self.secondary_oam[idx].sprite_index = self.sprite_eval.n;
                        self.sprite_eval.m = 1;
                    } else {
                        // Sprite not in range, move to next sprite
                        self.sprite_eval.n += 1;
                    }
                } else {
                    // Copy remaining bytes
                    let idx = self.secondary_oam_count as usize;
                    match self.sprite_eval.m {
                        1 => self.secondary_oam[idx].tile = self.sprite_eval.latch,
                        2 => self.secondary_oam[idx].attr = self.sprite_eval.latch,
                        3 => {
                            self.secondary_oam[idx].x = self.sprite_eval.latch;
                            self.secondary_oam_count += 1;
                            self.sprite_eval.n += 1;
                            self.sprite_eval.m = 0;
                            return;
                        }
                        _ => {}
                    }
                    self.sprite_eval.m += 1;
                }
            } else {
                // Secondary OAM is full, check for sprite overflow
                // This is the buggy behavior where m is incorrectly incremented
                if self.sprite_eval.m == 0 {
                    let sprite_y = (self.sprite_eval.latch as u16).wrapping_add(1);
                    let sprite_size = self.ctrl.sprite_size() as u16;
                    let next_scanline = self.scanline + 1;

                    if next_scanline >= sprite_y
                        && next_scanline < sprite_y.wrapping_add(sprite_size)
                    {
                        // Sprite overflow detected
                        self.status.insert(StatusRegister::SPRITE_OVERFLOW);
                        // The real PPU has buggy behavior here where it increments both n and m
                        self.sprite_eval.m = (self.sprite_eval.m + 1) & 3;
                    }
                    self.sprite_eval.n += 1;
                } else {
                    // Bug: increment m even when checking wrong byte
                    self.sprite_eval.m = (self.sprite_eval.m + 1) & 3;
                    if self.sprite_eval.m == 0 {
                        self.sprite_eval.n += 1;
                    }
                }
            }
        }
    }

    fn fetch_sprite_patterns(&mut self, rom: &mut Rom) {
        // Cycles 258-320: 64 cycles for 8 sprites = 8 cycles each
        // Cycle pattern within each sprite's 8 cycles:
        // 0-1: Garbage NT fetch
        // 2-3: Garbage NT fetch
        // 4-5: Pattern table low
        // 6-7: Pattern table high

        let sprite_cycle = self.cycles - 258;
        let sprite_idx = sprite_cycle / 8;
        let cycle_in_sprite = sprite_cycle % 8;

        // At cycle 0 of first sprite, copy secondary OAM data to render state
        if self.cycles == 258 {
            self.sprite_render_count = self.secondary_oam_count;
            for i in 0..8 {
                self.sprite_render[i] = SpriteRenderData::default();
            }
        }

        // Pattern fetches happen at cycles 4 and 6 within each sprite's window
        if sprite_idx < 8 {
            let next_scanline = self.scanline + 1;
            match cycle_in_sprite {
                0 | 2 => {
                    // Garbage NT fetches - these still access the bus
                    let v = self.internal.get_v();
                    let addr = 0x2000 | (v & 0x0FFF);
                    self.peek_vram(rom, addr);
                }
                4 => {
                    // Fetch pattern low
                    if (sprite_idx as u8) < self.secondary_oam_count {
                        let entry = &self.secondary_oam[sprite_idx];
                        let addr =
                            self.calc_sprite_pattern_addr_for_scanline(entry, true, next_scanline);
                        let pattern = rom.read_chr(addr);
                        self.sprite_render[sprite_idx].pattern_lo = pattern;
                        self.sprite_render[sprite_idx].attr = entry.attr;
                        self.sprite_render[sprite_idx].x_counter = entry.x;
                        self.sprite_render[sprite_idx].sprite_index = entry.sprite_index;
                    } else {
                        // Dummy fetch for tile 0xFF
                        let addr = self.ctrl.sprt_pattern_addr() + 0xFF * 16;
                        let _ = rom.read_chr(addr);
                    }
                }
                6 => {
                    // Fetch pattern high
                    if (sprite_idx as u8) < self.secondary_oam_count {
                        let entry = &self.secondary_oam[sprite_idx];
                        let addr =
                            self.calc_sprite_pattern_addr_for_scanline(entry, false, next_scanline);
                        let pattern = rom.read_chr(addr);
                        self.sprite_render[sprite_idx].pattern_hi = pattern;
                    } else {
                        // Dummy fetch
                        let addr = self.ctrl.sprt_pattern_addr() + 0xFF * 16 + 8;
                        let _ = rom.read_chr(addr);
                    }
                }
                _ => {}
            }
        }
    }

    fn advance_cycle(&mut self, rendering_enabled: bool) {
        self.cycles += 1;

        // Odd frame skip: on pre-render scanline, skip from cycle 339 to 0
        if self.scanline == 261
            && self.cycles == 339
            && self.odd_frame
            && rendering_enabled
            && self.mask.show_background()
        {
            self.cycles = 340;
        }

        if self.cycles >= 341 {
            self.cycles = 0;
            self.scanline += 1;

            if self.scanline >= 262 {
                self.scanline = 0;
                self.nmi_interrupt = None;
                self.odd_frame = !self.odd_frame;
                self.frames += 1;
            }
        }
    }

    fn fetch_background(&mut self, rom: &mut Rom) {
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
                let addr = 0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07);
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
                if self.mask.show_background() || self.mask.show_sprites() {
                    self.internal.increment_x();
                }
            }
            _ => {}
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

        // Left-most 8px masking for background
        if x < 8 && !self.mask.show_leftmost_background() {
            bg_pixel = 0;
        }

        // 2. Resolve Sprite Pixel from render data
        let (sprite_pixel, sprite_palette, sprite_priority, sprite_is_zero) =
            self.get_sprite_pixel(x);

        // Left-most 8px masking for sprites
        let sprite_visible = if x < 8 && !self.mask.show_leftmost_sprites() {
            false
        } else {
            sprite_pixel != 0 && self.mask.show_sprites()
        };

        // 3. Sprite 0 Hit detection
        if sprite_is_zero
            && bg_pixel != 0
            && sprite_pixel != 0
            && self.mask.show_background()
            && self.mask.show_sprites()
        {
            // Check left masking - sprite 0 hit doesn't occur in leftmost 8 pixels if clipping
            let left_clip =
                !self.mask.show_leftmost_background() || !self.mask.show_leftmost_sprites();
            if !(x < 8 && left_clip) && x != 255 {
                self.status.set(StatusRegister::SPRITE_ZERO_HIT, true);
            }
        }

        // 4. Priority Mux
        let (final_pixel, final_palette, is_sprite) = if !sprite_visible {
            (bg_pixel, bg_palette, false)
        } else if bg_pixel == 0 {
            (sprite_pixel, sprite_palette, true)
        } else if sprite_priority {
            // Sprite behind background
            (bg_pixel, bg_palette, false)
        } else {
            // Sprite in front of background
            (sprite_pixel, sprite_palette, true)
        };

        // 5. Write to Framebuffer
        let final_color_idx = if final_pixel == 0 {
            self.read_palette(0)
        } else {
            let base = if is_sprite { 0x10 } else { 0x00 };
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

    fn get_sprite_pixel(&self, x: usize) -> (u8, u8, bool, bool) {
        // Check all sprites in priority order (lower index = higher priority)
        for i in 0..self.sprite_render_count as usize {
            let sprite = &self.sprite_render[i];
            let sprite_x = sprite.x_counter as usize;

            // Check if pixel is within sprite's horizontal range
            if x >= sprite_x && x < sprite_x + 8 {
                let pixel_offset = x - sprite_x;

                // Check horizontal flip
                let flip_h = (sprite.attr >> 6) & 1 == 1;
                let bit = if flip_h {
                    pixel_offset
                } else {
                    7 - pixel_offset
                };

                let lo = (sprite.pattern_lo >> bit) & 1;
                let hi = (sprite.pattern_hi >> bit) & 1;
                let color = (hi << 1) | lo;

                if color != 0 {
                    let palette = sprite.attr & 0x03;
                    let priority = (sprite.attr >> 5) & 1 == 1; // 1 = behind BG
                    let is_sprite_zero = sprite.sprite_index == 0;

                    return (color, palette, priority, is_sprite_zero);
                }
            }
        }

        (0, 0, false, false)
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

    pub fn update_a12(&mut self, rom: &mut Rom, addr: u16) {
        let a12 = (addr & 0x1000) != 0;
        if a12 != self.a12_state {
            if !a12 {
                self.a12lo_period = 0;
            } else {
                if self.a12lo_period >= 9 {
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

        let after_nmi_status = self.ctrl.contains(ControlRegister::GENERATE_NMI);

        log::info!(
            "{: >3},{: >3}: GENERATE_NMI {} -> {} @ frame {}",
            self.scanline,
            self.cycles,
            before_nmi_status,
            after_nmi_status,
            self.frames
        );
        if !before_nmi_status
            && after_nmi_status
            && self.status.contains(StatusRegister::VBLANK_STARTED)
        {
            if self.scanline == 241 && self.cycles == 1 {
                self.start_nmi = true;
            } else {
                self.nmi_interrupt = Some(2);
                log::info!(
                    "{: >3},{: >3}: IMM NMI @ frame {}",
                    self.scanline,
                    self.cycles,
                    self.frames
                );
            }
        }
    }

    pub fn write_to_mask(&mut self, value: u8) {
        self.mask.update(value);
    }

    pub fn read_status(&mut self) -> u8 {
        match (self.scanline, self.cycles) {
            (241, 0) => {
                // 1 dot before VBLANK_START
                self.status.remove(StatusRegister::VBLANK_STARTED);
                self.suppress_vbl = true;
                log::info!(
                    "{: >3},{: >3}: Suppress VBL @ frame {}",
                    self.scanline,
                    self.cycles,
                    self.frames
                );
            }
            (241, 1) if self.ctrl.contains(ControlRegister::GENERATE_NMI) => {
                self.start_nmi = false;
                log::info!(
                    "{: >3},{: >3}: Suppress NMI @ frame {}",
                    self.scanline,
                    self.cycles,
                    self.frames
                );
            }
            (241, 2) if self.ctrl.contains(ControlRegister::GENERATE_NMI) => {
                self.start_nmi = false;
                self.nmi_interrupt = None;
                log::info!(
                    "{: >3},{: >3}: Suppress NMI @ frame {}",
                    self.scanline,
                    self.cycles,
                    self.frames
                );
            }
            _ => {}
        };
        if self.scanline == 240 || self.scanline == 241 {
            log::info!(
                "{: >3},{: >3}: STATUS={:08b} @ frame {}",
                self.scanline,
                self.cycles,
                self.status.bits(),
                self.frames
            );
        }
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
        self.update_a12(rom, addr);

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
            0x3F00..=0x3FFF => self.palette_table[(addr - 0x3F00) as usize & 0x1F],
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
                self.palette_table[(addr - 0x3F00) as usize & 0x1F] = val;
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
            0x3F00..=0x3FFF => self.palette_table[(addr - 0x3F00) as usize & 0x1F],
            _ => 0,
        }
    }
}
