use crate::apu::units::envelope::Envelope;

use super::{LENGTH_TABLE, TimedChannel};

// Noise period lookup table (NTSC)
// Indexed by bits 0-3 of $400E
#[rustfmt::skip]
const NOISE_PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068
];

#[derive(Debug)]
pub struct NoiseChannel {
    loop_flag: bool,
    const_volume: bool,
    load_volume: u8,

    envelope: Envelope,
    timer_period: u16,
    timer_counter: u16,
    pub length_enabled: bool,
    pub length_counter: u8,
    shift_register: u16, // 15-bit LFSR
    mode: bool,          // false = mode 0 (normal), true = mode 1 (short/metallic)
}

impl NoiseChannel {
    pub fn new() -> Self {
        Self {
            loop_flag: false,
            const_volume: false,
            load_volume: 0,

            envelope: Envelope::default(),
            timer_period: 0,
            timer_counter: 0,
            length_enabled: true,
            length_counter: 0,
            shift_register: 1, // Initial value is 1
            mode: false,
        }
    }

    pub fn clock_timer(&mut self, cycles: usize) {
        for _ in 0..cycles {
            if self.timer_counter == 0 {
                self.timer_counter = self.timer_period;
                self.clock_shift_register();
            } else {
                self.timer_counter -= 1;
            }
        }
    }

    fn clock_shift_register(&mut self) {
        // Feedback bit depends on mode
        let feedback_bit = if self.mode {
            // Mode 1 (short): XOR bits 0 and 6
            (self.shift_register & 1) ^ ((self.shift_register >> 6) & 1)
        } else {
            // Mode 0 (normal): XOR bits 0 and 1
            (self.shift_register & 1) ^ ((self.shift_register >> 1) & 1)
        };

        // Shift right and insert feedback at bit 14
        self.shift_register >>= 1;
        self.shift_register |= feedback_bit << 14;
    }

    pub fn clock_envelope(&mut self) {
        self.envelope
            .clock(self.loop_flag, self.const_volume, self.load_volume);
    }

    pub fn clock_length(&mut self) {
        if !self.loop_flag && self.length_counter != 0 {
            self.length_counter -= 1;
        }
    }

    pub fn output(&self) -> u8 {
        // Silence if length counter is zero
        if self.length_counter == 0 {
            return 0;
        }

        // Output is 0 if bit 0 of shift register is set (inverted logic)
        if self.shift_register & 1 == 1 {
            return 0;
        }

        // Otherwise output envelope volume
        self.envelope.output
    }

    pub fn update_volume(&mut self, val: u8) {
        self.loop_flag = val & 0x20 != 0;
        self.const_volume = val & 0x10 != 0;
        self.load_volume = val & 0x0F;
    }
}

impl TimedChannel for NoiseChannel {
    fn update_period_lo(&mut self, val: u8) {
        self.mode = val & 0x80 != 0;
        self.timer_period = NOISE_PERIOD_TABLE[(val & 0x0F) as usize];
    }

    fn update_period_hi(&mut self, val: u8) {
        // Load length counter
        if self.length_enabled {
            self.length_counter = LENGTH_TABLE[(val >> 3) as usize];
        }

        // Set envelope start flag
        self.envelope.set_start();
    }
}
