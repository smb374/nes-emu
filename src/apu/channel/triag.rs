use crate::apu::registers::TriangleRegister;

use super::{LENGTH_TABLE, TimedChannel};

// Triangle waveform: 32-step sequence
// Goes from 15 -> 0 -> 15 in a linear ramp
#[rustfmt::skip]
const TRIANGLE_TABLE: [u8; 32] = [
    15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
];

#[derive(Debug)]
pub struct TriangleChannel {
    timer_period: u16,
    timer_counter: u16,
    pub length_enabled: bool,
    pub length_counter: u8,
    linear_counter: u8,
    linear_counter_reload_flag: bool,
    sequence_idx: u8, // 0-31, position in triangle wave
}

impl TriangleChannel {
    pub fn new() -> Self {
        Self {
            timer_period: 0,
            timer_counter: 0,
            length_enabled: true,
            length_counter: 0,
            linear_counter: 0,
            linear_counter_reload_flag: false,
            sequence_idx: 0,
        }
    }

    pub fn clock_timer(&mut self, cycles: usize) {
        // Triangle channel only advances if both counters are non-zero
        if self.length_counter == 0 || self.linear_counter == 0 {
            return;
        }

        for _ in 0..cycles {
            if self.timer_counter == 0 {
                self.timer_counter = self.timer_period;
                // Advance the 32-step sequencer
                self.sequence_idx = (self.sequence_idx + 1) % 32;
            } else {
                self.timer_counter -= 1;
            }
        }
    }

    pub fn clock_linear_counter(&mut self, reg: &TriangleRegister) {
        if self.linear_counter_reload_flag {
            self.linear_counter = reg.counter & 0x7F;
        } else if self.linear_counter != 0 {
            self.linear_counter -= 1;
        }

        if reg.counter & 0x80 == 0 {
            self.linear_counter_reload_flag = false;
        }
    }

    pub fn clock_length(&mut self, reg: &TriangleRegister) {
        // Length counter only decrements if control flag is clear
        if reg.counter & 0x80 == 0 && self.length_counter != 0 {
            self.length_counter -= 1;
        }
    }

    pub fn output(&self) -> u8 {
        // Silence if either counter is zero
        if self.length_counter == 0 || self.linear_counter == 0 {
            return 0;
        }

        // Ultrasonic frequencies produce a pop/buzz, silence them
        if self.timer_period < 2 {
            return 0;
        }

        // Output current position in triangle wave
        TRIANGLE_TABLE[self.sequence_idx as usize]
    }
}

impl TimedChannel for TriangleChannel {
    fn update_period_lo(&mut self, val: u8) {
        self.timer_period = (self.timer_period & 0x0700) | val as u16;
    }

    fn update_period_hi(&mut self, val: u8) {
        self.timer_period = (self.timer_period & 0x00FF) | ((val as u16 & 0x07) << 8);

        // Writing to high byte sets the linear counter reload flag
        self.linear_counter_reload_flag = true;

        // Load length counter if enabled
        if self.length_enabled {
            self.length_counter = LENGTH_TABLE[(val >> 3) as usize];
        }
    }
}
