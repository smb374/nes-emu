use crate::apu::{
    registers::PulseRegister,
    units::{envelope::Envelope, sweep::Sweep},
};

use super::{LENGTH_TABLE, TimedChannel};

#[rustfmt::skip]
const DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0], // 12.5%
    [0, 1, 1, 0, 0, 0, 0, 0], // 25%
    [0, 1, 1, 1, 1, 0, 0, 0], // 50%
    [1, 0, 0, 1, 1, 1, 1, 1], // 25% negated (75% high)
];

#[derive(Debug)]
pub struct PulseChannel {
    envelope: Envelope,
    sweep: Sweep,
    timer_period: u16,
    timer_counter: u16,
    pub length_enabled: bool, // Updated in the beginning of each APU tick with status register
    length_counter: u8,
    duty_mode: u8,
    duty_idx: u8,
    muted: bool,
}

impl PulseChannel {
    pub fn new(is_pulse2: bool) -> Self {
        Self {
            envelope: Envelope::default(),
            sweep: Sweep::new(is_pulse2),
            timer_period: 0,
            timer_counter: 0,
            length_enabled: true,
            length_counter: 0,
            duty_mode: 0,
            duty_idx: 0,
            muted: false,
        }
    }
    pub fn clock_timer(&mut self, cycles: usize) {
        for _ in 0..cycles {
            if self.timer_counter == 0 {
                self.timer_counter = self.timer_period;
                self.duty_idx = (self.duty_idx + 1) % 8;
            } else {
                self.timer_counter -= 1;
            }
        }
    }
    pub fn clock_envelope(&mut self, reg: &PulseRegister) {
        self.envelope.clock(reg.envelope);
    }
    pub fn clock_length(&mut self, reg: &PulseRegister) {
        if reg.envelope & 0x20 == 0 && self.length_counter != 0 {
            self.length_counter -= 1;
        }
    }
    pub fn clock_sweep(&mut self, reg: &PulseRegister) {
        let (timer, muted) = self.sweep.clock(reg.sweep, self.timer_period);
        self.timer_period = timer;
        self.muted = muted;
    }
    pub fn output(&self) -> u8 {
        if self.muted || self.length_counter == 0 {
            0
        } else {
            let duty_output = DUTY_TABLE[self.duty_mode as usize][self.duty_idx as usize];
            if duty_output == 0 {
                0
            } else {
                self.envelope.output
            }
        }
    }
    pub fn update_duty(&mut self, val: u8) {
        self.duty_mode = (val & 0xC0) >> 6;
    }
}

impl TimedChannel for PulseChannel {
    fn update_period_lo(&mut self, val: u8) {
        self.timer_period = (self.timer_period & 0x0700) | val as u16;
    }
    fn update_period_hi(&mut self, val: u8) {
        self.timer_period = (self.timer_period & 0x00FF) | ((val as u16 & 0x07) << 8);

        self.duty_idx = 0;
        self.envelope.set_start();

        if self.length_enabled {
            self.length_counter = LENGTH_TABLE[(val >> 3) as usize];
        }
    }
}
