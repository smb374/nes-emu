use crate::apu::units::{envelope::Envelope, sweep::Sweep};

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
    duty_mode: u8,
    loop_flag: bool,
    const_volume: bool,
    load_volume: u8,

    sweep_enabled: bool,
    sweep_period: u8,
    sweep_negate: bool,
    sweep_shamt: u8,

    envelope: Envelope,
    sweep: Sweep,
    timer_period: u16,
    timer_counter: u16,
    pub length_enabled: bool, // Updated in the beginning of each APU tick with status register
    pub length_counter: u8,
    duty_idx: u8,
    muted: bool,
}

impl PulseChannel {
    pub fn new(is_pulse2: bool) -> Self {
        Self {
            duty_mode: 0,
            loop_flag: false,
            const_volume: false,
            load_volume: 0,

            sweep_enabled: false,
            sweep_period: 0,
            sweep_negate: false,
            sweep_shamt: 0,

            envelope: Envelope::default(),
            sweep: Sweep::new(is_pulse2),
            timer_period: 0,
            timer_counter: 0,
            length_enabled: true,
            length_counter: 0,
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
    pub fn clock_envelope(&mut self) {
        self.envelope
            .clock(self.loop_flag, self.const_volume, self.load_volume);
    }
    pub fn clock_length(&mut self) {
        if !self.loop_flag && self.length_counter != 0 {
            self.length_counter -= 1;
        }
    }
    pub fn clock_sweep(&mut self) {
        let (timer, muted) = self.sweep.clock(
            self.sweep_enabled,
            self.sweep_period,
            self.sweep_negate,
            self.sweep_shamt,
            self.timer_period,
        );
        self.timer_period = timer;
        self.muted = muted;
    }
    pub fn output(&self) -> u8 {
        if self.muted || self.length_counter == 0 || self.timer_period < 8 {
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
    pub fn update_volume(&mut self, val: u8) {
        self.duty_mode = (val & 0xC0) >> 6;
        self.loop_flag = (val & 0x20) != 0;
        self.const_volume = (val & 0x10) != 0;
        self.load_volume = val & 0x0F;
    }
    pub fn update_sweep(&mut self, val: u8) {
        self.sweep_enabled = (val & 0x80) != 0;
        self.sweep_period = (val & 0x70) >> 4;
        self.sweep_negate = (val & 0x08) != 0;
        self.sweep_shamt = val & 0x07;
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
