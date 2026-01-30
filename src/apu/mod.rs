mod channel;
pub mod registers;
mod units;

pub use channel::TimedChannel;

use crate::cartridge::Rom;

use self::{
    channel::{dmc::DMCChannel, noise::NoiseChannel, pulse::PulseChannel, triag::TriangleChannel},
    registers::{APUStatus, FrameCounter},
};

const CPU_FREQ: f64 = 1_789_773.0;
const SAMPLE_RATE: f64 = 44_100.0;
const FRAME_COUNTER_RATE: usize = 3728;

pub struct APU {
    pub status: APUStatus,
    pub frame_counter: FrameCounter,
    pub irq_sig: bool,

    pub pulse1: PulseChannel,
    pub pulse2: PulseChannel,
    pub triag: TriangleChannel,
    pub noise: NoiseChannel,
    pub dmc: DMCChannel,

    cycles: usize,
    frame_cycle: usize,
    cycle_accumulator: usize,

    sample_accumulator: f64,
    pub sample_buffer: Vec<f32>,
}

impl APU {
    pub fn new() -> Self {
        Self {
            status: APUStatus::default(),
            frame_counter: FrameCounter::default(),
            irq_sig: false,

            pulse1: PulseChannel::new(false),
            pulse2: PulseChannel::new(true),
            triag: TriangleChannel::new(),
            noise: NoiseChannel::new(),
            dmc: DMCChannel::default(),

            cycles: 0,
            cycle_accumulator: 0,
            frame_cycle: 0,
            sample_accumulator: 0.0,
            sample_buffer: Vec::with_capacity(48000),
        }
    }

    pub fn tick(&mut self, rom: &mut Rom, cycles: u16) {
        let cycles = cycles as usize;
        self.cycles += cycles;

        let total_cycles = self.cycle_accumulator + cycles;
        let apu_ticks = total_cycles / 2;
        self.cycle_accumulator = total_cycles % 2;

        self.triag.clock_timer(cycles);
        self.dmc.clock_timer(cycles, rom);

        if apu_ticks > 0 {
            self.pulse1.clock_timer(apu_ticks);
            self.pulse2.clock_timer(apu_ticks);
            self.noise.clock_timer(apu_ticks);

            let old_quarter_frame = self.frame_cycle / FRAME_COUNTER_RATE;
            self.frame_cycle += apu_ticks;

            let frame_length = if self.frame_counter.is_five_mode() {
                FRAME_COUNTER_RATE * 5
            } else {
                FRAME_COUNTER_RATE * 4
            };

            if self.frame_cycle >= frame_length {
                self.frame_cycle %= frame_length;
            }

            let new_quarter_frame = self.frame_cycle / FRAME_COUNTER_RATE;

            if old_quarter_frame != new_quarter_frame {
                self.clock_frame_sequencer(new_quarter_frame);
            }
        }

        self.generate_samples(cycles);

        // Check for DMC IRQ (can happen at any time)
        if self.dmc.irq_flag {
            self.irq_sig = true;
        }
    }

    fn clock_frame_sequencer(&mut self, quarter_frame: usize) {
        match quarter_frame {
            0 => {
                self.clock_envelopes();
            }
            1 => {
                self.clock_envelopes();
                self.clock_length_and_sweep();
            }
            2 => {
                self.clock_envelopes();
            }
            3 if !self.frame_counter.is_five_mode() => {
                self.clock_envelopes();
                self.clock_length_and_sweep();

                self.irq_sig = self.frame_counter.emit_irq();
            }
            4 if self.frame_counter.is_five_mode() => {
                self.clock_envelopes();
                self.clock_length_and_sweep();
            }
            _ => {}
        }
    }

    fn clock_envelopes(&mut self) {
        self.pulse1.clock_envelope();
        self.pulse2.clock_envelope();
        self.triag.clock_linear_counter();
        self.noise.clock_envelope();
    }

    fn clock_length_and_sweep(&mut self) {
        if self.status.contains(APUStatus::PULSE_CHANNEL1) {
            self.pulse1.clock_length();
            self.pulse1.clock_sweep();
        }
        if self.status.contains(APUStatus::PULSE_CHANNEL2) {
            self.pulse2.clock_length();
            self.pulse2.clock_sweep();
        }
        if self.status.contains(APUStatus::NOISE_CHANNEL) {
            self.noise.clock_length();
        }
        if self.status.contains(APUStatus::TRIAG_CHANNEL) {
            self.triag.clock_length();
        }
    }

    fn generate_samples(&mut self, cycles: usize) {
        self.sample_accumulator += cycles as f64 * SAMPLE_RATE / CPU_FREQ;
        let samples_to_generate = self.sample_accumulator as usize;

        for _ in 0..samples_to_generate {
            let sample = self.mix_channels();
            self.sample_buffer.push(sample);
        }

        self.sample_accumulator -= samples_to_generate as f64;
    }

    pub fn read_status(&mut self) -> u8 {
        let mut res = 0u8;
        if self.pulse1.length_counter > 0 {
            res |= 0x01;
        }
        if self.pulse2.length_counter > 0 {
            res |= 0x02;
        }
        if self.triag.length_counter > 0 {
            res |= 0x04;
        }
        if self.noise.length_counter > 0 {
            res |= 0x08;
        }
        if self.dmc.bytes_remaining() > 0 {
            res |= 0x10;
        }
        if self.status.contains(APUStatus::FRAME_INTERRUPT) {
            res |= 0x40;
        }
        if self.status.contains(APUStatus::DMC_INTERRUPT) {
            res |= 0x80;
        }
        self.status.remove(APUStatus::FRAME_INTERRUPT);
        self.irq_sig = false;
        res
    }

    pub fn write_status(&mut self, val: u8) {
        self.status.update(val);
        // If bit is 0, length counter must be cleared immediately
        if !self.status.contains(APUStatus::PULSE_CHANNEL1) {
            self.pulse1.length_counter = 0;
            self.pulse1.length_enabled = false;
        } else {
            self.pulse1.length_enabled = true;
        }
        if !self.status.contains(APUStatus::PULSE_CHANNEL2) {
            self.pulse2.length_counter = 0;
            self.pulse2.length_enabled = false;
        } else {
            self.pulse2.length_enabled = true;
        }
        if !self.status.contains(APUStatus::TRIAG_CHANNEL) {
            self.triag.length_counter = 0;
            self.triag.length_enabled = false;
        } else {
            self.triag.length_enabled = true;
        }
        if !self.status.contains(APUStatus::NOISE_CHANNEL) {
            self.noise.length_counter = 0;
            self.noise.length_enabled = false;
        } else {
            self.noise.length_enabled = true;
        }

        // DMC control
        if self.status.contains(APUStatus::DMC_CHANNEL) {
            self.dmc.start_playback();
        } else {
            self.dmc.stop_playback();
        }
    }

    pub fn write_frame_counter(&mut self, value: u8) {
        self.frame_counter.update(value);
        if value & 0x40 != 0 {
            self.status.remove(APUStatus::FRAME_INTERRUPT);
            self.status.remove(APUStatus::DMC_INTERRUPT);
            self.irq_sig = false;
        }

        // If 5-step mode is set, immediately clock all units
        if self.frame_counter.is_five_mode() {
            self.clock_envelopes();
            self.clock_length_and_sweep();
        }
    }

    fn mix_channels(&self) -> f32 {
        let pulse1_out = self.pulse1.output();
        let pulse2_out = self.pulse2.output();
        let triangle_out = self.triag.output();
        let noise_out = self.noise.output();
        let dmc_out = self.dmc.output();

        let square_out = 0.00752 * (pulse1_out + pulse2_out) as f32;

        let tnd_out = 0.00851 * (3 * triangle_out) as f32
            + 0.00494 * (2 * noise_out) as f32
            + 0.00335 * dmc_out as f32;

        square_out + tnd_out
    }
}
