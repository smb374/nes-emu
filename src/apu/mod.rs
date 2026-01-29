mod noise;
mod pulse;
pub mod registers;
mod triag;

use std::collections::VecDeque;

use self::{
    noise::NoiseChannel,
    pulse::PulseChannel,
    registers::{APUStatus, FrameCounter, NoiseRegister, PulseRegister, TriangleRegister},
    triag::TriangleChannel,
};

const CPU_FREQ: f64 = 1_789_773.0;
const SAMPLE_RATE: f64 = 44_100.0;
const FRAME_COUNTER_RATE: usize = 3728;

#[derive(Debug)]
pub struct APU {
    pub pulse1_reg: PulseRegister,
    pub pulse2_reg: PulseRegister,
    pub triag_reg: TriangleRegister,
    pub noise_reg: NoiseRegister,
    pub status: APUStatus,
    pub frame_counter: FrameCounter,
    pub irq_sig: bool,

    pulse1: PulseChannel,
    pulse2: PulseChannel,
    triangle: TriangleChannel,
    noise: NoiseChannel,

    cycles: usize,
    frame_cycle: usize,

    sample_accumulator: f64,
    pub sample_buffer: VecDeque<f32>,
}

impl Default for APU {
    fn default() -> Self {
        Self {
            pulse1_reg: PulseRegister::default(),
            pulse2_reg: PulseRegister::default(),
            triag_reg: TriangleRegister::default(),
            noise_reg: NoiseRegister::default(),
            status: APUStatus::default(),
            frame_counter: FrameCounter::default(),
            irq_sig: false,

            pulse1: PulseChannel::new(false),
            pulse2: PulseChannel::new(true),
            triangle: TriangleChannel::new(),
            noise: NoiseChannel::new(),

            cycles: 0,
            frame_cycle: 0,
            sample_accumulator: 0.0,
            sample_buffer: VecDeque::with_capacity(4096),
        }
    }
}

impl APU {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tick(&mut self, cycles: u8) {
        let cycles = cycles as usize;
        self.cycles += cycles;

        let old_quarter_frame = self.frame_cycle / FRAME_COUNTER_RATE;
        self.frame_cycle += cycles;

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

        self.pulse1.clock_timer(cycles, &self.pulse1_reg);
        self.pulse2.clock_timer(cycles, &self.pulse2_reg);
        self.triangle.clock_timer(cycles, &self.triag_reg);
        self.noise.clock_timer(cycles, &self.noise_reg);

        self.generate_samples(cycles);
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
        self.pulse1.clock_envelope(&self.pulse1_reg);
        self.pulse2.clock_envelope(&self.pulse2_reg);
        self.triangle.clock_linear_counter(&self.triag_reg);
        self.noise.clock_envelope(&self.noise_reg);
    }

    fn clock_length_and_sweep(&mut self) {
        if self.status.contains(APUStatus::PULSE_CHANNEL1) {
            self.pulse1.clock_length(&self.pulse1_reg);
            self.pulse1.clock_sweep(&self.pulse1_reg);
        }
        if self.status.contains(APUStatus::PULSE_CHANNEL2) {
            self.pulse2.clock_length(&self.pulse2_reg);
            self.pulse2.clock_sweep(&self.pulse2_reg);
        }
        if self.status.contains(APUStatus::NOISE_CHANNEL) {
            self.noise.clock_length(&self.noise_reg);
        }
        if self.status.contains(APUStatus::TRIAG_CHANNEL) {
            self.triangle.clock_length(&self.triag_reg);
        }
    }

    fn generate_samples(&mut self, cycles: usize) {
        self.sample_accumulator += cycles as f64 * SAMPLE_RATE / CPU_FREQ;
        let samples_to_generate = self.sample_accumulator as usize;

        for _ in 0..samples_to_generate {
            let sample = self.mix_channels();
            self.sample_buffer.push_back(sample);
        }

        self.sample_accumulator -= samples_to_generate as f64;
    }

    fn mix_channels(&self) -> f32 {
        let pulse1_out = self.pulse1.output();
        let pulse2_out = self.pulse2.output();
        let triangle_out = self.triangle.output();
        let noise_out = self.noise.output();

        // Non-linear mixing (TODO: use lookup tables)
        let square_sum = pulse1_out + pulse2_out;
        let square_out = if square_sum == 0 {
            0.0
        } else {
            95.88 / (8128.0 / square_sum as f32 + 100.0)
        };

        let tnd_sum = 3.0 * triangle_out as f32 / 8227.0 + 2.0 * noise_out as f32 / 12241.0;
        let tnd_out = if tnd_sum == 0.0 {
            0.0
        } else {
            159.79 / (1.0 / tnd_sum + 100.0)
        };

        square_out + tnd_out
    }
}
