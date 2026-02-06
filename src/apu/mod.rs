mod channel;
pub mod registers;
mod units;

pub use channel::TimedChannel;
use sdl2::audio::AudioQueue;

use crate::{apu::units::filter::Filter, cartridge::Rom};

use self::{
    channel::{dmc::DMCChannel, noise::NoiseChannel, pulse::PulseChannel, triag::TriangleChannel},
    registers::{APUStatus, FrameCounter},
};

const CPU_FREQ: f64 = 1_789_773.0;
const SAMPLE_RATE: f64 = 44_100.0;
const FC_STEPS: [usize; 5] = [3728, 7456, 11185, 14914, 18640];

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
    hpf90: Filter<44100>,
    hpf442: Filter<44100>,
    lpf14k: Filter<44100>,

    audio_queue: AudioQueue<f32>,
    pending_frame_counter: Option<(u8, u8)>,
}

impl APU {
    pub fn new(audio_queue: AudioQueue<f32>) -> Self {
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
            hpf90: Filter::new(90.0, true),
            hpf442: Filter::new(442.0, true),
            lpf14k: Filter::new(14000.0, false),
            pending_frame_counter: None,
            audio_queue,
        }
    }

    pub fn tick(&mut self, rom: &mut Rom, cycles: u16, oam_dma: bool) -> usize {
        let cycles = cycles as usize;
        let mut stall = 0;

        for _ in 0..cycles {
            self.cycles += 1;

            self.triag.clock_timer(1);
            stall += self.dmc.clock_timer(1, rom, oam_dma);
            self.cycle_accumulator += 1;
            if self.cycle_accumulator >= 2 {
                self.cycle_accumulator = 0;
                self.step_apu_sequencer();
            }
            if let Some((val, mut delay)) = self.pending_frame_counter.take() {
                if delay != 0 {
                    delay -= 1;
                    self.pending_frame_counter = Some((val, delay));
                } else {
                    self.apply_frame_counter(val);
                }
            }
        }

        self.generate_samples(cycles);

        if self.dmc.irq_flag {
            self.status.insert(APUStatus::DMC_INTERRUPT);
        }

        if self.dmc.irq_flag || self.status.contains(APUStatus::FRAME_INTERRUPT) {
            self.irq_sig = true;
        }

        stall
    }

    fn step_apu_sequencer(&mut self) {
        self.pulse1.clock_timer(1);
        self.pulse2.clock_timer(1);
        self.noise.clock_timer(1);

        let old_frame_cycle = self.frame_cycle;
        self.frame_cycle += 1;

        let (max_step_idx, frame_length) = if self.frame_counter.is_five_mode() {
            (4, FC_STEPS[4])
        } else {
            (3, FC_STEPS[3])
        };

        if self.frame_cycle >= frame_length {
            self.frame_cycle -= frame_length;
            self.clock_frame_sequencer(max_step_idx);
        } else {
            for i in 0..max_step_idx {
                if old_frame_cycle < FC_STEPS[i] && self.frame_cycle >= FC_STEPS[i] {
                    self.clock_frame_sequencer(i);
                }
            }
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

                self.status
                    .set(APUStatus::FRAME_INTERRUPT, self.frame_counter.emit_irq());
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

        let mut buf = Vec::with_capacity(2048);

        for _ in 0..samples_to_generate {
            buf.push(self.mix_channels());
        }

        self.audio_queue.queue_audio(&buf).unwrap();

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
        if self.irq_sig || self.status.contains(APUStatus::FRAME_INTERRUPT) {
            res |= 0x40;
        }
        if self.dmc.irq_flag {
            self.status.insert(APUStatus::DMC_INTERRUPT);
            res |= 0x80;
        }
        self.status.remove(APUStatus::FRAME_INTERRUPT);
        self.irq_sig = self.dmc.irq_flag;
        res
    }

    pub fn write_status(&mut self, val: u8) {
        self.status.update(val);
        self.status.remove(APUStatus::DMC_INTERRUPT);
        self.dmc.clear_interrupt();
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
            self.dmc.start();
        } else {
            self.dmc.stop(); // Sets bytes_remaining to 0
        }

        self.irq_sig = self.status.contains(APUStatus::FRAME_INTERRUPT);
    }

    pub fn write_frame_counter(&mut self, value: u8) {
        self.pending_frame_counter = Some((value, if self.cycles % 2 == 0 { 3 } else { 4 }));
    }

    fn apply_frame_counter(&mut self, value: u8) {
        self.frame_counter.update(value);
        self.frame_cycle = 0;

        if (value & 0x40) != 0 {
            self.status.remove(APUStatus::FRAME_INTERRUPT);
            self.irq_sig = self.dmc.irq_flag;
        }

        if self.frame_counter.is_five_mode() {
            self.clock_envelopes();
            self.clock_length_and_sweep();
        }
    }

    fn mix_channels(&mut self) -> f32 {
        let pulse1_out = self.pulse1.output();
        let pulse2_out = self.pulse2.output();
        let triangle_out = self.triag.output();
        let noise_out = self.noise.output();
        let dmc_out = self.dmc.output();

        let pulse_sum = pulse1_out + pulse2_out;
        let pulse_out = if pulse_sum == 0 {
            0.0
        } else {
            95.88 / (8128.0 / pulse_sum as f32 + 100.0)
        };

        let tnd_sum = triangle_out + noise_out + dmc_out;
        let tnd_out = if tnd_sum == 0 {
            0.0
        } else {
            159.79
                / (1.0
                    / ((triangle_out as f32 / 8227.0)
                        + (noise_out as f32 / 12241.0)
                        + (dmc_out as f32 / 22638.0))
                    + 100.0)
        };

        let output = (pulse_out + tnd_out - 0.5) * 2.0;

        self.lpf14k
            .filter(self.hpf442.filter(self.hpf90.filter(output)))
    }
}
