mod channel;
mod mix_table;
pub mod registers;
mod units;

use std::sync::mpsc::{self, SyncSender, TryRecvError};

pub use channel::TimedChannel;
use cpal::{
    Device, Stream, StreamConfig,
    traits::{DeviceTrait, StreamTrait},
};

use crate::{apu::units::filter::Filter, cartridge::Rom};

use self::{
    channel::{dmc::DMCChannel, noise::NoiseChannel, pulse::PulseChannel, triag::TriangleChannel},
    mix_table::*,
    registers::{APUStatus, FrameCounter},
};

const CPU_FREQ: u32 = 1_789_773;
const SAMPLE_RATE: u32 = 44_100;
const FC_STEPS0: [usize; 4] = [7456, 7458, 7458, 7458];
const FC_STEPS1: [usize; 5] = [7458, 7456, 7458, 7458, 7452];
const FC_RELOAD0: usize = 7459;

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
    frame_step: usize,
    frame_delay: usize,
    cycle_accumulator: usize,

    sample_accumulator: f64,
    sample_tx: SyncSender<f32>,
    hpf90: Filter<44100>,
    hpf442: Filter<44100>,
    lpf14k: Filter<44100>,

    _stream: Stream,
    pending_frame_counter: Option<(u8, u8)>,
}

impl APU {
    pub fn new(device: Device) -> Self {
        let stream_config = StreamConfig {
            channels: 1,
            sample_rate: SAMPLE_RATE,
            buffer_size: cpal::BufferSize::Default,
        };

        let (tx, rx) = mpsc::sync_channel(SAMPLE_RATE as usize * 2);

        let stream = device
            .build_output_stream(
                &stream_config,
                move |data: &mut [f32], _| {
                    for i in 0..data.len() {
                        match rx.try_recv() {
                            Ok(sample) => {
                                data[i] = sample;
                            }
                            Err(TryRecvError::Empty) => break,
                            Err(TryRecvError::Disconnected) => {
                                panic!("Shouldn't be disconnected here!")
                            }
                        }
                    }
                },
                |err| log::warn!("[Stream] stream error: {}", err),
                None,
            )
            .expect("Failed to create output stream");
        stream.play().expect("Failed to start stream");

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
            frame_step: 0,
            frame_delay: FC_RELOAD0,
            sample_accumulator: 0.0,
            sample_tx: tx,
            hpf90: Filter::new(90.0, true),
            hpf442: Filter::new(442.0, true),
            lpf14k: Filter::new(14000.0, false),
            pending_frame_counter: None,
            _stream: stream,
        }
    }

    pub fn tick(&mut self, rom: &mut Rom, cycles: u16, oam_dma: bool) -> usize {
        let cycles = cycles as usize;
        let mut stall = 0;

        for _ in 0..cycles {
            self.cycles += 1;
            if let Some((val, mut delay)) = self.pending_frame_counter.take() {
                if delay != 0 {
                    delay -= 1;
                }
                if delay == 0 {
                    self.apply_frame_counter(val);
                } else {
                    self.pending_frame_counter = Some((val, delay));
                }
            }

            self.step_frame_sequencer();

            self.triag.clock_timer(1);
            stall += self.dmc.clock_timer(1, rom, oam_dma);

            self.cycle_accumulator += 1;
            self.cycle_accumulator &= 1;
            if self.cycle_accumulator == 0 {
                self.pulse1.clock_timer(1);
                self.pulse2.clock_timer(1);
                self.noise.clock_timer(1);
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

    fn step_frame_sequencer(&mut self) {
        if self.frame_delay > 0 {
            self.frame_delay -= 1;
        }
        if self.frame_delay == 0 {
            self.clock_frame_sequencer(self.frame_step);
            if self.frame_counter.is_five_mode() {
                self.frame_delay = FC_STEPS1[self.frame_step];
                self.frame_step = (self.frame_step + 1) % 5;
            } else {
                self.frame_delay = FC_STEPS0[self.frame_step];
                self.frame_step = (self.frame_step + 1) % 4;
            }
        }
    }

    fn clock_frame_sequencer(&mut self, quarter_frame: usize) {
        if self.frame_counter.is_five_mode() {
            match quarter_frame {
                0 => {
                    self.clock_envelopes();
                    self.clock_length_and_sweep();
                }
                1 => {
                    self.clock_envelopes();
                }
                2 => {
                    self.clock_envelopes();
                    self.clock_length_and_sweep();
                }
                3 => {
                    self.clock_envelopes();
                }
                _ => {}
            }
        } else {
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
                3 => {
                    self.clock_envelopes();
                    self.clock_length_and_sweep();

                    self.status
                        .set(APUStatus::FRAME_INTERRUPT, self.frame_counter.emit_irq());
                    self.irq_sig = self.frame_counter.emit_irq();
                }
                _ => {}
            }
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
        self.sample_accumulator += cycles as f64 * SAMPLE_RATE as f64 / CPU_FREQ as f64;
        let samples_to_generate = self.sample_accumulator as usize;

        for _ in 0..samples_to_generate {
            let sample = self.mix_channels();
            self.sample_tx.send(sample).expect("Should be able to send");
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
        self.pending_frame_counter = Some((value, if self.cycles % 2 == 0 { 3 } else { 2 }));
    }

    fn apply_frame_counter(&mut self, value: u8) {
        self.frame_counter.update(value);

        if (value & 0x40) != 0 {
            self.status.remove(APUStatus::FRAME_INTERRUPT);
            self.irq_sig = self.dmc.irq_flag;
        }

        self.frame_step = 0;
        if self.frame_counter.is_five_mode() {
            self.frame_delay = 1;
        } else {
            self.frame_delay = FC_RELOAD0;
        }
    }

    fn mix_channels(&mut self) -> f32 {
        let p1 = self.pulse1.output() as usize;
        let p2 = self.pulse2.output() as usize;
        let tri = self.triag.output() as usize;
        let noise = self.noise.output() as usize;
        let dmc = self.dmc.output() as usize;

        let pulse_out = PULSE_TABLE[p1 + p2];
        let tnd_out = TND_TABLE[3 * tri + 2 * noise + dmc];

        let output = (pulse_out + tnd_out - 0.5) * 2.0;

        self.lpf14k
            .filter(self.hpf442.filter(self.hpf90.filter(output)))
    }
}
