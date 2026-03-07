mod channel;
mod mix_table;
pub mod registers;
mod units;

use std::{
    sync::mpsc::{self, SyncSender, TryRecvError},
    time::SystemTime,
};

pub use channel::TimedChannel;
use cpal::{
    Device, Stream, StreamConfig,
    traits::{DeviceTrait, StreamTrait},
};

use crate::apu::units::filter::Filter;

use self::{
    channel::{dmc::DMCChannel, noise::NoiseChannel, pulse::PulseChannel, triag::TriangleChannel},
    mix_table::*,
    registers::{APUStatus, FrameCounter},
};

const CPU_FREQ: u32 = 1_789_773;
const SAMPLE_RATE: u32 = 44_100;

pub struct APU {
    pub status: APUStatus,
    pub frame_counter: FrameCounter,
    pub irq_sig: bool,
    pub put_cycle: bool,
    pub pulse1: PulseChannel,
    pub pulse2: PulseChannel,
    pub triag: TriangleChannel,
    pub noise: NoiseChannel,
    pub dmc: DMCChannel,
    dmc_dma_schedule: Option<usize>,
    clear_irq_flag: bool,

    cycles: usize,
    fcycles: usize,
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
        let time_base = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .ok()
            .unwrap_or_default()
            .as_millis();

        Self {
            status: APUStatus::default(),
            frame_counter: FrameCounter::default(),
            irq_sig: false,
            put_cycle: (time_base & 1) == 1,
            pulse1: PulseChannel::new(false),
            pulse2: PulseChannel::new(true),
            triag: TriangleChannel::new(),
            noise: NoiseChannel::new(),
            dmc: DMCChannel::default(),
            dmc_dma_schedule: None,
            clear_irq_flag: false,

            cycles: 0,
            fcycles: 0,
            sample_accumulator: 0.0,
            sample_tx: tx,
            hpf90: Filter::new(90.0, true),
            hpf442: Filter::new(442.0, true),
            lpf14k: Filter::new(14000.0, false),
            pending_frame_counter: None,
            _stream: stream,
        }
    }

    pub fn tick(&mut self) {
        self.cycles += 1;

        if self.clear_irq_flag && !self.put_cycle {
            self.clear_irq_flag = false;
            self.status.remove(APUStatus::FRAME_INTERRUPT);
        }
        self.step_frame_sequencer();
        if self.frame_counter.contains(FrameCounter::IRQ_INHIBIT) && self.fcycles != 14914 {
            self.status.remove(APUStatus::FRAME_INTERRUPT);
        }
        self.triag.clock_timer();
        self.dmc.clock_timer();

        if self.put_cycle {
            self.pulse1.clock_timer();
            self.pulse2.clock_timer();
            self.noise.clock_timer();
        }

        self.generate_samples();
        self.status.set(APUStatus::DMC_INTERRUPT, self.dmc.irq_flag);

        self.irq_sig = self
            .status
            .intersects(APUStatus::DMC_INTERRUPT | APUStatus::FRAME_INTERRUPT);

        self.put_cycle = !self.put_cycle;
        if !self.put_cycle {
            self.fcycles += 1;
            if self.dmc_dma_schedule.is_some_and(|c| c == self.fcycles) {
                self.dmc.dma_sample = true;
                self.dmc.dma_reload = false;
                self.dmc_dma_schedule = None;
            }
        }
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
    }

    fn step_frame_sequencer(&mut self) {
        if !self.frame_counter.is_five_mode() {
            match (self.fcycles, self.put_cycle) {
                (3728, true) => {
                    self.clock_envelopes();
                }
                (7456, true) => {
                    self.clock_envelopes();
                    self.clock_length_and_sweep();
                }
                (11185, true) => {
                    self.clock_envelopes();
                }
                (14914, false) => {
                    self.status.set(APUStatus::FRAME_INTERRUPT, true);
                }
                (14914, true) => {
                    self.clock_envelopes();
                    self.clock_length_and_sweep();
                    self.status.set(APUStatus::FRAME_INTERRUPT, true);
                }
                (14915, false) => {
                    self.fcycles = 0;
                    self.status
                        .set(APUStatus::FRAME_INTERRUPT, self.frame_counter.emit_irq());
                }
                _ => {}
            }
        } else {
            match (self.fcycles, self.put_cycle) {
                (3728, true) => {
                    self.clock_envelopes();
                }
                (7456, true) => {
                    self.clock_envelopes();
                    self.clock_length_and_sweep();
                }
                (11185, true) => {
                    self.clock_envelopes();
                }
                (14914, true) => {
                    // Do nothing
                }
                (18640, true) => {
                    self.clock_envelopes();
                    self.clock_length_and_sweep();
                }
                (18641, false) => {
                    self.fcycles = 0;
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

    fn generate_samples(&mut self) {
        self.sample_accumulator += 1.0 * SAMPLE_RATE as f64 / CPU_FREQ as f64;
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
        self.clear_irq_flag = true;
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
            if self.dmc.sample_buffer_empty {
                self.dmc_dma_schedule = Some(self.fcycles + 2);
            }
            self.dmc.start();
        } else {
            self.dmc.stop();
        }

        self.irq_sig = self.status.contains(APUStatus::FRAME_INTERRUPT);
    }

    pub fn write_frame_counter(&mut self, value: u8) {
        self.pending_frame_counter = Some((value, if self.put_cycle { 3 } else { 4 }));
    }

    fn apply_frame_counter(&mut self, value: u8) {
        self.frame_counter.update(value);
        if (value & 0x40) != 0 {
            self.status.remove(APUStatus::FRAME_INTERRUPT);
            self.irq_sig = self.dmc.irq_flag;
        }

        if (value & 0x80) != 0 {
            self.clock_envelopes();
            self.clock_length_and_sweep();
        }

        self.fcycles = 0;
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
