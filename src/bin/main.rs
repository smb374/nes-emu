use std::{
    collections::HashMap,
    ffi::c_int,
    io::Write,
    sync::{Arc, Mutex, atomic::Ordering},
    time::{Duration, Instant},
};

use clap::Parser;
use cpal::traits::HostTrait;
use eframe::egui;
use env_logger::Env;
use nes_emu::{
    bus::Bus,
    cartridge::Rom,
    cpu::{CPU, GLOBAL_EXIT},
    joypad,
    ppu::PPU,
};
use nix::sys::signal::{SaFlags, SigAction, SigHandler, SigSet, Signal};

const FRAME_DURATION: Duration = Duration::from_micros(16639);
const OVERSCAN_LEFT: usize = 0;
const OVERSCAN_RIGHT: usize = 0;
const OVERSCAN_TOP: usize = 8;
const OVERSCAN_BOTTOM: usize = 8;

const VISIBLE_WIDTH: usize = 256 - OVERSCAN_LEFT - OVERSCAN_RIGHT;
const VISIBLE_HEIGHT: usize = 240 - OVERSCAN_TOP - OVERSCAN_BOTTOM;

#[derive(Parser, Debug)]
#[command(name = "NES Emulator")]
#[command(about = "A Nintendo Entertainment System emulator", long_about = None)]
struct Args {
    #[arg(value_name = "ROM")]
    rom_path: String,
}

struct SharedFrameBuffer {
    data: [u8; VISIBLE_WIDTH * VISIBLE_HEIGHT * 3],
    frame_seq: u64,
}

#[allow(unused)]
enum InputEvent {
    KeyDown(joypad::JoypadButton),
    KeyUp(joypad::JoypadButton),
    Quit,
}

fn main() {
    env_logger::Builder::from_env(Env::default().default_filter_or("warn"))
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .init();

    let handler = SigHandler::Handler(shutdown);
    let action = SigAction::new(handler, SaFlags::empty(), SigSet::empty());
    unsafe {
        nix::sys::signal::sigaction(Signal::SIGINT, &action)
            .expect("Failed to set SIGINT handler.");
        nix::sys::signal::sigaction(Signal::SIGTERM, &action)
            .expect("Failed to set SIGTERM handler.");
    }

    let args = Args::parse();

    let frame_buffer = Arc::new(Mutex::new(SharedFrameBuffer {
        data: [0u8; VISIBLE_WIDTH * VISIBLE_HEIGHT * 3],
        frame_seq: 0,
    }));
    let (input_tx, input_rx) = std::sync::mpsc::channel::<InputEvent>();

    let fb_emu = Arc::clone(&frame_buffer);
    let emu_thread = std::thread::spawn(move || {
        let host = cpal::default_host();
        let device = host.default_output_device().expect("No available device");

        let path = std::path::absolute(&args.rom_path).expect("Failed to get absolute path");
        let bytes: Vec<u8> = std::fs::read(&path).expect("Failed to load ROM");
        let mut rom = Rom::new(&bytes, path).unwrap();

        if let Err(e) = rom.load_prg_ram() {
            eprintln!("Warning: Failed to load save file: {}", e);
        }

        let mut next_frame_target = Instant::now() + FRAME_DURATION;

        let bus = Bus::new(
            rom,
            device,
            move |ppu: &PPU, joypad: &mut joypad::Joypad| {
                {
                    let mut fb = fb_emu.lock().unwrap();
                    for y in 0..VISIBLE_HEIGHT {
                        let src_y = y + OVERSCAN_TOP;
                        let src_offset = (src_y * 256 + OVERSCAN_LEFT) * 3;
                        let dst_offset = y * VISIBLE_WIDTH * 3;

                        fb.data[dst_offset..dst_offset + VISIBLE_WIDTH * 3].copy_from_slice(
                            &ppu.frame_buffer[src_offset..src_offset + VISIBLE_WIDTH * 3],
                        );
                    }
                    fb.frame_seq += 1;
                }

                while let Ok(event) = input_rx.try_recv() {
                    match event {
                        InputEvent::KeyDown(button) => joypad.set_button_status(button, true),
                        InputEvent::KeyUp(button) => joypad.set_button_status(button, false),
                        InputEvent::Quit => return true,
                    }
                }

                // Frame pacing.
                let now = Instant::now();
                if now < next_frame_target {
                    std::thread::sleep(next_frame_target - now);
                } else if now - next_frame_target > FRAME_DURATION * 3 {
                    next_frame_target = now;
                }
                next_frame_target += FRAME_DURATION;

                false
            },
        );

        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run_with_cb(|cpu| {
            log::trace!("{}", nes_emu::trace::trace(cpu));
        });

        if let Err(e) = cpu.save_prg_ram() {
            log::warn!("Warning: Failed to save PRG-RAM: {}", e);
        }
    });

    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([(VISIBLE_WIDTH * 3) as f32, (VISIBLE_HEIGHT * 3) as f32])
            .with_resizable(true),
        ..Default::default()
    };

    let fb_ui = Arc::clone(&frame_buffer);

    eframe::run_native(
        "NES Emulator",
        native_options,
        Box::new(move |_cc| Ok(Box::new(EmulatorApp::new(fb_ui, input_tx)))),
    )
    .expect("Failed to run eframe");

    GLOBAL_EXIT.store(true, Ordering::Release);
    let _ = emu_thread.join();
}

struct EmulatorApp {
    frame_buffer: Arc<Mutex<SharedFrameBuffer>>,
    input_tx: std::sync::mpsc::Sender<InputEvent>,
    texture_handle: Option<egui::TextureHandle>,
    last_frame_seq: u64,
    key_map: HashMap<egui::Key, joypad::JoypadButton>,
    pressed: HashMap<egui::Key, bool>,
}

impl EmulatorApp {
    fn new(
        frame_buffer: Arc<Mutex<SharedFrameBuffer>>,
        input_tx: std::sync::mpsc::Sender<InputEvent>,
    ) -> Self {
        let mut key_map = HashMap::new();
        key_map.insert(egui::Key::ArrowDown, joypad::JoypadButton::DOWN);
        key_map.insert(egui::Key::ArrowUp, joypad::JoypadButton::UP);
        key_map.insert(egui::Key::ArrowRight, joypad::JoypadButton::RIGHT);
        key_map.insert(egui::Key::ArrowLeft, joypad::JoypadButton::LEFT);
        key_map.insert(egui::Key::Space, joypad::JoypadButton::SELECT);
        key_map.insert(egui::Key::Enter, joypad::JoypadButton::START);
        key_map.insert(egui::Key::Z, joypad::JoypadButton::BUTTON_A);
        key_map.insert(egui::Key::X, joypad::JoypadButton::BUTTON_B);

        Self {
            frame_buffer,
            input_tx,
            texture_handle: None,
            last_frame_seq: 0,
            key_map,
            pressed: HashMap::new(),
        }
    }

    fn process_input(&mut self, ctx: &egui::Context) {
        ctx.input(|input| {
            for (&key, button) in &self.key_map {
                let is_down = input.key_down(key);
                let was_down = *self.pressed.get(&key).unwrap_or(&false);

                if is_down && !was_down {
                    let _ = self.input_tx.send(InputEvent::KeyDown(*button));
                } else if !is_down && was_down {
                    let _ = self.input_tx.send(InputEvent::KeyUp(*button));
                }

                self.pressed.insert(key, is_down);
            }
        });
    }
}

impl eframe::App for EmulatorApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if GLOBAL_EXIT.load(Ordering::Acquire) {
            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
            return;
        }

        self.process_input(ctx);

        let needs_update = {
            let fb = self.frame_buffer.lock().unwrap();
            fb.frame_seq != self.last_frame_seq
        };

        if needs_update {
            let fb = self.frame_buffer.lock().unwrap();
            self.last_frame_seq = fb.frame_seq;

            let image = egui::ColorImage::from_rgb([VISIBLE_WIDTH, VISIBLE_HEIGHT], &fb.data);
            drop(fb);

            match &mut self.texture_handle {
                Some(handle) => handle.set(image, egui::TextureOptions::NEAREST),
                None => {
                    self.texture_handle =
                        Some(ctx.load_texture("nes_frame", image, egui::TextureOptions::NEAREST));
                }
            }
        }

        egui::CentralPanel::default()
            .frame(egui::Frame::NONE.fill(egui::Color32::BLACK))
            .show(ctx, |ui| {
                if let Some(tex) = &self.texture_handle {
                    let available = ui.available_size();
                    let aspect = VISIBLE_WIDTH as f32 / VISIBLE_HEIGHT as f32;
                    let size = if available.x / available.y > aspect {
                        egui::vec2(available.y * aspect, available.y)
                    } else {
                        egui::vec2(available.x, available.x / aspect)
                    };

                    let offset = (available - size) / 2.0;
                    ui.add_space(offset.y.max(0.0));
                    ui.horizontal(|ui| {
                        ui.add_space(offset.x.max(0.0));
                        ui.image(egui::load::SizedTexture::new(tex.id(), size));
                    });
                }
            });

        ctx.request_repaint();
    }

    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
        GLOBAL_EXIT.store(true, Ordering::Release);
    }
}

extern "C" fn shutdown(_: c_int) {
    log::info!("Shutting down...");
    GLOBAL_EXIT.store(true, Ordering::Release);
}
