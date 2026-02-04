use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

use clap::Parser;
use nes_emu::{bus::Bus, cartridge::Rom, cpu::CPU, joypad, ppu::PPU};
use sdl2::{
    audio::{AudioQueue, AudioSpecDesired},
    event::Event,
    keyboard::Keycode,
    pixels::PixelFormatEnum,
};

const FRAME_DURATION: Duration = Duration::from_micros(16639);
const OVERSCAN_LEFT: usize = 0;
const OVERSCAN_RIGHT: usize = 0;
const OVERSCAN_TOP: usize = 8;
const OVERSCAN_BOTTOM: usize = 8;

const VISIBLE_WIDTH: usize = 256 - OVERSCAN_LEFT - OVERSCAN_RIGHT; // 248
const VISIBLE_HEIGHT: usize = 240 - OVERSCAN_TOP - OVERSCAN_BOTTOM; // 224

#[derive(Parser, Debug)]
#[command(name = "NES Emulator")]
#[command(about = "A Nintendo Entertainment System emulator", long_about = None)]
struct Args {
    /// Path to the ROM file to load
    #[arg(value_name = "ROM")]
    rom_path: String,
}

fn main() {
    simple_logger::init_with_env().unwrap();
    let args = Args::parse();
    let mut key_map = HashMap::new();
    key_map.insert(Keycode::Down, joypad::JoypadButton::DOWN);
    key_map.insert(Keycode::Up, joypad::JoypadButton::UP);
    key_map.insert(Keycode::Right, joypad::JoypadButton::RIGHT);
    key_map.insert(Keycode::Left, joypad::JoypadButton::LEFT);
    key_map.insert(Keycode::Space, joypad::JoypadButton::SELECT);
    key_map.insert(Keycode::Return, joypad::JoypadButton::START);
    key_map.insert(Keycode::Z, joypad::JoypadButton::BUTTON_A);
    key_map.insert(Keycode::X, joypad::JoypadButton::BUTTON_B);
    // init sdl2
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(
            "NES Emulator",
            (VISIBLE_WIDTH * 3) as u32,
            (VISIBLE_HEIGHT * 3) as u32,
        )
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(
            PixelFormatEnum::RGB24,
            VISIBLE_WIDTH as u32,
            VISIBLE_HEIGHT as u32,
        )
        .unwrap();
    let mut visible_buffer = [0u8; VISIBLE_WIDTH * VISIBLE_HEIGHT * 3];

    let desired_spec = AudioSpecDesired {
        freq: Some(44100),
        channels: Some(1),
        samples: Some(1024),
    };
    let audio_device: AudioQueue<f32> = sdl_context
        .audio()
        .unwrap()
        .open_queue(None, &desired_spec)
        .unwrap();
    audio_device.resume();

    //load the game
    let path = std::path::absolute(&args.rom_path).expect("Failed to get absolute path");
    let bytes: Vec<u8> = std::fs::read(&path).expect("Failed to load ROM");
    let mut rom = Rom::new(&bytes, path).unwrap();

    // Load save file if it exists
    if let Err(e) = rom.load_prg_ram() {
        eprintln!("Warning: Failed to load save file: {}", e);
    }

    let mut next_frame_target = Instant::now();

    // run the game cycle
    let bus = Bus::new(
        rom,
        audio_device,
        move |ppu: &PPU, joypad: &mut joypad::Joypad| {
            // Copy visible region from PPU frame buffer, applying overscan
            for y in 0..VISIBLE_HEIGHT {
                let src_y = y + OVERSCAN_TOP;
                let src_offset = (src_y * 256 + OVERSCAN_LEFT) * 3;
                let dst_offset = y * VISIBLE_WIDTH * 3;

                visible_buffer[dst_offset..dst_offset + VISIBLE_WIDTH * 3]
                    .copy_from_slice(&ppu.frame_buffer[src_offset..src_offset + VISIBLE_WIDTH * 3]);
            }

            texture
                .update(None, &visible_buffer[..], VISIBLE_WIDTH * 3)
                .unwrap();

            canvas.copy(&texture, None, None).unwrap();

            canvas.present();
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => {
                        sdl2::mixer::close_audio();
                        return true;
                    }
                    Event::KeyDown { keycode, .. } => {
                        if let Some(&key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad.set_button_status(key, true);
                        }
                    }
                    Event::KeyUp { keycode, .. } => {
                        if let Some(&key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad.set_button_status(key, false);
                        }
                    }
                    _ => { /* do nothing */ }
                }
            }
            next_frame_target += FRAME_DURATION;

            let now = Instant::now();

            if now < next_frame_target {
                std::thread::sleep(next_frame_target - now);
            } else {
                if now - next_frame_target > FRAME_DURATION * 3 {
                    next_frame_target = now;
                }
            }
            false
        },
    );

    let mut cpu = CPU::new(bus);

    cpu.reset();
    cpu.run_with_cb(|cpu| {
        log::debug!("{}", nes_emu::trace::trace(cpu));
    });

    // Save PRG-RAM on exit
    if let Err(e) = cpu.save_prg_ram() {
        log::warn!("Warning: Failed to save PRG-RAM: {}", e);
    }
}
