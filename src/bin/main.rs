use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

use nes_emu::{apu::APU, bus::Bus, cartridge::Rom, cpu::CPU, joypad, ppu::PPU};
use sdl2::{
    audio::{AudioQueue, AudioSpecDesired},
    event::Event,
    keyboard::Keycode,
    pixels::PixelFormatEnum,
};

const FRAME_DURATION: Duration = Duration::from_micros(16639);

fn main() {
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
        .window("Tile viewer", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
        .unwrap();

    let desired_spec = AudioSpecDesired {
        freq: Some(44100),
        channels: Some(1),
        samples: Some(2048),
    };
    let audio_device: AudioQueue<f32> = sdl_context
        .audio()
        .unwrap()
        .open_queue(None, &desired_spec)
        .unwrap();
    audio_device.resume();

    //load the game
    let bytes: Vec<u8> = std::fs::read("zelda.nes").unwrap();
    let rom = Rom::new(&bytes).unwrap();

    let mut next_frame_target = Instant::now();

    // run the game cycle
    let bus = Bus::new(
        rom,
        move |ppu: &PPU, apu: &mut APU, joypad: &mut joypad::Joypad| {
            // Copy the frame buffer from PPU to texture
            texture
                .update(None, &ppu.frame_buffer[..], 256 * 3)
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
                        std::process::exit(0);
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
            audio_device.queue_audio(&apu.sample_buffer[..]).unwrap();
            apu.sample_buffer.clear();
            next_frame_target += FRAME_DURATION;

            let max_queue_size = 44100 * 4 / 20;
            while audio_device.size() > max_queue_size {
                std::thread::sleep(Duration::from_micros(100));
            }

            let now = Instant::now();

            if now < next_frame_target {
                std::thread::sleep(next_frame_target - now);
            } else {
                if now - next_frame_target > FRAME_DURATION * 3 {
                    next_frame_target = now;
                }
            }
        },
    );

    let mut cpu = CPU::new(bus);

    cpu.reset();
    cpu.run_with_cb(|_cpu| {
        // println!("{}", nes_emu::trace::trace(_cpu));
    });
}
