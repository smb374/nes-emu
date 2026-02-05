use sdl2::audio::AudioQueue;

use crate::{
    Mem,
    apu::{APU, TimedChannel},
    cartridge::Rom,
    joypad::Joypad,
    ppu::PPU,
};

//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       | SRAM          |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       | I/O Registers |
// | $2000-$2007   |       |               |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       | RAM           |
// |_ _ _ _ _ _ _ _| $0200 |               |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|

const RAM_BASE: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
// const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

#[allow(unused)]
pub struct Bus<'call> {
    vram: [u8; 0x800],
    rom: Rom,
    pub ppu: PPU,
    apu: APU,

    oam_dma: bool,
    oam_dma_remain: u16,

    cycles: usize,
    cycles_acc: usize,
    joypad1: Joypad,
    joypad2: Joypad,
    cb: Box<dyn FnMut(&PPU, &mut Joypad) -> bool + 'call>,
}

impl<'call> Bus<'call> {
    pub fn new<F>(rom: Rom, audio_queue: AudioQueue<f32>, f: F) -> Self
    where
        F: FnMut(&PPU, &mut Joypad) -> bool + 'call,
    {
        let ppu = PPU::new();
        let apu = APU::new(audio_queue);
        Self {
            vram: [0u8; 0x800],
            rom,
            ppu,
            apu,
            oam_dma: false,
            oam_dma_remain: 0,

            joypad1: Joypad::new(),
            joypad2: Joypad::new(),
            cycles: 0,
            cycles_acc: 0,
            cb: Box::new(f),
        }
    }

    pub fn tick(&mut self, cycles: u16) -> (bool, bool) {
        self.cycles += cycles as usize;

        if self.oam_dma_remain > 0 {
            self.oam_dma_remain = self.oam_dma_remain.saturating_sub(cycles);
            if self.oam_dma_remain == 0 {
                self.oam_dma = false;
            }
        }
        let stall = self.apu.tick(&mut self.rom, cycles, self.oam_dma);
        for _ in 0..cycles {
            let frame_before = self.ppu.frames;
            self.ppu.tick(&mut self.rom, 3);
            let frame_after = self.ppu.frames;

            if frame_before != frame_after {
                if (self.cb)(&self.ppu, &mut self.joypad1) {
                    return (false, true);
                }
            }
        }

        if stall != 0 {
            self.tick(stall as u16)
        } else {
            let mapper_irq = self.rom.irq_sig;
            self.rom.irq_sig = false;
            let apu_irq = self.apu.irq_sig;
            self.apu.irq_sig = false;
            (mapper_irq || apu_irq, false)
        }
    }

    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        self.ppu.nmi_interrupt.take()
    }

    pub fn save_prg_ram(&self) -> Result<(), String> {
        self.rom.save_prg_ram()
    }
}

impl<'call> Mem for Bus<'call> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        match addr {
            RAM_BASE..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0x7FF;
                self.vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                // panic!("Attempt to read from write-only PPU address {:x}", addr);
                0
            }
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(&mut self.rom),

            0x4000..=0x4014 => {
                // Open Bus
                0
            }

            0x4015 => self.apu.read_status(),

            0x4016 => self.joypad1.read(),
            0x4017 => 0x40, // $4017 is write-only, return open bus

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0x2007;
                self.read_u8(mirror_down_addr)
            }
            0x6000..=0xFFFF => self.rom.read_prg(addr),

            _ => {
                // eprintln!("Ignoring mem access at {}", addr);
                0
            }
        }
    }

    fn write_u8(&mut self, addr: u16, data: u8) {
        match addr {
            RAM_BASE..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0x7FF;
                self.vram[mirror_down_addr as usize] = data;
            }
            0x2000 => {
                self.ppu.write_to_ctrl(data);
            }
            0x2001 => {
                self.ppu.write_to_mask(data);
            }

            0x2002 => {}

            0x2003 => {
                self.ppu.write_to_oam_addr(data);
            }
            0x2004 => {
                self.ppu.write_to_oam_data(data);
            }
            0x2005 => {
                self.ppu.write_to_scroll(data);
            }

            0x2006 => {
                self.ppu.write_to_ppu_addr(&mut self.rom, data);
            }
            0x2007 => {
                self.ppu.write_data(&mut self.rom, data);
            }

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0x2007;
                self.write_u8(mirror_down_addr, data);
            }

            0x4000 => self.apu.pulse1.update_volume(data),
            0x4004 => self.apu.pulse2.update_volume(data),

            0x4001 => self.apu.pulse1.update_sweep(data),
            0x4005 => self.apu.pulse2.update_sweep(data),

            0x4002 => self.apu.pulse1.update_period_lo(data),
            0x4006 => self.apu.pulse2.update_period_lo(data),

            0x4003 => self.apu.pulse1.update_period_hi(data),
            0x4007 => self.apu.pulse2.update_period_hi(data),

            0x4008 => self.apu.triag.update_counter(data),
            0x400A => self.apu.triag.update_period_lo(data),
            0x400B => self.apu.triag.update_period_hi(data),

            0x400C => self.apu.noise.update_volume(data),
            0x400E => self.apu.noise.update_period_lo(data),
            0x400F => self.apu.noise.update_period_hi(data),

            0x4010 => self.apu.dmc.update_control(data),
            0x4011 => self.apu.dmc.update_direct_load(data),
            0x4012 => self.apu.dmc.update_sample_address(data),
            0x4013 => self.apu.dmc.update_sample_length(data),

            // https://wiki.nesdev.com/w/index.php/PPU_programmer_reference#OAM_DMA_.28.244014.29_.3E_write
            0x4014 => {
                let hi: u16 = (data as u16) << 8;
                let add_cycles: u16 = if self.cycles % 2 == 1 { 2 } else { 1 };
                self.oam_dma = true;
                self.oam_dma_remain = 512 + add_cycles;

                // Halt + optional alignment cycles
                self.tick(add_cycles);

                // 256 get/put pairs - write happens during the put cycle
                for i in 0..256u16 {
                    let byte = self.read_u8(hi + i); // Get cycle (read from memory)
                    self.tick(1);
                    self.ppu.write_to_oam_data(byte); // Put cycle (write to $2004)
                    self.tick(1);
                }
            }

            0x4015 => self.apu.write_status(data),

            0x4016 => self.joypad1.write(data),

            0x4017 => self.apu.write_frame_counter(data),

            0x6000..=0xFFFF => self.rom.write_prg(addr, data),

            _ => {
                // eprintln!("Ignoring mem write-access at {}", addr);
            }
        }
    }
}
