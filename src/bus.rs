use std::{cell::RefCell, rc::Rc};

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
    rom: Rc<RefCell<Rom>>,
    ppu: PPU,
    apu: APU,

    cycles: usize,
    joypad1: Joypad,
    joypad2: Joypad,
    cb: Box<dyn FnMut(&PPU, &mut APU, &mut Joypad) + 'call>,
}

impl<'call> Bus<'call> {
    pub fn new<F>(rom: Rom, f: F) -> Self
    where
        F: FnMut(&PPU, &mut APU, &mut Joypad) + 'call,
    {
        let rom = Rc::new(RefCell::new(rom));
        let ppu = PPU::new(Rc::clone(&rom));
        Self {
            vram: [0u8; 0x800],
            rom,
            ppu,
            apu: APU::default(),

            joypad1: Joypad::new(),
            joypad2: Joypad::new(),
            cycles: 0,
            cb: Box::new(f),
        }
    }

    pub fn tick(&mut self, cycles: u16) -> bool {
        self.cycles += cycles as usize;

        self.apu.tick(cycles);

        let nmi_before = self.ppu.nmi_interrupt.is_some();
        self.ppu.tick(cycles * 3);
        let nmi_after = self.ppu.nmi_interrupt.is_some();

        if !nmi_before && nmi_after {
            (self.cb)(&self.ppu, &mut self.apu, &mut self.joypad1);
        }

        let irq = self.rom.borrow().irq_sig;
        self.rom.borrow_mut().irq_sig = false;
        irq
    }

    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        self.ppu.nmi_interrupt.take()
    }
}

impl<'call> Mem for Bus<'call> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        match addr {
            RAM_BASE..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0x7FF;
                self.vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address {:x}", addr);
                // 0
            }
            0x2001 => self.ppu.read_mask(), // Ice Climber reads this.
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),

            0x4000..=0x4014 => {
                // Open Bus
                0
            }

            0x4015 => self.apu.status.bits(),

            0x4016 => self.joypad1.read(),
            0x4017 => self.apu.frame_counter.bits(),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0x2007;
                self.read_u8(mirror_down_addr)
            }
            0x6000..=0xFFFF => self.rom.borrow_mut().read_prg(addr),

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

            0x2002 => panic!("attempt to write to PPU status register"),

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
                self.ppu.write_to_ppu_addr(data);
            }
            0x2007 => {
                self.ppu.write_data(data);
            }

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0x2007;
                self.write_u8(mirror_down_addr, data);
            }

            0x4000 => {
                self.apu.pulse1_reg.envelope = data;
                self.apu.pulse1.update_duty(data);
            }
            0x4004 => {
                self.apu.pulse2_reg.envelope = data;
                self.apu.pulse2.update_duty(data);
            }

            0x4001 => self.apu.pulse1_reg.sweep = data,
            0x4005 => self.apu.pulse2_reg.sweep = data,

            0x4002 => self.apu.pulse1.update_period_lo(data),
            0x4006 => self.apu.pulse2.update_period_hi(data),

            0x4003 => self.apu.pulse1.update_period_lo(data),
            0x4007 => self.apu.pulse2.update_period_hi(data),

            0x4008 => self.apu.triag_reg.counter = data,
            0x400A => {} // Update triag timer lo
            0x400B => {} // Update triag timer hi

            0x400C => self.apu.noise_reg.envelope = data,
            0x400E => self.apu.noise_reg.noise = data,
            0x400F => self.apu.noise_reg.lcload = data,

            0x4000..=0x4013 => {}

            // https://wiki.nesdev.com/w/index.php/PPU_programmer_reference#OAM_DMA_.28.244014.29_.3E_write
            0x4014 => {
                let mut buffer: [u8; 256] = [0; 256];
                let hi: u16 = (data as u16) << 8;
                for i in 0..256u16 {
                    buffer[i as usize] = self.read_u8(hi + i);
                }

                self.ppu.write_oam_dma(&buffer);
            }

            0x4015 => self.apu.status.update(data),

            0x4016 => self.joypad1.write(data),

            0x4017 => self.apu.frame_counter.update(data),

            0x6000..=0xFFFF => self.rom.borrow_mut().write_prg(addr, data),

            _ => {
                // eprintln!("Ignoring mem write-access at {}", addr);
            }
        }
    }
}
