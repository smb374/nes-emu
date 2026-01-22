use crate::{Mem, cartridge::Rom};

pub struct Bus {
    vram: [u8; 0x800],
    rom: Option<Rom>,
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            vram: [0u8; 0x800],
            rom: None,
        }
    }
}

impl Bus {
    pub fn insert(&mut self, rom: Rom) {
        self.rom = Some(rom);
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        let Some(rom) = self.rom.as_ref() else {
            panic!("Must insert a rom before reading rom")
        };
        addr -= 0x8000;
        if rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            //mirror if needed
            addr = addr % 0x4000;
        }
        rom.prg_rom[addr as usize]
    }
}

const RAM_BASE: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

impl Mem for Bus {
    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            RAM_BASE..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0x7FF;
                self.vram[mirror_down_addr as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                // todo!("PPU is not supported yet")
                0
            }
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => {
                // println!("Ignoring mem access at {}", addr);
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
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                todo!("PPU is not supported yet");
            }
            0x8000..=0xFFFF => {
                panic!("Attempt to write to Cartridge ROM space")
            }
            _ => {
                // println!("Ignoring mem write-access at {}", addr);
            }
        }
    }
}
