pub mod bus;
pub mod cartridge;
pub mod cpu;
pub mod opcodes;
pub mod ppu;
pub mod trace;
pub mod utils;

pub trait Mem {
    fn read_u8(&mut self, addr: u16) -> u8;
    fn write_u8(&mut self, addr: u16, val: u8);

    fn read_u16(&mut self, addr: u16) -> u16 {
        u16::from_le_bytes([self.read_u8(addr), self.read_u8(addr.wrapping_add(1))])
    }
    fn write_u16(&mut self, addr: u16, val: u16) {
        let bytes = val.to_le_bytes();
        self.write_u8(addr, bytes[0]);
        self.write_u8(addr.wrapping_add(1), bytes[1]);
    }
}
