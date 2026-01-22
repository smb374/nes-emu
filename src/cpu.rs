use bitflags::bitflags;

use crate::{Mem, bus::Bus, cartridge::Rom, utils};

use AddressMode::*;
use OpFamily::*;

const STACK_BASE: u16 = 0x100;
const STACK_RESET: u8 = 0xFD;

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct CpuFlags: u8 {
        const C = 0b0000_0001;
        const Z = 0b0000_0010;
        const I = 0b0000_0100;
        const D = 0b0000_1000;
        const B = 0b0001_0000;
        const B2 = 0b0010_0000;
        const V = 0b0100_0000;
        const N = 0b1000_0000;
    }
}

pub struct CPU {
    pub pc: u16,
    pub reg_a: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub sp: u8,
    pub status: CpuFlags,
    bus: Bus,
    pub jmp: Option<u16>,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            pc: 0,
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            sp: STACK_RESET,
            status: CpuFlags::B2 | CpuFlags::I,
            bus: Bus::default(),
            jmp: None,
        }
    }

    pub fn with_bus(bus: Bus) -> Self {
        CPU {
            pc: 0,
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            sp: STACK_RESET,
            status: CpuFlags::B2 | CpuFlags::I,
            bus,
            jmp: None,
        }
    }

    pub fn load(&mut self, rom: Rom) {
        self.bus.insert(rom);
    }

    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.sp = STACK_RESET;
        self.status = CpuFlags::B2 | CpuFlags::I;

        self.pc = self.read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, rom: Rom) {
        self.load(rom);
        self.reset();
        self.run();
    }

    pub fn run(&mut self) {
        self.run_with_cb(|_| {});
    }

    pub fn print_stack(&self) {
        let start = STACK_BASE + self.sp as u16;
        let end = STACK_BASE + STACK_RESET as u16;
        for i in start..end {
            println!("stack[{:#x}] = 0x{:02x}", i + 1, self.bus.read_u8(i + 1));
        }
    }

    pub fn run_with_cb<F>(&mut self, mut cb: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            cb(self);
            let opcode = self.read_u8(self.pc);
            self.pc += 1;
            let pc_cache = self.pc;
            if let Some(op) = OPS[opcode as usize] {
                match op.family {
                    ADC => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        let car = self.status.contains(CpuFlags::C) as u16;
                        let sum = self.reg_a as u16 + val as u16 + car;

                        self.status.set(CpuFlags::C, sum > 0xFF);

                        let result = (sum & 0xFF) as u8;
                        self.status.set(
                            CpuFlags::V,
                            ((self.reg_a ^ result) & (val ^ result) & 0x80) != 0,
                        );

                        self.reg_a = result;
                        self.update_nz(self.reg_a);
                    }
                    AND => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_a &= self.read_u8(addr);
                        self.update_nz(self.reg_a);
                    }
                    ASL => {
                        if op.mode == Some(ACC) {
                            self.status.set(CpuFlags::C, self.reg_a & 0x80 != 0);
                            self.reg_a <<= 1;
                            self.update_nz(self.reg_a);
                        } else {
                            let addr = self.operand_addr(op.mode).unwrap();
                            let mut val = self.read_u8(addr);
                            self.status.set(CpuFlags::C, val & 0x80 != 0);
                            val <<= 1;
                            self.update_nz(val);
                            self.write_u8(addr, val);
                        }
                    }
                    BCC => self.branch_if(!self.status.contains(CpuFlags::C)),
                    BCS => self.branch_if(self.status.contains(CpuFlags::C)),
                    BEQ => self.branch_if(self.status.contains(CpuFlags::Z)),
                    BIT => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        let res = self.reg_a & val;
                        self.status.set(CpuFlags::Z, res == 0);
                        self.status.set(CpuFlags::V, val & 0x40 != 0);
                        self.status.set(CpuFlags::N, val & 0x80 != 0);
                    }
                    BMI => self.branch_if(self.status.contains(CpuFlags::N)),
                    BNE => self.branch_if(!self.status.contains(CpuFlags::Z)),
                    BPL => self.branch_if(!self.status.contains(CpuFlags::N)),
                    BRK => {
                        let npc = (self.pc + 1).to_le_bytes();
                        self.push_stack(npc[1]);
                        self.push_stack(npc[0]);
                        self.push_stack((self.status | CpuFlags::B | CpuFlags::B2).bits());
                        self.status.insert(CpuFlags::I);
                        self.pc += 1;
                        // self.jmp = Some(self.read_u16(0xFFFE));
                        break;
                    }
                    BVC => self.branch_if(!self.status.contains(CpuFlags::V)),
                    BVS => self.branch_if(self.status.contains(CpuFlags::V)),
                    CLC => self.status &= !CpuFlags::C,
                    CLD => self.status &= !CpuFlags::D,
                    CLI => self.status &= !CpuFlags::I,
                    CLV => self.status &= !CpuFlags::V,
                    CMP => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let (res, car) = self.reg_a.overflowing_sub(self.read_u8(addr));
                        self.status.set(CpuFlags::C, !car);
                        self.update_nz(res);
                    }
                    CPX => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let (res, car) = self.reg_x.overflowing_sub(self.read_u8(addr));
                        self.status.set(CpuFlags::C, !car);
                        self.update_nz(res);
                    }
                    CPY => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let (res, car) = self.reg_y.overflowing_sub(self.read_u8(addr));
                        self.status.set(CpuFlags::C, !car);
                        self.update_nz(res);
                    }
                    DEC => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let res = self.read_u8(addr).wrapping_sub(1);
                        self.write_u8(addr, res);
                        self.update_nz(res);
                    }
                    DEX => {
                        self.reg_x = self.reg_x.wrapping_sub(1);
                        self.update_nz(self.reg_x);
                    }
                    DEY => {
                        self.reg_y = self.reg_y.wrapping_sub(1);
                        self.update_nz(self.reg_y);
                    }
                    EOR => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_a ^= self.read_u8(addr);
                        self.update_nz(self.reg_a);
                    }
                    INC => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let res = self.read_u8(addr).wrapping_add(1);
                        self.write_u8(addr, res);
                        self.update_nz(res);
                    }
                    INX => {
                        self.reg_x = self.reg_x.wrapping_add(1);
                        self.update_nz(self.reg_x);
                    }
                    INY => {
                        self.reg_y = self.reg_y.wrapping_add(1);
                        self.update_nz(self.reg_y);
                    }
                    JMP => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.jmp = Some(addr);
                    }
                    JSR => {
                        let npc = (self.pc + 1).to_le_bytes();
                        self.push_stack(npc[1]);
                        self.push_stack(npc[0]);
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.jmp = Some(addr);
                    }
                    LDA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_a = self.read_u8(addr);
                        self.update_nz(self.reg_a);
                    }
                    LDX => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_x = self.read_u8(addr);
                        self.update_nz(self.reg_x);
                    }
                    LDY => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_y = self.read_u8(addr);
                        self.update_nz(self.reg_y);
                    }
                    LSR => {
                        if op.mode == Some(ACC) {
                            self.status.set(CpuFlags::C, self.reg_a & 0x01 != 0);
                            self.reg_a >>= 1;
                            self.update_nz(self.reg_a);
                        } else {
                            let addr = self.operand_addr(op.mode).unwrap();
                            let mut val = self.read_u8(addr);
                            self.status.set(CpuFlags::C, val & 0x01 != 0);
                            val >>= 1;
                            self.update_nz(val);
                            self.write_u8(addr, val);
                        }
                    }
                    NOP => {}
                    ORA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_a |= self.read_u8(addr);
                        self.update_nz(self.reg_a);
                    }
                    PHA => self.push_stack(self.reg_a),
                    PHP => self.push_stack((self.status | CpuFlags::B | CpuFlags::B2).bits()),
                    PLA => {
                        self.reg_a = self.pop_stack();
                        self.update_nz(self.reg_a);
                    }
                    PLP => {
                        let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                        status &= !CpuFlags::B;
                        status |= CpuFlags::B2;
                        self.status = status;
                    }
                    ROL => {
                        let ocar = self.status.contains(CpuFlags::C);
                        if op.mode == Some(ACC) {
                            let (res, car) = utils::rol(self.reg_a, ocar);
                            self.status.set(CpuFlags::C, car);
                            self.reg_a = res;
                            self.update_nz(self.reg_a);
                        } else {
                            let addr = self.operand_addr(op.mode).unwrap();
                            let val = self.read_u8(addr);
                            let (res, car) = utils::rol(val, ocar);
                            self.status.set(CpuFlags::C, car);
                            self.update_nz(res);
                            self.write_u8(addr, res);
                        }
                    }
                    ROR => {
                        let ocar = self.status.contains(CpuFlags::C);
                        if op.mode == Some(ACC) {
                            let (res, car) = utils::ror(self.reg_a, ocar);
                            self.status.set(CpuFlags::C, car);
                            self.reg_a = res;
                            self.update_nz(self.reg_a);
                        } else {
                            let addr = self.operand_addr(op.mode).unwrap();
                            let val = self.read_u8(addr);
                            let (res, car) = utils::ror(val, ocar);
                            self.status.set(CpuFlags::C, car);
                            self.update_nz(res);
                            self.write_u8(addr, res);
                        }
                    }
                    RTI => {
                        let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                        status &= !CpuFlags::B;
                        status |= CpuFlags::B2;
                        self.status = status;
                        let npc = [self.pop_stack(), self.pop_stack()];
                        self.jmp = Some(u16::from_le_bytes(npc));
                    }
                    RTS => {
                        let npc = [self.pop_stack(), self.pop_stack()];
                        self.jmp = Some(u16::from_le_bytes(npc) + 1);
                    }
                    SBC => {
                        // ADC with inverted operand
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        let car = self.status.contains(CpuFlags::C) as u16;
                        let sum = self.reg_a as u16 + (val ^ 0xFF) as u16 + car;

                        self.status.set(CpuFlags::C, sum > 0xFF);

                        let res = (sum & 0xFF) as u8;
                        self.status.set(
                            CpuFlags::V,
                            ((self.reg_a ^ res) & (self.reg_a ^ val) & 0x80) != 0,
                        );

                        self.reg_a = res;
                        self.update_nz(self.reg_a);
                    }
                    SEC => self.status |= CpuFlags::C,
                    SED => self.status |= CpuFlags::D,
                    SEI => self.status |= CpuFlags::I,
                    STA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.write_u8(addr, self.reg_a);
                    }
                    STX => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.write_u8(addr, self.reg_x);
                    }
                    STY => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.write_u8(addr, self.reg_y);
                    }
                    TAX => {
                        self.reg_x = self.reg_a;
                        self.update_nz(self.reg_x);
                    }
                    TAY => {
                        self.reg_y = self.reg_a;
                        self.update_nz(self.reg_y);
                    }
                    TSX => {
                        self.reg_x = self.sp;
                        self.update_nz(self.reg_x);
                    }
                    TXA => {
                        self.reg_a = self.reg_x;
                        self.update_nz(self.reg_a);
                    }
                    TXS => self.sp = self.reg_x,
                    TYA => {
                        self.reg_a = self.reg_y;
                        self.update_nz(self.reg_a);
                    }
                    // Unofficial
                    AAC => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_a &= self.read_u8(addr);
                        self.update_nz(self.reg_a);
                        self.status
                            .set(CpuFlags::C, self.status.contains(CpuFlags::N));
                    }
                    AAX => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let res = self.reg_a & self.reg_x;
                        self.write_u8(addr, res);
                    }
                    ARR => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        let (res, _) =
                            utils::ror(self.reg_a & val, self.status.contains(CpuFlags::C));
                        self.reg_a = res;
                        self.update_nz(self.reg_a);
                        let chk = (res & 0b0110_0000) >> 5;
                        match chk {
                            0b11 => {
                                self.status |= CpuFlags::C;
                                self.status &= !CpuFlags::V;
                            }
                            0b00 => {
                                self.status &= !(CpuFlags::C | CpuFlags::V);
                            }
                            0b01 => {
                                self.status |= CpuFlags::V;
                                self.status &= !CpuFlags::C;
                            }
                            0b10 => {
                                self.status |= CpuFlags::C | CpuFlags::V;
                            }
                            _ => unreachable!(),
                        }
                    }
                    ASR => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.reg_a & self.read_u8(addr);
                        self.reg_a = val >> 1;
                        self.update_nz(self.reg_a);
                        self.status.set(CpuFlags::C, val & 1 != 0);
                    }
                    ATX => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        self.reg_a &= self.read_u8(addr);
                        self.reg_x = self.reg_a;
                        self.update_nz(self.reg_a);
                    }
                    AXA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let res = self.reg_a & self.reg_x & 7;
                        self.write_u8(addr, res);
                    }
                    AXS => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        self.reg_x &= self.reg_a;
                        let sum = self.reg_x as u16 + (val ^ 0xFF) as u16 + 1;
                        self.status.set(CpuFlags::C, sum > 0xFF);
                        self.reg_x = (sum & 0xFF) as u8;
                        self.update_nz(self.reg_x);
                    }
                    DCP => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let res = self.read_u8(addr).wrapping_sub(1);
                        self.write_u8(addr, res);
                        let (res, car) = self.reg_a.overflowing_sub(res);
                        self.status.set(CpuFlags::C, !car);
                        self.update_nz(res);
                    }
                    DOP => {}
                    ISC => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr).wrapping_add(1);
                        self.write_u8(addr, val);
                        let car = self.status.contains(CpuFlags::C) as u16;
                        let sum = self.reg_a as u16 + (val ^ 0xFF) as u16 + car;

                        self.status.set(CpuFlags::C, sum > 0xFF);

                        let res = (sum & 0xFF) as u8;
                        self.status.set(
                            CpuFlags::V,
                            ((self.reg_a ^ res) & (self.reg_a ^ val) & 0x80) != 0,
                        );

                        self.reg_a = res;
                        self.update_nz(self.reg_a);
                    }
                    KIL => {
                        self.pc -= 1;
                        break;
                    }
                    LAR => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        self.sp &= val;
                        self.reg_a = self.sp;
                        self.reg_x = self.sp;
                        self.update_nz(self.reg_a);
                    }
                    LAX => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        self.reg_a = val;
                        self.reg_x = val;
                        self.update_nz(val);
                    }
                    RLA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        {
                            let val = self.read_u8(addr);
                            let ocar = self.status.contains(CpuFlags::C);
                            let (res, car) = utils::rol(val, ocar);
                            self.status.set(CpuFlags::C, car);
                            self.update_nz(res);
                            self.write_u8(addr, res);
                        }
                        {
                            self.reg_a &= self.read_u8(addr);
                            self.update_nz(self.reg_a);
                        }
                    }
                    RRA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        {
                            let ocar = self.status.contains(CpuFlags::C);
                            let val = self.read_u8(addr);
                            let (res, car) = utils::ror(val, ocar);
                            self.status.set(CpuFlags::C, car);
                            self.update_nz(res);
                            self.write_u8(addr, res);
                        }
                        {
                            let val = self.read_u8(addr);
                            let car = self.status.contains(CpuFlags::C) as u16;
                            let sum = self.reg_a as u16 + val as u16 + car;

                            self.status.set(CpuFlags::C, sum > 0xFF);

                            let result = (sum & 0xFF) as u8;
                            self.status.set(
                                CpuFlags::V,
                                ((self.reg_a ^ result) & (val ^ result) & 0x80) != 0,
                            );

                            self.reg_a = result;
                            self.update_nz(self.reg_a);
                        }
                    }
                    SLO => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        {
                            let mut val = self.read_u8(addr);
                            self.status.set(CpuFlags::C, val & 0x80 != 0);
                            val <<= 1;
                            self.update_nz(val);
                            self.write_u8(addr, val);
                        }
                        {
                            self.reg_a |= self.read_u8(addr);
                            self.update_nz(self.reg_a);
                        }
                    }
                    SRE => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        {
                            let mut val = self.read_u8(addr);
                            self.status.set(CpuFlags::C, val & 0x01 != 0);
                            val >>= 1;
                            self.update_nz(val);
                            self.write_u8(addr, val);
                        }
                        {
                            self.reg_a ^= self.read_u8(addr);
                            self.update_nz(self.reg_a);
                        }
                    }
                    SXA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let hi = ((addr >> 8) as u8).wrapping_add(1);
                        let res = self.reg_x & hi;
                        self.write_u8(addr, res);
                    }
                    SYA => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let hi = ((addr >> 8) as u8).wrapping_add(1);
                        let res = self.reg_y & hi;
                        self.write_u8(addr, res);
                    }
                    TOP => {}
                    XAA => {
                        {
                            self.reg_a = self.reg_x;
                            self.update_nz(self.reg_a);
                        }
                        {
                            let addr = self.operand_addr(op.mode).unwrap();
                            self.reg_a &= self.read_u8(addr);
                            self.update_nz(self.reg_a);
                        }
                    }
                    XAS => {
                        self.sp = self.reg_x & self.reg_a;
                        let addr = self.operand_addr(op.mode).unwrap();
                        let hi = ((addr >> 8) as u8).wrapping_add(1);
                        let res = self.sp & hi;
                        self.write_u8(addr, res);
                    }
                }
                if let Some(pc) = self.jmp.take() {
                    self.pc = pc;
                } else if pc_cache == self.pc {
                    self.pc += op.size as u16 - 1;
                }
            } else {
                panic!("Unknown op: {}", opcode);
            }
        }
    }

    fn push_stack(&mut self, val: u8) {
        self.write_u8(STACK_BASE + self.sp as u16, val);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.read_u8(STACK_BASE + self.sp as u16)
    }

    fn update_nz(&mut self, val: u8) {
        self.status.set(CpuFlags::Z, val == 0);
        self.status.set(CpuFlags::N, val & 0x80 != 0);
    }

    fn branch_if(&mut self, condition: bool) {
        let offset = self.read_u8(self.pc) as i8 as i16;
        if condition {
            self.jmp = Some(self.pc.wrapping_add(1).wrapping_add(offset as u16));
        }
        self.pc += 1;
    }

    #[allow(dead_code)]
    fn page_crossed(a: u16, b: u16) -> bool {
        (a & 0xFF00) != (b & 0xFF00)
    }

    pub fn operand_addr(&mut self, mode: Option<AddressMode>) -> Option<u16> {
        let res = mode.and_then(|m| match m {
            IMM => Some(self.pc),
            ZP => Some(self.read_u8(self.pc) as u16),
            ZPX => Some(self.read_u8(self.pc).wrapping_add(self.reg_x) as u16),
            ZPY => Some(self.read_u8(self.pc).wrapping_add(self.reg_y) as u16),
            ABS => Some(self.read_u16(self.pc)),
            ABSX => Some(self.read_u16(self.pc).wrapping_add(self.reg_x as u16)),
            ABSY => Some(self.read_u16(self.pc).wrapping_add(self.reg_y as u16)),
            IND => {
                let addr = self.read_u16(self.pc);
                // 6502 bug: if address is $xxFF, high byte wraps to $xx00
                if addr & 0xFF == 0xFF {
                    let lo = self.read_u8(addr);
                    let hi = self.read_u8(addr & 0xFF00); // Wrap to same page
                    Some((hi as u16) << 8 | lo as u16)
                } else {
                    Some(self.read_u16(addr))
                }
            }
            INDX => {
                let ptr = self.read_u8(self.pc).wrapping_add(self.reg_x);
                let lo = self.read_u8(ptr as u16);
                let hi = self.read_u8(ptr.wrapping_add(1) as u16);
                Some((hi as u16) << 8 | lo as u16)
            }
            INDY => {
                let ptr = self.read_u8(self.pc);
                let lo = self.read_u8(ptr as u16);
                let hi = self.read_u8(ptr.wrapping_add(1) as u16);
                Some(((hi as u16) << 8 | lo as u16).wrapping_add(self.reg_y as u16))
            }
            _ => None,
        });

        res
    }
}

impl Mem for CPU {
    fn read_u8(&self, addr: u16) -> u8 {
        self.bus.read_u8(addr)
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        self.bus.write_u8(addr, val);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Op {
    pub code: u8,
    pub family: OpFamily,
    pub mode: Option<AddressMode>,
    pub size: u8,
    pub cycles: u8,
    pub mnemonic: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpFamily {
    // Official
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    // Unofficial
    AAC,
    AAX,
    ARR,
    ASR,
    ATX,
    AXA,
    AXS,
    DCP,
    DOP,
    ISC,
    KIL,
    LAR,
    LAX,
    RLA,
    RRA,
    SLO,
    SRE,
    SXA,
    SYA,
    TOP,
    XAA,
    XAS,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressMode {
    IMM,  // Immediate
    ZP,   // Zero Page
    ZPX,  // Zero Page, X
    ZPY,  // Zero Page, Y
    ABS,  // Absolute
    ABSX, // Absolute, X
    ABSY, // Absolute, Y
    INDX, // (Indirect, X)
    INDY, // (Indirect), Y
    ACC,  // Accumulator
    REL,  // Relative (for branches)
    IND,  // Indirect (for JMP)
}

macro_rules! op {
    ($code:expr, $family:ident, $mode:ident, $size:expr, $cycles:expr, $mnemonic:expr) => {
        Some(Op {
            code: $code,
            family: $family,
            mode: Some($mode),
            size: $size,
            cycles: $cycles,
            mnemonic: $mnemonic,
        })
    };
    ($code:expr, $family:ident, $size:expr, $cycles:expr, $mnemonic:expr) => {
        Some(Op {
            code: $code,
            family: $family,
            mode: None,
            size: $size,
            cycles: $cycles,
            mnemonic: $mnemonic,
        })
    };
}

pub const OPS: [Option<Op>; 256] = {
    let mut ops: [Option<Op>; 256] = [None; 256];

    // Official OP codes
    // ADC - Add with Carry
    ops[0x69] = op!(0x69, ADC, IMM, 2, 2, "ADC");
    ops[0x65] = op!(0x65, ADC, ZP, 2, 3, "ADC");
    ops[0x75] = op!(0x75, ADC, ZPX, 2, 4, "ADC");
    ops[0x6D] = op!(0x6D, ADC, ABS, 3, 4, "ADC");
    ops[0x7D] = op!(0x7D, ADC, ABSX, 3, 4, "ADC"); // +1 if page crossed
    ops[0x79] = op!(0x79, ADC, ABSY, 3, 4, "ADC"); // +1 if page crossed
    ops[0x61] = op!(0x61, ADC, INDX, 2, 6, "ADC");
    ops[0x71] = op!(0x71, ADC, INDY, 2, 5, "ADC"); // +1 if page crossed

    // AND - Bitwise AND
    ops[0x29] = op!(0x29, AND, IMM, 2, 2, "AND");
    ops[0x25] = op!(0x25, AND, ZP, 2, 3, "AND");
    ops[0x35] = op!(0x35, AND, ZPX, 2, 4, "AND");
    ops[0x2D] = op!(0x2D, AND, ABS, 3, 4, "AND");
    ops[0x3D] = op!(0x3D, AND, ABSX, 3, 4, "AND"); // +1 if page crossed
    ops[0x39] = op!(0x39, AND, ABSY, 3, 4, "AND"); // +1 if page crossed
    ops[0x21] = op!(0x21, AND, INDX, 2, 6, "AND");
    ops[0x31] = op!(0x31, AND, INDY, 2, 5, "AND"); // +1 if page crossed

    // ASL - Arithmetic Shift Left
    ops[0x0A] = op!(0x0A, ASL, ACC, 1, 2, "ASL");
    ops[0x06] = op!(0x06, ASL, ZP, 2, 5, "ASL");
    ops[0x16] = op!(0x16, ASL, ZPX, 2, 6, "ASL");
    ops[0x0E] = op!(0x0E, ASL, ABS, 3, 6, "ASL");
    ops[0x1E] = op!(0x1E, ASL, ABSX, 3, 7, "ASL");

    // BCC - Branch if Carry Clear
    ops[0x90] = op!(0x90, BCC, REL, 2, 2, "BCC"); // +1 if branch taken, +1 if page crossed

    // BCS - Branch if Carry Set
    ops[0xB0] = op!(0xB0, BCS, REL, 2, 2, "BCS"); // +1 if branch taken, +1 if page crossed

    // BEQ - Branch if Equal (Zero Set)
    ops[0xF0] = op!(0xF0, BEQ, REL, 2, 2, "BEQ"); // +1 if branch taken, +1 if page crossed

    // BIT - Bit Test
    ops[0x24] = op!(0x24, BIT, ZP, 2, 3, "BIT");
    ops[0x2C] = op!(0x2C, BIT, ABS, 3, 4, "BIT");

    // BMI - Branch if Minus (Negative Set)
    ops[0x30] = op!(0x30, BMI, REL, 2, 2, "BMI"); // +1 if branch taken, +1 if page crossed

    // BNE - Branch if Not Equal (Zero Clear)
    ops[0xD0] = op!(0xD0, BNE, REL, 2, 2, "BNE"); // +1 if branch taken, +1 if page crossed

    // BPL - Branch if Plus (Negative Clear)
    ops[0x10] = op!(0x10, BPL, REL, 2, 2, "BPL"); // +1 if branch taken, +1 if page crossed

    // BRK - Break (Software IRQ)
    ops[0x00] = op!(0x00, BRK, 2, 7, "BRK");

    // BVC - Branch if Overflow Clear
    ops[0x50] = op!(0x50, BVC, REL, 2, 2, "BVC"); // +1 if branch taken, +1 if page crossed

    // BVS - Branch if Overflow Set
    ops[0x70] = op!(0x70, BVS, REL, 2, 2, "BVS"); // +1 if branch taken, +1 if page crossed

    // CLC - Clear Carry
    ops[0x18] = op!(0x18, CLC, 1, 2, "CLC");

    // CLD - Clear Decimal
    ops[0xD8] = op!(0xD8, CLD, 1, 2, "CLD");

    // CLI - Clear Interrupt Disable
    ops[0x58] = op!(0x58, CLI, 1, 2, "CLI");

    // CLV - Clear Overflow
    ops[0xB8] = op!(0xB8, CLV, 1, 2, "CLV");

    // CMP - Compare A
    ops[0xC9] = op!(0xC9, CMP, IMM, 2, 2, "CMP");
    ops[0xC5] = op!(0xC5, CMP, ZP, 2, 3, "CMP");
    ops[0xD5] = op!(0xD5, CMP, ZPX, 2, 4, "CMP");
    ops[0xCD] = op!(0xCD, CMP, ABS, 3, 4, "CMP");
    ops[0xDD] = op!(0xDD, CMP, ABSX, 3, 4, "CMP"); // +1 if page crossed
    ops[0xD9] = op!(0xD9, CMP, ABSY, 3, 4, "CMP"); // +1 if page crossed
    ops[0xC1] = op!(0xC1, CMP, INDX, 2, 6, "CMP");
    ops[0xD1] = op!(0xD1, CMP, INDY, 2, 5, "CMP"); // +1 if page crossed

    // CPX - Compare X
    ops[0xE0] = op!(0xE0, CPX, IMM, 2, 2, "CPX");
    ops[0xE4] = op!(0xE4, CPX, ZP, 2, 3, "CPX");
    ops[0xEC] = op!(0xEC, CPX, ABS, 3, 4, "CPX");

    // CPY - Compare Y
    ops[0xC0] = op!(0xC0, CPY, IMM, 2, 2, "CPY");
    ops[0xC4] = op!(0xC4, CPY, ZP, 2, 3, "CPY");
    ops[0xCC] = op!(0xCC, CPY, ABS, 3, 4, "CPY");

    // DEC - Decrement Memory
    ops[0xC6] = op!(0xC6, DEC, ZP, 2, 5, "DEC");
    ops[0xD6] = op!(0xD6, DEC, ZPX, 2, 6, "DEC");
    ops[0xCE] = op!(0xCE, DEC, ABS, 3, 6, "DEC");
    ops[0xDE] = op!(0xDE, DEC, ABSX, 3, 7, "DEC");

    // DEX - Decrement X
    ops[0xCA] = op!(0xCA, DEX, 1, 2, "DEX");

    // DEY - Decrement Y
    ops[0x88] = op!(0x88, DEY, 1, 2, "DEY");

    // EOR - Bitwise Exclusive OR
    ops[0x49] = op!(0x49, EOR, IMM, 2, 2, "EOR");
    ops[0x45] = op!(0x45, EOR, ZP, 2, 3, "EOR");
    ops[0x55] = op!(0x55, EOR, ZPX, 2, 4, "EOR");
    ops[0x4D] = op!(0x4D, EOR, ABS, 3, 4, "EOR");
    ops[0x5D] = op!(0x5D, EOR, ABSX, 3, 4, "EOR"); // +1 if page crossed
    ops[0x59] = op!(0x59, EOR, ABSY, 3, 4, "EOR"); // +1 if page crossed
    ops[0x41] = op!(0x41, EOR, INDX, 2, 6, "EOR");
    ops[0x51] = op!(0x51, EOR, INDY, 2, 5, "EOR"); // +1 if page crossed

    // INC - Increment Memory
    ops[0xE6] = op!(0xE6, INC, ZP, 2, 5, "INC");
    ops[0xF6] = op!(0xF6, INC, ZPX, 2, 6, "INC");
    ops[0xEE] = op!(0xEE, INC, ABS, 3, 6, "INC");
    ops[0xFE] = op!(0xFE, INC, ABSX, 3, 7, "INC");

    // INX - Increment X
    ops[0xE8] = op!(0xE8, INX, 1, 2, "INX");

    // INY - Increment Y
    ops[0xC8] = op!(0xC8, INY, 1, 2, "INY");

    // JMP - Jump
    ops[0x4C] = op!(0x4C, JMP, ABS, 3, 3, "JMP");
    ops[0x6C] = op!(0x6C, JMP, IND, 3, 5, "JMP");

    // JSR - Jump to Subroutine
    ops[0x20] = op!(0x20, JSR, ABS, 3, 6, "JSR");

    // LDA - Load A
    ops[0xA9] = op!(0xA9, LDA, IMM, 2, 2, "LDA");
    ops[0xA5] = op!(0xA5, LDA, ZP, 2, 3, "LDA");
    ops[0xB5] = op!(0xB5, LDA, ZPX, 2, 4, "LDA");
    ops[0xAD] = op!(0xAD, LDA, ABS, 3, 4, "LDA");
    ops[0xBD] = op!(0xBD, LDA, ABSX, 3, 4, "LDA"); // +1 if page crossed
    ops[0xB9] = op!(0xB9, LDA, ABSY, 3, 4, "LDA"); // +1 if page crossed
    ops[0xA1] = op!(0xA1, LDA, INDX, 2, 6, "LDA");
    ops[0xB1] = op!(0xB1, LDA, INDY, 2, 5, "LDA"); // +1 if page crossed

    // LDX - Load X
    ops[0xA2] = op!(0xA2, LDX, IMM, 2, 2, "LDX");
    ops[0xA6] = op!(0xA6, LDX, ZP, 2, 3, "LDX");
    ops[0xB6] = op!(0xB6, LDX, ZPY, 2, 4, "LDX");
    ops[0xAE] = op!(0xAE, LDX, ABS, 3, 4, "LDX");
    ops[0xBE] = op!(0xBE, LDX, ABSY, 3, 4, "LDX"); // +1 if page crossed

    // LDY - Load Y
    ops[0xA0] = op!(0xA0, LDY, IMM, 2, 2, "LDY");
    ops[0xA4] = op!(0xA4, LDY, ZP, 2, 3, "LDY");
    ops[0xB4] = op!(0xB4, LDY, ZPX, 2, 4, "LDY");
    ops[0xAC] = op!(0xAC, LDY, ABS, 3, 4, "LDY");
    ops[0xBC] = op!(0xBC, LDY, ABSX, 3, 4, "LDY"); // +1 if page crossed

    // LSR - Logical Shift Right
    ops[0x4A] = op!(0x4A, LSR, ACC, 1, 2, "LSR");
    ops[0x46] = op!(0x46, LSR, ZP, 2, 5, "LSR");
    ops[0x56] = op!(0x56, LSR, ZPX, 2, 6, "LSR");
    ops[0x4E] = op!(0x4E, LSR, ABS, 3, 6, "LSR");
    ops[0x5E] = op!(0x5E, LSR, ABSX, 3, 7, "LSR");

    // NOP - No Operation
    ops[0xEA] = op!(0xEA, NOP, 1, 2, "NOP");

    // ORA - Bitwise OR
    ops[0x09] = op!(0x09, ORA, IMM, 2, 2, "ORA");
    ops[0x05] = op!(0x05, ORA, ZP, 2, 3, "ORA");
    ops[0x15] = op!(0x15, ORA, ZPX, 2, 4, "ORA");
    ops[0x0D] = op!(0x0D, ORA, ABS, 3, 4, "ORA");
    ops[0x1D] = op!(0x1D, ORA, ABSX, 3, 4, "ORA"); // +1 if page crossed
    ops[0x19] = op!(0x19, ORA, ABSY, 3, 4, "ORA"); // +1 if page crossed
    ops[0x01] = op!(0x01, ORA, INDX, 2, 6, "ORA");
    ops[0x11] = op!(0x11, ORA, INDY, 2, 5, "ORA"); // +1 if page crossed

    // PHA - Push A
    ops[0x48] = op!(0x48, PHA, 1, 3, "PHA");

    // PHP - Push Processor Status
    ops[0x08] = op!(0x08, PHP, 1, 3, "PHP");

    // PLA - Pull A
    ops[0x68] = op!(0x68, PLA, 1, 4, "PLA");

    // PLP - Pull Processor Status
    ops[0x28] = op!(0x28, PLP, 1, 4, "PLP");

    // ROL - Rotate Left
    ops[0x2A] = op!(0x2A, ROL, ACC, 1, 2, "ROL");
    ops[0x26] = op!(0x26, ROL, ZP, 2, 5, "ROL");
    ops[0x36] = op!(0x36, ROL, ZPX, 2, 6, "ROL");
    ops[0x2E] = op!(0x2E, ROL, ABS, 3, 6, "ROL");
    ops[0x3E] = op!(0x3E, ROL, ABSX, 3, 7, "ROL");

    // ROR - Rotate Right
    ops[0x6A] = op!(0x6A, ROR, ACC, 1, 2, "ROR");
    ops[0x66] = op!(0x66, ROR, ZP, 2, 5, "ROR");
    ops[0x76] = op!(0x76, ROR, ZPX, 2, 6, "ROR");
    ops[0x6E] = op!(0x6E, ROR, ABS, 3, 6, "ROR");
    ops[0x7E] = op!(0x7E, ROR, ABSX, 3, 7, "ROR");

    // RTI - Return from Interrupt
    ops[0x40] = op!(0x40, RTI, 1, 6, "RTI");

    // RTS - Return from Subroutine
    ops[0x60] = op!(0x60, RTS, 1, 6, "RTS");

    // SBC - Subtract with Carry
    ops[0xE9] = op!(0xE9, SBC, IMM, 2, 2, "SBC");
    ops[0xE5] = op!(0xE5, SBC, ZP, 2, 3, "SBC");
    ops[0xF5] = op!(0xF5, SBC, ZPX, 2, 4, "SBC");
    ops[0xED] = op!(0xED, SBC, ABS, 3, 4, "SBC");
    ops[0xFD] = op!(0xFD, SBC, ABSX, 3, 4, "SBC"); // +1 if page crossed
    ops[0xF9] = op!(0xF9, SBC, ABSY, 3, 4, "SBC"); // +1 if page crossed
    ops[0xE1] = op!(0xE1, SBC, INDX, 2, 6, "SBC");
    ops[0xF1] = op!(0xF1, SBC, INDY, 2, 5, "SBC"); // +1 if page crossed

    // SEC - Set Carry
    ops[0x38] = op!(0x38, SEC, 1, 2, "SEC");

    // SED - Set Decimal
    ops[0xF8] = op!(0xF8, SED, 1, 2, "SED");

    // SEI - Set Interrupt Disable
    ops[0x78] = op!(0x78, SEI, 1, 2, "SEI");

    // STA - Store A
    ops[0x85] = op!(0x85, STA, ZP, 2, 3, "STA");
    ops[0x95] = op!(0x95, STA, ZPX, 2, 4, "STA");
    ops[0x8D] = op!(0x8D, STA, ABS, 3, 4, "STA");
    ops[0x9D] = op!(0x9D, STA, ABSX, 3, 5, "STA");
    ops[0x99] = op!(0x99, STA, ABSY, 3, 5, "STA");
    ops[0x81] = op!(0x81, STA, INDX, 2, 6, "STA");
    ops[0x91] = op!(0x91, STA, INDY, 2, 6, "STA");

    // STX - Store X
    ops[0x86] = op!(0x86, STX, ZP, 2, 3, "STX");
    ops[0x96] = op!(0x96, STX, ZPY, 2, 4, "STX");
    ops[0x8E] = op!(0x8E, STX, ABS, 3, 4, "STX");

    // STY - Store Y
    ops[0x84] = op!(0x84, STY, ZP, 2, 3, "STY");
    ops[0x94] = op!(0x94, STY, ZPX, 2, 4, "STY");
    ops[0x8C] = op!(0x8C, STY, ABS, 3, 4, "STY");

    // TAX - Transfer A to X
    ops[0xAA] = op!(0xAA, TAX, 1, 2, "TAX");

    // TAY - Transfer A to Y
    ops[0xA8] = op!(0xA8, TAY, 1, 2, "TAY");

    // TSX - Transfer Stack Pointer to X
    ops[0xBA] = op!(0xBA, TSX, 1, 2, "TSX");

    // TXA - Transfer X to A
    ops[0x8A] = op!(0x8A, TXA, 1, 2, "TXA");

    // TXS - Transfer X to Stack Pointer
    ops[0x9A] = op!(0x9A, TXS, 1, 2, "TXS");

    // TYA - Transfer Y to A
    ops[0x98] = op!(0x98, TYA, 1, 2, "TYA");

    // Unofficial OP codes
    // AAC (ANC) - AND with A, then copy N to C
    ops[0x0B] = op!(0x0B, AAC, IMM, 2, 2, "*AAC");
    ops[0x2B] = op!(0x2B, AAC, IMM, 2, 2, "*AAC");

    // AAX (SAX) - Store A AND X
    ops[0x87] = op!(0x87, AAX, ZP, 2, 3, "*SAX");
    ops[0x97] = op!(0x97, AAX, ZPY, 2, 4, "*SAX");
    ops[0x83] = op!(0x83, AAX, INDX, 2, 6, "*SAX");
    ops[0x8F] = op!(0x8F, AAX, ABS, 3, 4, "*SAX");

    // ARR - AND with A, then rotate right
    ops[0x6B] = op!(0x6B, ARR, IMM, 2, 2, "*ARR");

    // ASR (ALR) - AND with A, then shift right
    ops[0x4B] = op!(0x4B, ASR, IMM, 2, 2, "*ASR");

    // ATX (LXA, OAL) - Load A and X
    ops[0xAB] = op!(0xAB, ATX, IMM, 2, 2, "*ATX");

    // AXA (SHA, AHX) - Store A AND X AND (high byte of addr + 1)
    ops[0x9F] = op!(0x9F, AXA, ABSY, 3, 5, "*AXA");
    ops[0x93] = op!(0x93, AXA, INDY, 2, 6, "*AXA");

    // AXS (SBX, SAX) - X AND A, then subtract
    ops[0xCB] = op!(0xCB, AXS, IMM, 2, 2, "*AXS");

    // DCP (DCM) - Decrement memory, then compare with A
    ops[0xC7] = op!(0xC7, DCP, ZP, 2, 5, "*DCP");
    ops[0xD7] = op!(0xD7, DCP, ZPX, 2, 6, "*DCP");
    ops[0xCF] = op!(0xCF, DCP, ABS, 3, 6, "*DCP");
    ops[0xDF] = op!(0xDF, DCP, ABSX, 3, 7, "*DCP");
    ops[0xDB] = op!(0xDB, DCP, ABSY, 3, 7, "*DCP");
    ops[0xC3] = op!(0xC3, DCP, INDX, 2, 8, "*DCP");
    ops[0xD3] = op!(0xD3, DCP, INDY, 2, 8, "*DCP");

    // DOP (SKB, NOP) - Double NOP (2 byte NOP)
    ops[0x04] = op!(0x04, DOP, ZP, 2, 3, "*NOP");
    ops[0x14] = op!(0x14, DOP, ZPX, 2, 4, "*NOP");
    ops[0x34] = op!(0x34, DOP, ZPX, 2, 4, "*NOP");
    ops[0x44] = op!(0x44, DOP, ZP, 2, 3, "*NOP");
    ops[0x54] = op!(0x54, DOP, ZPX, 2, 4, "*NOP");
    ops[0x64] = op!(0x64, DOP, ZP, 2, 3, "*NOP");
    ops[0x74] = op!(0x74, DOP, ZPX, 2, 4, "*NOP");
    ops[0x80] = op!(0x80, DOP, IMM, 2, 2, "*NOP");
    ops[0x82] = op!(0x82, DOP, IMM, 2, 2, "*NOP");
    ops[0x89] = op!(0x89, DOP, IMM, 2, 2, "*NOP");
    ops[0xC2] = op!(0xC2, DOP, IMM, 2, 2, "*NOP");
    ops[0xD4] = op!(0xD4, DOP, ZPX, 2, 4, "*NOP");
    ops[0xE2] = op!(0xE2, DOP, IMM, 2, 2, "*NOP");
    ops[0xF4] = op!(0xF4, DOP, ZPX, 2, 4, "*NOP");

    // ISC (ISB, INS) - Increment memory, then SBC
    ops[0xE7] = op!(0xE7, ISC, ZP, 2, 5, "*ISB");
    ops[0xF7] = op!(0xF7, ISC, ZPX, 2, 6, "*ISB");
    ops[0xEF] = op!(0xEF, ISC, ABS, 3, 6, "*ISB");
    ops[0xFF] = op!(0xFF, ISC, ABSX, 3, 7, "*ISB");
    ops[0xFB] = op!(0xFB, ISC, ABSY, 3, 7, "*ISB");
    ops[0xE3] = op!(0xE3, ISC, INDX, 2, 8, "*ISB");
    ops[0xF3] = op!(0xF3, ISC, INDY, 2, 8, "*ISB");

    // KIL (JAM, HLT) - Halt the CPU
    ops[0x02] = op!(0x02, KIL, 1, 0, "*KIL");
    ops[0x12] = op!(0x12, KIL, 1, 0, "*KIL");
    ops[0x22] = op!(0x22, KIL, 1, 0, "*KIL");
    ops[0x32] = op!(0x32, KIL, 1, 0, "*KIL");
    ops[0x42] = op!(0x42, KIL, 1, 0, "*KIL");
    ops[0x52] = op!(0x52, KIL, 1, 0, "*KIL");
    ops[0x62] = op!(0x62, KIL, 1, 0, "*KIL");
    ops[0x72] = op!(0x72, KIL, 1, 0, "*KIL");
    ops[0x92] = op!(0x92, KIL, 1, 0, "*KIL");
    ops[0xB2] = op!(0xB2, KIL, 1, 0, "*KIL");
    ops[0xD2] = op!(0xD2, KIL, 1, 0, "*KIL");
    ops[0xF2] = op!(0xF2, KIL, 1, 0, "*KIL");

    // LAR (LAE, LAS) - Load A, X, and SP
    ops[0xBB] = op!(0xBB, LAR, ABSY, 3, 4, "*LAR"); // +1 if page crossed

    // LAX - Load A and X
    ops[0xA7] = op!(0xA7, LAX, ZP, 2, 3, "*LAX");
    ops[0xB7] = op!(0xB7, LAX, ZPY, 2, 4, "*LAX");
    ops[0xAF] = op!(0xAF, LAX, ABS, 3, 4, "*LAX");
    ops[0xBF] = op!(0xBF, LAX, ABSY, 3, 4, "*LAX"); // +1 if page crossed
    ops[0xA3] = op!(0xA3, LAX, INDX, 2, 6, "*LAX");
    ops[0xB3] = op!(0xB3, LAX, INDY, 2, 5, "*LAX"); // +1 if page crossed

    // NOP - Unofficial NOPs (1 byte)
    ops[0x1A] = op!(0x1A, NOP, 1, 2, "*NOP");
    ops[0x3A] = op!(0x3A, NOP, 1, 2, "*NOP");
    ops[0x5A] = op!(0x5A, NOP, 1, 2, "*NOP");
    ops[0x7A] = op!(0x7A, NOP, 1, 2, "*NOP");
    ops[0xDA] = op!(0xDA, NOP, 1, 2, "*NOP");
    ops[0xFA] = op!(0xFA, NOP, 1, 2, "*NOP");

    // RLA - Rotate left, then AND with A
    ops[0x27] = op!(0x27, RLA, ZP, 2, 5, "*RLA");
    ops[0x37] = op!(0x37, RLA, ZPX, 2, 6, "*RLA");
    ops[0x2F] = op!(0x2F, RLA, ABS, 3, 6, "*RLA");
    ops[0x3F] = op!(0x3F, RLA, ABSX, 3, 7, "*RLA");
    ops[0x3B] = op!(0x3B, RLA, ABSY, 3, 7, "*RLA");
    ops[0x23] = op!(0x23, RLA, INDX, 2, 8, "*RLA");
    ops[0x33] = op!(0x33, RLA, INDY, 2, 8, "*RLA");

    // RRA - Rotate right, then ADC
    ops[0x67] = op!(0x67, RRA, ZP, 2, 5, "*RRA");
    ops[0x77] = op!(0x77, RRA, ZPX, 2, 6, "*RRA");
    ops[0x6F] = op!(0x6F, RRA, ABS, 3, 6, "*RRA");
    ops[0x7F] = op!(0x7F, RRA, ABSX, 3, 7, "*RRA");
    ops[0x7B] = op!(0x7B, RRA, ABSY, 3, 7, "*RRA");
    ops[0x63] = op!(0x63, RRA, INDX, 2, 8, "*RRA");
    ops[0x73] = op!(0x73, RRA, INDY, 2, 8, "*RRA");

    // SBC - Unofficial SBC
    ops[0xEB] = op!(0xEB, SBC, IMM, 2, 2, "*SBC");

    // SLO (ASO) - Shift left, then OR with A
    ops[0x07] = op!(0x07, SLO, ZP, 2, 5, "*SLO");
    ops[0x17] = op!(0x17, SLO, ZPX, 2, 6, "*SLO");
    ops[0x0F] = op!(0x0F, SLO, ABS, 3, 6, "*SLO");
    ops[0x1F] = op!(0x1F, SLO, ABSX, 3, 7, "*SLO");
    ops[0x1B] = op!(0x1B, SLO, ABSY, 3, 7, "*SLO");
    ops[0x03] = op!(0x03, SLO, INDX, 2, 8, "*SLO");
    ops[0x13] = op!(0x13, SLO, INDY, 2, 8, "*SLO");

    // SRE (LSE) - Shift right, then EOR with A
    ops[0x47] = op!(0x47, SRE, ZP, 2, 5, "*SRE");
    ops[0x57] = op!(0x57, SRE, ZPX, 2, 6, "*SRE");
    ops[0x4F] = op!(0x4F, SRE, ABS, 3, 6, "*SRE");
    ops[0x5F] = op!(0x5F, SRE, ABSX, 3, 7, "*SRE");
    ops[0x5B] = op!(0x5B, SRE, ABSY, 3, 7, "*SRE");
    ops[0x43] = op!(0x43, SRE, INDX, 2, 8, "*SRE");
    ops[0x53] = op!(0x53, SRE, INDY, 2, 8, "*SRE");

    // SXA (SHX, XAS) - Store X AND (high byte of addr + 1)
    ops[0x9E] = op!(0x9E, SXA, ABSY, 3, 5, "*SXA");

    // SYA (SHY, SAY) - Store Y AND (high byte of addr + 1)
    ops[0x9C] = op!(0x9C, SYA, ABSX, 3, 5, "*SYA");

    // TOP (SKW, NOP) - Triple NOP (3 byte NOP)
    ops[0x0C] = op!(0x0C, TOP, ABS, 3, 4, "*NOP");
    ops[0x1C] = op!(0x1C, TOP, ABSX, 3, 4, "*NOP"); // +1 if page crossed
    ops[0x3C] = op!(0x3C, TOP, ABSX, 3, 4, "*NOP"); // +1 if page crossed
    ops[0x5C] = op!(0x5C, TOP, ABSX, 3, 4, "*NOP"); // +1 if page crossed
    ops[0x7C] = op!(0x7C, TOP, ABSX, 3, 4, "*NOP"); // +1 if page crossed
    ops[0xDC] = op!(0xDC, TOP, ABSX, 3, 4, "*NOP"); // +1 if page crossed
    ops[0xFC] = op!(0xFC, TOP, ABSX, 3, 4, "*NOP"); // +1 if page crossed

    // XAA (ANE) - Transfer X to A, then AND with immediate
    ops[0x8B] = op!(0x8B, XAA, IMM, 2, 2, "*XAA");

    // XAS (SHS, TAS) - Transfer A AND X to SP, then store in memory
    ops[0x9B] = op!(0x9B, XAS, ABSY, 3, 5, "*XAS");

    ops
};
