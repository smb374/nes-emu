use bitflags::bitflags;

use crate::{Mem, bus::Bus, cartridge::Rom, utils};

use crate::opcodes::{
    AddressMode::{self, *},
    OPS,
    OpFamily::*,
};

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
