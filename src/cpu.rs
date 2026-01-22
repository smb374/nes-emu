use bitflags::bitflags;

use crate::Mem;

use AddressMode::*;
use OpFamily::*;

const STACK_BASE: u16 = 0x100;
const STACK_RESET: u8 = 0xFF;

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
    pc: u16,
    reg_a: u8,
    reg_x: u8,
    reg_y: u8,
    sp: u8,
    status: CpuFlags,
    memory: [u8; 0x10000],
    jmp: Option<u16>,
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
            memory: [0u8; 0x10000],
            jmp: None,
        }
    }

    pub fn load(&mut self, program: &[u8], entry: u16) {
        if program.len() > 0xFFFF - entry as usize {
            panic!("Program too big");
        }

        self.memory[entry as usize..entry as usize + program.len()].copy_from_slice(program);
        self.write_u16(0xFFFC, entry);
    }

    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.sp = STACK_RESET;
        self.status = CpuFlags::B2 | CpuFlags::I;

        self.pc = self.read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: &[u8], entry: u16) {
        self.load(program, entry);
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
            println!(
                "stack[{:#x}] = 0x{:02x}",
                i + 1,
                self.memory[i as usize + 1]
            );
        }
    }

    pub fn run_with_cb<F>(&mut self, mut cb: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            let opcode = self.read_u8(self.pc);
            self.pc += 1;
            let pc_cache = self.pc;
            if let Some(op) = OPS[opcode as usize] {
                match op.family {
                    ADC => {
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        let carry_in = self.status.contains(CpuFlags::C) as u16;
                        let sum = self.reg_a as u16 + val as u16 + carry_in;

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
                    CLC => self.status &= CpuFlags::C.complement(),
                    CLD => self.status &= CpuFlags::D.complement(),
                    CLI => self.status &= CpuFlags::I.complement(),
                    CLV => self.status &= CpuFlags::V.complement(),
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
                        status &= CpuFlags::B.complement();
                        status |= CpuFlags::B2;
                        self.status = status;
                    }
                    ROL => {
                        let ocar = self.status.contains(CpuFlags::C) as u8;
                        if op.mode == Some(ACC) {
                            self.status.set(CpuFlags::C, self.reg_a & 0x80 != 0);
                            self.reg_a = (self.reg_a << 1) | ocar;
                            self.update_nz(self.reg_a);
                        } else {
                            let addr = self.operand_addr(op.mode).unwrap();
                            let mut val = self.read_u8(addr);
                            self.status.set(CpuFlags::C, val & 0x80 != 0);
                            val = (val << 1) | ocar;
                            self.update_nz(val);
                            self.write_u8(addr, val);
                        }
                    }
                    ROR => {
                        let old_carry = (self.status.contains(CpuFlags::C) as u8) << 7;
                        if op.mode == Some(ACC) {
                            self.status.set(CpuFlags::C, self.reg_a & 0x01 != 0);
                            self.reg_a = (self.reg_a >> 1) | old_carry;
                            self.update_nz(self.reg_a);
                        } else {
                            let addr = self.operand_addr(op.mode).unwrap();
                            let mut val = self.read_u8(addr);
                            self.status.set(CpuFlags::C, val & 0x01 != 0);
                            val = (val >> 1) | old_carry;
                            self.update_nz(val);
                            self.write_u8(addr, val);
                        }
                    }
                    RTI => {
                        let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                        status &= CpuFlags::B.complement();
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
                        let addr = self.operand_addr(op.mode).unwrap();
                        let val = self.read_u8(addr);
                        // SBC is equivalent to ADC with inverted operand
                        let inverted = val ^ 0xFF;
                        let carry_in = self.status.contains(CpuFlags::C) as u16;
                        let sum = self.reg_a as u16 + inverted as u16 + carry_in;

                        self.status.set(CpuFlags::C, sum > 0xFF);

                        let result = (sum & 0xFF) as u8;
                        self.status.set(
                            CpuFlags::V,
                            ((self.reg_a ^ result) & (self.reg_a ^ val) & 0x80) != 0,
                        );

                        self.reg_a = result;
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
                }
                if let Some(pc) = self.jmp.take() {
                    self.pc = pc;
                } else if pc_cache == self.pc {
                    self.pc += op.size as u16 - 1;
                }
                cb(self);
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

    fn operand_addr(&mut self, mode: Option<AddressMode>) -> Option<u16> {
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
        self.memory[addr as usize]
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Op {
    code: u8,
    family: OpFamily,
    mode: Option<AddressMode>,
    size: u8,
    cycles: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpFamily {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AddressMode {
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
    ($code:expr, $family:ident, $mode:ident, $size:expr, $cycles:expr) => {
        Some(Op {
            code: $code,
            family: $family,
            mode: Some($mode),
            size: $size,
            cycles: $cycles,
        })
    };
    ($code:expr, $family:ident, $size:expr, $cycles:expr) => {
        Some(Op {
            code: $code,
            family: $family,
            mode: None,
            size: $size,
            cycles: $cycles,
        })
    };
}

const OPS: [Option<Op>; 256] = {
    let mut ops: [Option<Op>; 256] = [None; 256];

    // ADC - Add with Carry
    ops[0x69] = op!(0x69, ADC, IMM, 2, 2);
    ops[0x65] = op!(0x65, ADC, ZP, 2, 3);
    ops[0x75] = op!(0x75, ADC, ZPX, 2, 4);
    ops[0x6D] = op!(0x6D, ADC, ABS, 3, 4);
    ops[0x7D] = op!(0x7D, ADC, ABSX, 3, 4); // +1 if page crossed
    ops[0x79] = op!(0x79, ADC, ABSY, 3, 4); // +1 if page crossed
    ops[0x61] = op!(0x61, ADC, INDX, 2, 6);
    ops[0x71] = op!(0x71, ADC, INDY, 2, 5); // +1 if page crossed

    // AND - Bitwise AND
    ops[0x29] = op!(0x29, AND, IMM, 2, 2);
    ops[0x25] = op!(0x25, AND, ZP, 2, 3);
    ops[0x35] = op!(0x35, AND, ZPX, 2, 4);
    ops[0x2D] = op!(0x2D, AND, ABS, 3, 4);
    ops[0x3D] = op!(0x3D, AND, ABSX, 3, 4); // +1 if page crossed
    ops[0x39] = op!(0x39, AND, ABSY, 3, 4); // +1 if page crossed
    ops[0x21] = op!(0x21, AND, INDX, 2, 6);
    ops[0x31] = op!(0x31, AND, INDY, 2, 5); // +1 if page crossed

    // ASL - Arithmetic Shift Left
    ops[0x0A] = op!(0x0A, ASL, ACC, 1, 2);
    ops[0x06] = op!(0x06, ASL, ZP, 2, 5);
    ops[0x16] = op!(0x16, ASL, ZPX, 2, 6);
    ops[0x0E] = op!(0x0E, ASL, ABS, 3, 6);
    ops[0x1E] = op!(0x1E, ASL, ABSX, 3, 7);

    // BCC - Branch if Carry Clear
    ops[0x90] = op!(0x90, BCC, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // BCS - Branch if Carry Set
    ops[0xB0] = op!(0xB0, BCS, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // BEQ - Branch if Equal (Zero Set)
    ops[0xF0] = op!(0xF0, BEQ, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // BIT - Bit Test
    ops[0x24] = op!(0x24, BIT, ZP, 2, 3);
    ops[0x2C] = op!(0x2C, BIT, ABS, 3, 4);

    // BMI - Branch if Minus (Negative Set)
    ops[0x30] = op!(0x30, BMI, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // BNE - Branch if Not Equal (Zero Clear)
    ops[0xD0] = op!(0xD0, BNE, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // BPL - Branch if Plus (Negative Clear)
    ops[0x10] = op!(0x10, BPL, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // BRK - Break (Software IRQ)
    ops[0x00] = op!(0x00, BRK, 2, 7);

    // BVC - Branch if Overflow Clear
    ops[0x50] = op!(0x50, BVC, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // BVS - Branch if Overflow Set
    ops[0x70] = op!(0x70, BVS, REL, 2, 2); // +1 if branch taken, +1 if page crossed

    // CLC - Clear Carry
    ops[0x18] = op!(0x18, CLC, 1, 2);

    // CLD - Clear Decimal
    ops[0xD8] = op!(0xD8, CLD, 1, 2);

    // CLI - Clear Interrupt Disable
    ops[0x58] = op!(0x58, CLI, 1, 2);

    // CLV - Clear Overflow
    ops[0xB8] = op!(0xB8, CLV, 1, 2);

    // CMP - Compare A
    ops[0xC9] = op!(0xC9, CMP, IMM, 2, 2);
    ops[0xC5] = op!(0xC5, CMP, ZP, 2, 3);
    ops[0xD5] = op!(0xD5, CMP, ZPX, 2, 4);
    ops[0xCD] = op!(0xCD, CMP, ABS, 3, 4);
    ops[0xDD] = op!(0xDD, CMP, ABSX, 3, 4); // +1 if page crossed
    ops[0xD9] = op!(0xD9, CMP, ABSY, 3, 4); // +1 if page crossed
    ops[0xC1] = op!(0xC1, CMP, INDX, 2, 6);
    ops[0xD1] = op!(0xD1, CMP, INDY, 2, 5); // +1 if page crossed

    // CPX - Compare X
    ops[0xE0] = op!(0xE0, CPX, IMM, 2, 2);
    ops[0xE4] = op!(0xE4, CPX, ZP, 2, 3);
    ops[0xEC] = op!(0xEC, CPX, ABS, 3, 4);

    // CPY - Compare Y
    ops[0xC0] = op!(0xC0, CPY, IMM, 2, 2);
    ops[0xC4] = op!(0xC4, CPY, ZP, 2, 3);
    ops[0xCC] = op!(0xCC, CPY, ABS, 3, 4);

    // DEC - Decrement Memory
    ops[0xC6] = op!(0xC6, DEC, ZP, 2, 5);
    ops[0xD6] = op!(0xD6, DEC, ZPX, 2, 6);
    ops[0xCE] = op!(0xCE, DEC, ABS, 3, 6);
    ops[0xDE] = op!(0xDE, DEC, ABSX, 3, 7);

    // DEX - Decrement X
    ops[0xCA] = op!(0xCA, DEX, 1, 2);

    // DEY - Decrement Y
    ops[0x88] = op!(0x88, DEY, 1, 2);

    // EOR - Bitwise Exclusive OR
    ops[0x49] = op!(0x49, EOR, IMM, 2, 2);
    ops[0x45] = op!(0x45, EOR, ZP, 2, 3);
    ops[0x55] = op!(0x55, EOR, ZPX, 2, 4);
    ops[0x4D] = op!(0x4D, EOR, ABS, 3, 4);
    ops[0x5D] = op!(0x5D, EOR, ABSX, 3, 4); // +1 if page crossed
    ops[0x59] = op!(0x59, EOR, ABSY, 3, 4); // +1 if page crossed
    ops[0x41] = op!(0x41, EOR, INDX, 2, 6);
    ops[0x51] = op!(0x51, EOR, INDY, 2, 5); // +1 if page crossed

    // INC - Increment Memory
    ops[0xE6] = op!(0xE6, INC, ZP, 2, 5);
    ops[0xF6] = op!(0xF6, INC, ZPX, 2, 6);
    ops[0xEE] = op!(0xEE, INC, ABS, 3, 6);
    ops[0xFE] = op!(0xFE, INC, ABSX, 3, 7);

    // INX - Increment X
    ops[0xE8] = op!(0xE8, INX, 1, 2);

    // INY - Increment Y
    ops[0xC8] = op!(0xC8, INY, 1, 2);

    // JMP - Jump
    ops[0x4C] = op!(0x4C, JMP, ABS, 3, 3);
    ops[0x6C] = op!(0x6C, JMP, IND, 3, 5);

    // JSR - Jump to Subroutine
    ops[0x20] = op!(0x20, JSR, ABS, 3, 6);

    // LDA - Load A
    ops[0xA9] = op!(0xA9, LDA, IMM, 2, 2);
    ops[0xA5] = op!(0xA5, LDA, ZP, 2, 3);
    ops[0xB5] = op!(0xB5, LDA, ZPX, 2, 4);
    ops[0xAD] = op!(0xAD, LDA, ABS, 3, 4);
    ops[0xBD] = op!(0xBD, LDA, ABSX, 3, 4); // +1 if page crossed
    ops[0xB9] = op!(0xB9, LDA, ABSY, 3, 4); // +1 if page crossed
    ops[0xA1] = op!(0xA1, LDA, INDX, 2, 6);
    ops[0xB1] = op!(0xB1, LDA, INDY, 2, 5); // +1 if page crossed

    // LDX - Load X
    ops[0xA2] = op!(0xA2, LDX, IMM, 2, 2);
    ops[0xA6] = op!(0xA6, LDX, ZP, 2, 3);
    ops[0xB6] = op!(0xB6, LDX, ZPY, 2, 4);
    ops[0xAE] = op!(0xAE, LDX, ABS, 3, 4);
    ops[0xBE] = op!(0xBE, LDX, ABSY, 3, 4); // +1 if page crossed

    // LDY - Load Y
    ops[0xA0] = op!(0xA0, LDY, IMM, 2, 2);
    ops[0xA4] = op!(0xA4, LDY, ZP, 2, 3);
    ops[0xB4] = op!(0xB4, LDY, ZPX, 2, 4);
    ops[0xAC] = op!(0xAC, LDY, ABS, 3, 4);
    ops[0xBC] = op!(0xBC, LDY, ABSX, 3, 4); // +1 if page crossed

    // LSR - Logical Shift Right
    ops[0x4A] = op!(0x4A, LSR, ACC, 1, 2);
    ops[0x46] = op!(0x46, LSR, ZP, 2, 5);
    ops[0x56] = op!(0x56, LSR, ZPX, 2, 6);
    ops[0x4E] = op!(0x4E, LSR, ABS, 3, 6);
    ops[0x5E] = op!(0x5E, LSR, ABSX, 3, 7);

    // NOP - No Operation
    ops[0xEA] = op!(0xEA, NOP, 1, 2);

    // ORA - Bitwise OR
    ops[0x09] = op!(0x09, ORA, IMM, 2, 2);
    ops[0x05] = op!(0x05, ORA, ZP, 2, 3);
    ops[0x15] = op!(0x15, ORA, ZPX, 2, 4);
    ops[0x0D] = op!(0x0D, ORA, ABS, 3, 4);
    ops[0x1D] = op!(0x1D, ORA, ABSX, 3, 4); // +1 if page crossed
    ops[0x19] = op!(0x19, ORA, ABSY, 3, 4); // +1 if page crossed
    ops[0x01] = op!(0x01, ORA, INDX, 2, 6);
    ops[0x11] = op!(0x11, ORA, INDY, 2, 5); // +1 if page crossed

    // PHA - Push A
    ops[0x48] = op!(0x48, PHA, 1, 3);

    // PHP - Push Processor Status
    ops[0x08] = op!(0x08, PHP, 1, 3);

    // PLA - Pull A
    ops[0x68] = op!(0x68, PLA, 1, 4);

    // PLP - Pull Processor Status
    ops[0x28] = op!(0x28, PLP, 1, 4);

    // ROL - Rotate Left
    ops[0x2A] = op!(0x2A, ROL, ACC, 1, 2);
    ops[0x26] = op!(0x26, ROL, ZP, 2, 5);
    ops[0x36] = op!(0x36, ROL, ZPX, 2, 6);
    ops[0x2E] = op!(0x2E, ROL, ABS, 3, 6);
    ops[0x3E] = op!(0x3E, ROL, ABSX, 3, 7);

    // ROR - Rotate Right
    ops[0x6A] = op!(0x6A, ROR, ACC, 1, 2);
    ops[0x66] = op!(0x66, ROR, ZP, 2, 5);
    ops[0x76] = op!(0x76, ROR, ZPX, 2, 6);
    ops[0x6E] = op!(0x6E, ROR, ABS, 3, 6);
    ops[0x7E] = op!(0x7E, ROR, ABSX, 3, 7);

    // RTI - Return from Interrupt
    ops[0x40] = op!(0x40, RTI, 1, 6);

    // RTS - Return from Subroutine
    ops[0x60] = op!(0x60, RTS, 1, 6);

    // SBC - Subtract with Carry
    ops[0xE9] = op!(0xE9, SBC, IMM, 2, 2);
    ops[0xE5] = op!(0xE5, SBC, ZP, 2, 3);
    ops[0xF5] = op!(0xF5, SBC, ZPX, 2, 4);
    ops[0xED] = op!(0xED, SBC, ABS, 3, 4);
    ops[0xFD] = op!(0xFD, SBC, ABSX, 3, 4); // +1 if page crossed
    ops[0xF9] = op!(0xF9, SBC, ABSY, 3, 4); // +1 if page crossed
    ops[0xE1] = op!(0xE1, SBC, INDX, 2, 6);
    ops[0xF1] = op!(0xF1, SBC, INDY, 2, 5); // +1 if page crossed

    // SEC - Set Carry
    ops[0x38] = op!(0x38, SEC, 1, 2);

    // SED - Set Decimal
    ops[0xF8] = op!(0xF8, SED, 1, 2);

    // SEI - Set Interrupt Disable
    ops[0x78] = op!(0x78, SEI, 1, 2);

    // STA - Store A
    ops[0x85] = op!(0x85, STA, ZP, 2, 3);
    ops[0x95] = op!(0x95, STA, ZPX, 2, 4);
    ops[0x8D] = op!(0x8D, STA, ABS, 3, 4);
    ops[0x9D] = op!(0x9D, STA, ABSX, 3, 5);
    ops[0x99] = op!(0x99, STA, ABSY, 3, 5);
    ops[0x81] = op!(0x81, STA, INDX, 2, 6);
    ops[0x91] = op!(0x91, STA, INDY, 2, 6);

    // STX - Store X
    ops[0x86] = op!(0x86, STX, ZP, 2, 3);
    ops[0x96] = op!(0x96, STX, ZPY, 2, 4);
    ops[0x8E] = op!(0x8E, STX, ABS, 3, 4);

    // STY - Store Y
    ops[0x84] = op!(0x84, STY, ZP, 2, 3);
    ops[0x94] = op!(0x94, STY, ZPX, 2, 4);
    ops[0x8C] = op!(0x8C, STY, ABS, 3, 4);

    // TAX - Transfer A to X
    ops[0xAA] = op!(0xAA, TAX, 1, 2);

    // TAY - Transfer A to Y
    ops[0xA8] = op!(0xA8, TAY, 1, 2);

    // TSX - Transfer Stack Pointer to X
    ops[0xBA] = op!(0xBA, TSX, 1, 2);

    // TXA - Transfer X to A
    ops[0x8A] = op!(0x8A, TXA, 1, 2);

    // TXS - Transfer X to Stack Pointer
    ops[0x9A] = op!(0x9A, TXS, 1, 2);

    // TYA - Transfer Y to A
    ops[0x98] = op!(0x98, TYA, 1, 2);

    ops
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_brk() {
        let program: [u8; _] = [0x00];
        let mut cpu = CPU::new();
        cpu.load_and_run(&program, 0x8000);

        assert_eq!(cpu.read_u8(STACK_BASE + STACK_RESET as u16 - 2), 0x34);
        assert_eq!(cpu.read_u16(STACK_BASE + STACK_RESET as u16 - 1), cpu.pc);
    }

    #[test]
    fn test_lda() {
        let program: [u8; _] = [0xa9, 0x05, 0x00];
        let mut cpu = CPU::new();
        cpu.load_and_run(&program, 0x8000);

        assert_eq!(cpu.reg_a, 0x05);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_lda_zero() {
        let program: [u8; _] = [0xa9, 0x00, 0x00];
        let mut cpu = CPU::new();
        cpu.load_and_run(&program, 0x8000);

        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
    }

    #[test]
    fn test_tax() {
        let program: [u8; _] = [0xa9, 0x05, 0xaa, 0x00];
        let mut cpu = CPU::new();
        cpu.load_and_run(&program, 0x8000);

        assert_eq!(cpu.reg_x, 0x05);
    }

    #[test]
    fn test_inx() {
        let program: [u8; _] = [0xa9, 0xc0, 0xaa, 0xe8, 0x00];
        let mut cpu = CPU::new();
        cpu.load_and_run(&program, 0x8000);

        assert_eq!(cpu.reg_x, 0xc1);
    }

    #[test]
    fn test_inx_overflow() {
        let program: [u8; _] = [0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00];
        let mut cpu = CPU::new();
        cpu.load_and_run(&program, 0x8000);

        assert_eq!(cpu.reg_x, 0x01);
    }

    #[test]
    fn test_stack() {
        let vals: [u8; _] = [1, 2, 3, 4, 5];
        let mut cpu = CPU::new();
        for v in vals {
            cpu.push_stack(v);
        }

        for &v in vals.iter().rev() {
            assert_eq!(cpu.pop_stack(), v);
        }
    }
}
