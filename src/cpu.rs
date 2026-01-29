use bitflags::bitflags;

use crate::{
    Mem,
    bus::Bus,
    opcodes::{
        AddressMode::{self, *},
        OPS, Op,
        OpFamily::*,
    },
    utils,
};

const STACK_BASE: u16 = 0x100;
const STACK_RESET: u8 = 0xFD;

bitflags! {
    // 7  bit  0
    // ---- ----
    // NV1B DIZC
    // |||| ||||
    // |||| |||+- Carry
    // |||| ||+-- Zero
    // |||| |+--- Interrupt Disable
    // |||| +---- Decimal
    // |||+------ (No CPU effect; see: the B flag)
    // ||+------- (No CPU effect; always pushed as 1)
    // |+-------- Overflow
    // +--------- Negative
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct CpuFlags: u8 {
        const CARRY        = 0b00000001;
        const ZERO         = 0b00000010;
        const INTR_DISABLE = 0b00000100;
        const DECIMAL      = 0b00001000;
        const BREAK        = 0b00010000;
        const BREAK2       = 0b00100000;
        const OVERFLOW     = 0b01000000;
        const NEGATIVE     = 0b10000000;
    }
}

pub struct CPU<'a> {
    pub pc: u16,
    pub reg_a: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub sp: u8,
    pub status: CpuFlags,
    bus: Bus<'a>,
    irq_sig: bool,
    pub cycles: usize,
}

impl<'a> Mem for CPU<'a> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        self.bus.read_u8(addr)
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        self.bus.write_u8(addr, val);
    }
}

impl<'a> CPU<'a> {
    pub fn new(bus: Bus<'a>) -> Self {
        CPU {
            pc: 0,
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            sp: STACK_RESET,
            status: CpuFlags::BREAK2 | CpuFlags::INTR_DISABLE,
            bus,
            irq_sig: false,
            cycles: 0,
        }
    }

    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.sp = STACK_RESET;
        self.status = CpuFlags::BREAK2 | CpuFlags::INTR_DISABLE;
        self.cycles = 0;

        self.pc = self.read_u16(0xFFFC);
    }

    pub fn run(&mut self) {
        self.run_with_cb(|_| {});
    }

    pub fn run_with_cb<F>(&mut self, mut cb: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            if let Some(_nmi) = self.bus.poll_nmi_status() {
                self.interrupt_nmi();
            } else if self.irq_sig && !self.status.contains(CpuFlags::INTR_DISABLE) {
                self.interrupt_irq();
            }
            self.irq_sig = false;
            cb(self);
            let opcode = self.read_u8(self.pc);
            self.pc += 1;
            let pc_cache = self.pc;
            if let Some(op) = OPS[opcode as usize] {
                self.run_op(op);
                self.cycles += op.cycles as usize;
                self.irq_sig = self.irq_sig || self.bus.tick(op.cycles);
                if pc_cache == self.pc {
                    self.pc += (op.len - 1) as u16;
                }
            } else {
                panic!("Unknown op: {}", opcode);
            }
        }
    }

    fn interrupt_nmi(&mut self) {
        self.push_stack_u16(self.pc);
        let mut flag = self.status.clone();
        flag.set(CpuFlags::BREAK, false);
        flag.set(CpuFlags::BREAK2, true);

        self.push_stack(flag.bits());
        self.status.insert(CpuFlags::INTR_DISABLE);

        self.bus.tick(2);
        self.pc = self.read_u16(0xFFFA);
    }

    fn interrupt_irq(&mut self) {
        self.push_stack_u16(self.pc);
        let mut flag = self.status.clone();
        flag.set(CpuFlags::BREAK, false);
        flag.set(CpuFlags::BREAK2, true);

        self.push_stack(flag.bits());
        self.status.insert(CpuFlags::INTR_DISABLE);

        self.bus.tick(2);
        self.pc = self.read_u16(0xFFFE);
    }

    fn push_stack_u16(&mut self, val: u16) {
        let vbuf = val.to_le_bytes();
        self.push_stack(vbuf[1]);
        self.push_stack(vbuf[0]);
    }

    fn pop_stack_u16(&mut self) -> u16 {
        let vbuf = [self.pop_stack(), self.pop_stack()];
        u16::from_le_bytes(vbuf)
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
        self.status.set(CpuFlags::ZERO, val == 0);
        self.status.set(CpuFlags::NEGATIVE, val & 0x80 != 0);
    }

    fn branch_if(&mut self, condition: bool) {
        let offset = self.read_u8(self.pc) as i8 as i16;
        if condition {
            self.pc = self.pc.wrapping_add(1).wrapping_add(offset as u16);
        } else {
            self.pc += 1;
        }
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

    fn run_op(&mut self, op: Op) {
        match op.family {
            ADC => {
                let addr = self.operand_addr(op.mode).unwrap();
                let val = self.read_u8(addr);
                let car = self.status.contains(CpuFlags::CARRY) as u16;
                let sum = self.reg_a as u16 + val as u16 + car;

                self.status.set(CpuFlags::CARRY, sum > 0xFF);

                let result = (sum & 0xFF) as u8;
                self.status.set(
                    CpuFlags::OVERFLOW,
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
                    self.status.set(CpuFlags::CARRY, self.reg_a & 0x80 != 0);
                    self.reg_a <<= 1;
                    self.update_nz(self.reg_a);
                } else {
                    let addr = self.operand_addr(op.mode).unwrap();
                    let mut val = self.read_u8(addr);
                    self.status.set(CpuFlags::CARRY, val & 0x80 != 0);
                    val <<= 1;
                    self.update_nz(val);
                    self.write_u8(addr, val);
                }
            }
            BCC => self.branch_if(!self.status.contains(CpuFlags::CARRY)),
            BCS => self.branch_if(self.status.contains(CpuFlags::CARRY)),
            BEQ => self.branch_if(self.status.contains(CpuFlags::ZERO)),
            BIT => {
                let addr = self.operand_addr(op.mode).unwrap();
                let val = self.read_u8(addr);
                let res = self.reg_a & val;
                self.status.set(CpuFlags::ZERO, res == 0);
                self.status.set(CpuFlags::OVERFLOW, val & 0x40 != 0);
                self.status.set(CpuFlags::NEGATIVE, val & 0x80 != 0);
            }
            BMI => self.branch_if(self.status.contains(CpuFlags::NEGATIVE)),
            BNE => self.branch_if(!self.status.contains(CpuFlags::ZERO)),
            BPL => self.branch_if(!self.status.contains(CpuFlags::NEGATIVE)),
            BRK => {
                self.push_stack_u16(self.pc + 1);
                self.push_stack((self.status | CpuFlags::BREAK | CpuFlags::BREAK2).bits());
                self.status.insert(CpuFlags::INTR_DISABLE);
                self.pc = self.read_u16(0xFFFE);
            }
            BVC => self.branch_if(!self.status.contains(CpuFlags::OVERFLOW)),
            BVS => self.branch_if(self.status.contains(CpuFlags::OVERFLOW)),
            CLC => self.status &= !CpuFlags::CARRY,
            CLD => self.status &= !CpuFlags::DECIMAL,
            CLI => self.status &= !CpuFlags::INTR_DISABLE,
            CLV => self.status &= !CpuFlags::OVERFLOW,
            CMP => {
                let addr = self.operand_addr(op.mode).unwrap();
                let (res, car) = self.reg_a.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            CPX => {
                let addr = self.operand_addr(op.mode).unwrap();
                let (res, car) = self.reg_x.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            CPY => {
                let addr = self.operand_addr(op.mode).unwrap();
                let (res, car) = self.reg_y.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
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
            JMP => self.pc = self.operand_addr(op.mode).unwrap(),
            JSR => {
                self.push_stack_u16(self.pc + 1);
                self.pc = self.operand_addr(op.mode).unwrap();
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
                    self.status.set(CpuFlags::CARRY, self.reg_a & 0x01 != 0);
                    self.reg_a >>= 1;
                    self.update_nz(self.reg_a);
                } else {
                    let addr = self.operand_addr(op.mode).unwrap();
                    let mut val = self.read_u8(addr);
                    self.status.set(CpuFlags::CARRY, val & 0x01 != 0);
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
            PHP => self.push_stack((self.status | CpuFlags::BREAK | CpuFlags::BREAK2).bits()),
            PLA => {
                self.reg_a = self.pop_stack();
                self.update_nz(self.reg_a);
            }
            PLP => {
                let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                status &= !CpuFlags::BREAK;
                status |= CpuFlags::BREAK2;
                self.status = status;
            }
            ROL => {
                let ocar = self.status.contains(CpuFlags::CARRY);
                if op.mode == Some(ACC) {
                    let (res, car) = utils::rol(self.reg_a, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.reg_a = res;
                    self.update_nz(self.reg_a);
                } else {
                    let addr = self.operand_addr(op.mode).unwrap();
                    let val = self.read_u8(addr);
                    let (res, car) = utils::rol(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.write_u8(addr, res);
                }
            }
            ROR => {
                let ocar = self.status.contains(CpuFlags::CARRY);
                if op.mode == Some(ACC) {
                    let (res, car) = utils::ror(self.reg_a, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.reg_a = res;
                    self.update_nz(self.reg_a);
                } else {
                    let addr = self.operand_addr(op.mode).unwrap();
                    let val = self.read_u8(addr);
                    let (res, car) = utils::ror(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.write_u8(addr, res);
                }
            }
            RTI => {
                let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                status &= !CpuFlags::BREAK;
                status |= CpuFlags::BREAK2;
                self.status = status;
                self.pc = self.pop_stack_u16();
            }
            RTS => self.pc = self.pop_stack_u16() + 1,
            SBC => {
                // ADC with inverted operand
                let addr = self.operand_addr(op.mode).unwrap();
                let val = self.read_u8(addr);
                let car = self.status.contains(CpuFlags::CARRY) as u16;
                let sum = self.reg_a as u16 + (val ^ 0xFF) as u16 + car;

                self.status.set(CpuFlags::CARRY, sum > 0xFF);

                let res = (sum & 0xFF) as u8;
                self.status.set(
                    CpuFlags::OVERFLOW,
                    ((self.reg_a ^ res) & (self.reg_a ^ val) & 0x80) != 0,
                );

                self.reg_a = res;
                self.update_nz(self.reg_a);
            }
            SEC => self.status |= CpuFlags::CARRY,
            SED => self.status |= CpuFlags::DECIMAL,
            SEI => self.status |= CpuFlags::INTR_DISABLE,
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
                    .set(CpuFlags::CARRY, self.status.contains(CpuFlags::NEGATIVE));
            }
            AAX => {
                let addr = self.operand_addr(op.mode).unwrap();
                let res = self.reg_a & self.reg_x;
                self.write_u8(addr, res);
            }
            ARR => {
                let addr = self.operand_addr(op.mode).unwrap();
                let val = self.read_u8(addr);
                let (res, _) = utils::ror(self.reg_a & val, self.status.contains(CpuFlags::CARRY));
                self.reg_a = res;
                self.update_nz(self.reg_a);
                let chk = (res & 0b0110_0000) >> 5;
                match chk {
                    0b11 => {
                        self.status |= CpuFlags::CARRY;
                        self.status &= !CpuFlags::OVERFLOW;
                    }
                    0b00 => {
                        self.status &= !(CpuFlags::CARRY | CpuFlags::OVERFLOW);
                    }
                    0b01 => {
                        self.status |= CpuFlags::OVERFLOW;
                        self.status &= !CpuFlags::CARRY;
                    }
                    0b10 => {
                        self.status |= CpuFlags::CARRY | CpuFlags::OVERFLOW;
                    }
                    _ => unreachable!(),
                }
            }
            ASR => {
                let addr = self.operand_addr(op.mode).unwrap();
                let val = self.reg_a & self.read_u8(addr);
                self.reg_a = val >> 1;
                self.update_nz(self.reg_a);
                self.status.set(CpuFlags::CARRY, val & 1 != 0);
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
                self.status.set(CpuFlags::CARRY, sum > 0xFF);
                self.reg_x = (sum & 0xFF) as u8;
                self.update_nz(self.reg_x);
            }
            DCP => {
                let addr = self.operand_addr(op.mode).unwrap();
                let res = self.read_u8(addr).wrapping_sub(1);
                self.write_u8(addr, res);
                let (res, car) = self.reg_a.overflowing_sub(res);
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            DOP => {}
            ISC => {
                let addr = self.operand_addr(op.mode).unwrap();
                let val = self.read_u8(addr).wrapping_add(1);
                self.write_u8(addr, val);
                let car = self.status.contains(CpuFlags::CARRY) as u16;
                let sum = self.reg_a as u16 + (val ^ 0xFF) as u16 + car;

                self.status.set(CpuFlags::CARRY, sum > 0xFF);

                let res = (sum & 0xFF) as u8;
                self.status.set(
                    CpuFlags::OVERFLOW,
                    ((self.reg_a ^ res) & (self.reg_a ^ val) & 0x80) != 0,
                );

                self.reg_a = res;
                self.update_nz(self.reg_a);
            }
            KIL => {
                std::process::exit(0);
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
                    let ocar = self.status.contains(CpuFlags::CARRY);
                    let (res, car) = utils::rol(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
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
                    let ocar = self.status.contains(CpuFlags::CARRY);
                    let val = self.read_u8(addr);
                    let (res, car) = utils::ror(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.write_u8(addr, res);
                }
                {
                    let val = self.read_u8(addr);
                    let car = self.status.contains(CpuFlags::CARRY) as u16;
                    let sum = self.reg_a as u16 + val as u16 + car;

                    self.status.set(CpuFlags::CARRY, sum > 0xFF);

                    let result = (sum & 0xFF) as u8;
                    self.status.set(
                        CpuFlags::OVERFLOW,
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
                    self.status.set(CpuFlags::CARRY, val & 0x80 != 0);
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
                    self.status.set(CpuFlags::CARRY, val & 0x01 != 0);
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
    }
}
