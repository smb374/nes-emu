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

    pub fn save_prg_ram(&self) -> Result<(), String> {
        self.bus.save_prg_ram()
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
                continue;
            } else if self.irq_sig && !self.status.contains(CpuFlags::INTR_DISABLE) {
                self.interrupt_irq();
                continue;
            }
            self.irq_sig = false;
            cb(self);
            let opcode = self.read_u8(self.pc);
            self.pc += 1;
            let pc_cache = self.pc;
            if let Some(op) = OPS[opcode as usize] {
                let (irq_sig0, exit0) = self.bus.tick(op.cycles as u16);
                let extra_cycles = self.run_op(op);
                let (irq_sig1, exit1) = self.bus.tick(extra_cycles as u16);
                self.cycles += (op.cycles + extra_cycles) as usize;
                if exit0 || exit1 {
                    break;
                }
                self.irq_sig = self.irq_sig || irq_sig0 || irq_sig1;
                if pc_cache == self.pc {
                    self.pc += (op.len - 1) as u16;
                }
            } else {
                panic!("Unknown op: {}", opcode);
            }
        }
    }

    fn interrupt_nmi(&mut self) -> bool {
        self.push_stack_u16(self.pc);
        let mut flag = self.status.clone();
        flag.set(CpuFlags::BREAK, false);
        flag.set(CpuFlags::BREAK2, true);

        self.push_stack(flag.bits());
        self.status.insert(CpuFlags::INTR_DISABLE);

        self.pc = self.read_u16(0xFFFA);
        let (irq_sig, exit) = self.bus.tick(7);
        self.irq_sig = self.irq_sig || irq_sig;
        exit
    }

    fn interrupt_irq(&mut self) -> bool {
        self.push_stack_u16(self.pc);
        let mut flag = self.status.clone();
        flag.set(CpuFlags::BREAK, false);
        flag.set(CpuFlags::BREAK2, true);

        self.push_stack(flag.bits());
        self.status.insert(CpuFlags::INTR_DISABLE);

        self.pc = self.read_u16(0xFFFE);
        let (irq_sig, exit) = self.bus.tick(7);
        self.irq_sig = self.irq_sig || irq_sig;
        exit
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

    fn branch_if(&mut self, condition: bool) -> u8 {
        let offset = self.read_u8(self.pc) as i8 as i16;
        if condition {
            let old_pc = self.pc.wrapping_add(1);
            let new_pc = old_pc.wrapping_add(offset as u16);
            self.pc = new_pc;
            // +1 for branch taken, +1 more if page crossed
            if Self::page_crossed(old_pc, new_pc) {
                2
            } else {
                1
            }
        } else {
            self.pc += 1;
            0
        }
    }

    #[allow(dead_code)]
    fn page_crossed(a: u16, b: u16) -> bool {
        (a & 0xFF00) != (b & 0xFF00)
    }

    pub fn operand_addr(&mut self, mode: Option<AddressMode>) -> (Option<u16>, bool) {
        match mode {
            Some(m) => match m {
                IMM => (Some(self.pc), false),
                ZP => (Some(self.read_u8(self.pc) as u16), false),
                ZPX => (
                    Some(self.read_u8(self.pc).wrapping_add(self.reg_x) as u16),
                    false,
                ),
                ZPY => (
                    Some(self.read_u8(self.pc).wrapping_add(self.reg_y) as u16),
                    false,
                ),
                ABS => (Some(self.read_u16(self.pc)), false),
                ABSX => {
                    let base = self.read_u16(self.pc);
                    let addr = base.wrapping_add(self.reg_x as u16);
                    (Some(addr), Self::page_crossed(base, addr))
                }
                ABSY => {
                    let base = self.read_u16(self.pc);
                    let addr = base.wrapping_add(self.reg_y as u16);
                    (Some(addr), Self::page_crossed(base, addr))
                }
                IND => {
                    let addr = self.read_u16(self.pc);
                    // 6502 bug: if address is $xxFF, high byte wraps to $xx00
                    let result = if addr & 0xFF == 0xFF {
                        let lo = self.read_u8(addr);
                        let hi = self.read_u8(addr & 0xFF00); // Wrap to same page
                        (hi as u16) << 8 | lo as u16
                    } else {
                        self.read_u16(addr)
                    };
                    (Some(result), false)
                }
                INDX => {
                    let ptr = self.read_u8(self.pc).wrapping_add(self.reg_x);
                    let lo = self.read_u8(ptr as u16);
                    let hi = self.read_u8(ptr.wrapping_add(1) as u16);
                    (Some((hi as u16) << 8 | lo as u16), false)
                }
                INDY => {
                    let ptr = self.read_u8(self.pc);
                    let lo = self.read_u8(ptr as u16);
                    let hi = self.read_u8(ptr.wrapping_add(1) as u16);
                    let base = (hi as u16) << 8 | lo as u16;
                    let addr = base.wrapping_add(self.reg_y as u16);
                    (Some(addr), Self::page_crossed(base, addr))
                }
                _ => (None, false),
            },
            None => (None, false),
        }
    }

    fn run_op(&mut self, op: Op) -> u8 {
        match op.family {
            ADC => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
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

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            AND => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            ASL => {
                if op.mode == Some(ACC) {
                    self.status.set(CpuFlags::CARRY, (self.reg_a & 0x80) != 0);
                    self.reg_a <<= 1;
                    self.update_nz(self.reg_a);
                } else {
                    let (addr_opt, _) = self.operand_addr(op.mode);
                    let addr = addr_opt.unwrap();
                    let mut val = self.read_u8(addr);
                    self.status.set(CpuFlags::CARRY, (val & 0x80) != 0);
                    val <<= 1;
                    self.update_nz(val);
                    self.write_u8(addr, val);
                }
                0
            }
            BCC => self.branch_if(!self.status.contains(CpuFlags::CARRY)),
            BCS => self.branch_if(self.status.contains(CpuFlags::CARRY)),
            BEQ => self.branch_if(self.status.contains(CpuFlags::ZERO)),
            BIT => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                let res = self.reg_a & val;
                self.status.set(CpuFlags::ZERO, res == 0);
                self.status.set(CpuFlags::OVERFLOW, (val & 0x40) != 0);
                self.status.set(CpuFlags::NEGATIVE, (val & 0x80) != 0);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            BMI => self.branch_if(self.status.contains(CpuFlags::NEGATIVE)),
            BNE => self.branch_if(!self.status.contains(CpuFlags::ZERO)),
            BPL => self.branch_if(!self.status.contains(CpuFlags::NEGATIVE)),
            BRK => {
                self.push_stack_u16(self.pc + 1);
                self.push_stack((self.status | CpuFlags::BREAK | CpuFlags::BREAK2).bits());
                self.status.insert(CpuFlags::INTR_DISABLE);
                self.pc = self.read_u16(0xFFFE);
                0
            }
            BVC => self.branch_if(!self.status.contains(CpuFlags::OVERFLOW)),
            BVS => self.branch_if(self.status.contains(CpuFlags::OVERFLOW)),
            CLC => {
                self.status &= !CpuFlags::CARRY;
                0
            }
            CLD => {
                self.status &= !CpuFlags::DECIMAL;
                0
            }
            CLI => {
                self.status &= !CpuFlags::INTR_DISABLE;
                0
            }
            CLV => {
                self.status &= !CpuFlags::OVERFLOW;
                0
            }
            CMP => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let (res, car) = self.reg_a.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            CPX => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let (res, car) = self.reg_x.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            CPY => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let (res, car) = self.reg_y.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            DEC => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let res = self.read_u8(addr).wrapping_sub(1);
                self.write_u8(addr, res);
                self.update_nz(res);
                0
            }
            DEX => {
                self.reg_x = self.reg_x.wrapping_sub(1);
                self.update_nz(self.reg_x);
                0
            }
            DEY => {
                self.reg_y = self.reg_y.wrapping_sub(1);
                self.update_nz(self.reg_y);
                0
            }
            EOR => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_a ^= self.read_u8(addr);
                self.update_nz(self.reg_a);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            INC => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let res = self.read_u8(addr).wrapping_add(1);
                self.write_u8(addr, res);
                self.update_nz(res);
                0
            }
            INX => {
                self.reg_x = self.reg_x.wrapping_add(1);
                self.update_nz(self.reg_x);
                0
            }
            INY => {
                self.reg_y = self.reg_y.wrapping_add(1);
                self.update_nz(self.reg_y);
                0
            }
            JMP => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                self.pc = addr_opt.unwrap();
                0
            }
            JSR => {
                self.push_stack_u16(self.pc + 1);
                let (addr_opt, _) = self.operand_addr(op.mode);
                self.pc = addr_opt.unwrap();
                0
            }
            LDA => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_a = self.read_u8(addr);
                self.update_nz(self.reg_a);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            LDX => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_x = self.read_u8(addr);
                self.update_nz(self.reg_x);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            LDY => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_y = self.read_u8(addr);
                self.update_nz(self.reg_y);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            LSR => {
                if op.mode == Some(ACC) {
                    self.status.set(CpuFlags::CARRY, (self.reg_a & 0x01) != 0);
                    self.reg_a >>= 1;
                    self.update_nz(self.reg_a);
                } else {
                    let (addr_opt, _) = self.operand_addr(op.mode);
                    let addr = addr_opt.unwrap();
                    let mut val = self.read_u8(addr);
                    self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
                    val >>= 1;
                    self.update_nz(val);
                    self.write_u8(addr, val);
                }
                0
            }
            NOP => 0,
            ORA => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_a |= self.read_u8(addr);
                self.update_nz(self.reg_a);

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            PHA => {
                self.push_stack(self.reg_a);
                0
            }
            PHP => {
                self.push_stack((self.status | CpuFlags::BREAK | CpuFlags::BREAK2).bits());
                0
            }
            PLA => {
                self.reg_a = self.pop_stack();
                self.update_nz(self.reg_a);
                0
            }
            PLP => {
                let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                status &= !CpuFlags::BREAK;
                status |= CpuFlags::BREAK2;
                self.status = status;
                0
            }
            ROL => {
                let ocar = self.status.contains(CpuFlags::CARRY);
                if op.mode == Some(ACC) {
                    let (res, car) = utils::rol(self.reg_a, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.reg_a = res;
                    self.update_nz(self.reg_a);
                } else {
                    let (addr_opt, _) = self.operand_addr(op.mode);
                    let addr = addr_opt.unwrap();
                    let val = self.read_u8(addr);
                    let (res, car) = utils::rol(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.write_u8(addr, res);
                }
                0
            }
            ROR => {
                let ocar = self.status.contains(CpuFlags::CARRY);
                if op.mode == Some(ACC) {
                    let (res, car) = utils::ror(self.reg_a, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.reg_a = res;
                    self.update_nz(self.reg_a);
                } else {
                    let (addr_opt, _) = self.operand_addr(op.mode);
                    let addr = addr_opt.unwrap();
                    let val = self.read_u8(addr);
                    let (res, car) = utils::ror(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.write_u8(addr, res);
                }
                0
            }
            RTI => {
                let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                status &= !CpuFlags::BREAK;
                status |= CpuFlags::BREAK2;
                self.status = status;
                self.pc = self.pop_stack_u16();
                0
            }
            RTS => {
                self.pc = self.pop_stack_u16() + 1;
                0
            }
            SBC => {
                // ADC with inverted operand
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
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

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            SEC => {
                self.status |= CpuFlags::CARRY;
                0
            }
            SED => {
                self.status |= CpuFlags::DECIMAL;
                0
            }
            SEI => {
                self.status |= CpuFlags::INTR_DISABLE;
                0
            }
            STA => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.write_u8(addr, self.reg_a);
                0
            }
            STX => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.write_u8(addr, self.reg_x);
                0
            }
            STY => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.write_u8(addr, self.reg_y);
                0
            }
            TAX => {
                self.reg_x = self.reg_a;
                self.update_nz(self.reg_x);
                0
            }
            TAY => {
                self.reg_y = self.reg_a;
                self.update_nz(self.reg_y);
                0
            }
            TSX => {
                self.reg_x = self.sp;
                self.update_nz(self.reg_x);
                0
            }
            TXA => {
                self.reg_a = self.reg_x;
                self.update_nz(self.reg_a);
                0
            }
            TXS => {
                self.sp = self.reg_x;
                0
            }
            TYA => {
                self.reg_a = self.reg_y;
                self.update_nz(self.reg_a);
                0
            }
            // Unofficial
            AAC => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);
                self.status
                    .set(CpuFlags::CARRY, self.status.contains(CpuFlags::NEGATIVE));

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            AAX => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let res = self.reg_a & self.reg_x;
                self.write_u8(addr, res);
                0
            }
            ARR => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
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

                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            ASR => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let val = self.reg_a & self.read_u8(addr);
                self.reg_a = val >> 1;
                self.update_nz(self.reg_a);
                self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            ATX => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                self.reg_a &= self.read_u8(addr);
                self.reg_x = self.reg_a;
                self.update_nz(self.reg_a);
                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            AXA => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let res = self.reg_a & self.reg_x & 7;
                self.write_u8(addr, res);
                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            AXS => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.reg_x &= self.reg_a;
                let sum = self.reg_x as u16 + (val ^ 0xFF) as u16 + 1;
                self.status.set(CpuFlags::CARRY, sum > 0xFF);
                self.reg_x = (sum & 0xFF) as u8;
                self.update_nz(self.reg_x);
                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            DCP => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let res = self.read_u8(addr).wrapping_sub(1);
                self.write_u8(addr, res);
                let (res, car) = self.reg_a.overflowing_sub(res);
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
                0
            }
            DOP => 0,
            ISC => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
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
                0
            }
            KIL => {
                std::process::exit(0);
            }
            LAR => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.sp &= val;
                self.reg_a = self.sp;
                self.reg_x = self.sp;
                self.update_nz(self.reg_a);
                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            LAX => {
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.reg_a = val;
                self.reg_x = val;
                self.update_nz(val);
                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
            RLA => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
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
                    0
                }
            }
            RRA => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
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
                    0
                }
            }
            SLO => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                {
                    let mut val = self.read_u8(addr);
                    self.status.set(CpuFlags::CARRY, (val & 0x80) != 0);
                    val <<= 1;
                    self.update_nz(val);
                    self.write_u8(addr, val);
                }
                {
                    self.reg_a |= self.read_u8(addr);
                    self.update_nz(self.reg_a);
                    0
                }
            }
            SRE => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                {
                    let mut val = self.read_u8(addr);
                    self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
                    val >>= 1;
                    self.update_nz(val);
                    self.write_u8(addr, val);
                }
                {
                    self.reg_a ^= self.read_u8(addr);
                    self.update_nz(self.reg_a);
                    0
                }
            }
            SXA => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let hi = ((addr >> 8) as u8).wrapping_add(1);
                let res = self.reg_x & hi;
                self.write_u8(addr, res);
                0
            }
            SYA => {
                let (addr_opt, _) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let hi = ((addr >> 8) as u8).wrapping_add(1);
                let res = self.reg_y & hi;
                self.write_u8(addr, res);
                0
            }
            TOP => 0,
            XAA => {
                {
                    self.reg_a = self.reg_x;
                    self.update_nz(self.reg_a);
                }
                {
                    let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                    let addr = addr_opt.unwrap();
                    self.reg_a &= self.read_u8(addr);
                    self.update_nz(self.reg_a);

                    if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                        1
                    } else {
                        0
                    }
                }
            }
            XAS => {
                self.sp = self.reg_x & self.reg_a;
                let (addr_opt, page_crossed) = self.operand_addr(op.mode);
                let addr = addr_opt.unwrap();
                let hi = ((addr >> 8) as u8).wrapping_add(1);
                let res = self.sp & hi;
                self.write_u8(addr, res);
                if page_crossed && matches!(op.mode, Some(ABSX) | Some(ABSY) | Some(INDY)) {
                    1
                } else {
                    0
                }
            }
        }
    }
}
