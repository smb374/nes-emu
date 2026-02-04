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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionType {
    Read,
    Write,
    Rmw,
}

pub struct CPU<'a> {
    pub pc: u16,
    pub reg_a: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub sp: u8,
    pub status: CpuFlags,
    pub bus: Bus<'a>,
    irq_sig: bool,
    exit_sig: bool,
    pub cycles: usize,
    pub tick_disable: bool,
}

impl<'a> Mem for CPU<'a> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        self.tick(1);
        self.bus.read_u8(addr)
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        self.bus.write_u8(addr, val);
        self.tick(1);
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
            exit_sig: false,
            cycles: 0,
            tick_disable: false,
        }
    }

    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.sp = STACK_RESET;
        self.status = CpuFlags::BREAK2 | CpuFlags::INTR_DISABLE;
        self.cycles = 0;

        self.tick(5);
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
                if self.exit_sig {
                    break;
                }
                if pc_cache == self.pc {
                    self.pc += (op.len - 1) as u16;
                }
            } else {
                panic!("Unknown op: {}", opcode);
            }
        }
    }

    fn tick(&mut self, cycles: u16) {
        if !self.tick_disable {
            let (irq, exit) = self.bus.tick(cycles);
            self.irq_sig |= irq;
            self.exit_sig |= exit;

            self.cycles += cycles as usize;
        }
    }

    fn interrupt_nmi(&mut self) {
        self.tick(2);
        self.push_stack_u16(self.pc);
        let mut flag = self.status.clone();
        flag.set(CpuFlags::BREAK, false);
        flag.set(CpuFlags::BREAK2, true);

        self.push_stack(flag.bits());
        self.status.insert(CpuFlags::INTR_DISABLE);

        self.pc = self.read_u16(0xFFFA);
    }

    fn interrupt_irq(&mut self) {
        self.tick(2);
        self.push_stack_u16(self.pc);
        let mut flag = self.status.clone();
        flag.set(CpuFlags::BREAK, false);
        flag.set(CpuFlags::BREAK2, true);

        self.push_stack(flag.bits());
        self.status.insert(CpuFlags::INTR_DISABLE);

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
        self.pc = self.pc.wrapping_add(1);
        if condition {
            // Dummy read while adding offset to PCL
            self.bus.read_u8(self.pc);
            self.tick(1);

            let old_pc = self.pc;
            let new_pc = self.pc.wrapping_add_signed(offset);

            if Self::page_crossed(old_pc, new_pc) {
                // Another dummy read from wrong address
                let wrong_pc = (old_pc & 0xFF00) | (new_pc & 0x00FF);
                self.bus.read_u8(wrong_pc);
                self.tick(1);
            }
            self.pc = new_pc;
        }
    }

    fn page_crossed(a: u16, b: u16) -> bool {
        (a & 0xFF00) != (b & 0xFF00)
    }

    pub fn operand_addr(
        &mut self,
        mode: Option<AddressMode>,
        instr_type: InstructionType,
    ) -> Option<u16> {
        match mode {
            None => None,
            Some(m) => match m {
                AddressMode::IMM => {
                    let addr = self.pc;
                    Some(addr)
                }
                AddressMode::ZP => {
                    let addr = self.read_u8(self.pc) as u16;
                    Some(addr)
                }
                AddressMode::ZPX => {
                    let base = self.read_u8(self.pc);
                    self.read_u8(base as u16);
                    let addr = base.wrapping_add(self.reg_x) as u16;
                    Some(addr)
                }
                AddressMode::ZPY => {
                    let base = self.read_u8(self.pc);
                    self.read_u8(base as u16);
                    let addr = base.wrapping_add(self.reg_y) as u16;
                    Some(addr)
                }
                AddressMode::ABS => {
                    let lo = self.read_u8(self.pc) as u16;
                    let hi = self.read_u8(self.pc.wrapping_add(1)) as u16;
                    Some((hi << 8) | lo)
                }
                AddressMode::ABSX => {
                    let lo = self.read_u8(self.pc) as u16;
                    let hi = self.read_u8(self.pc.wrapping_add(1)) as u16;

                    let base = (hi << 8) | lo;
                    let addr = base.wrapping_add(self.reg_x as u16);
                    let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                    // For writes and RMW, always do the dummy read
                    // For reads, only if page crossed
                    if instr_type != InstructionType::Read || page_crossed {
                        // Dummy read from potentially incorrect address
                        let wrong_addr = (hi << 8) | ((lo + self.reg_x as u16) & 0xFF);
                        self.read_u8(wrong_addr);
                    }

                    Some(addr)
                }

                // Absolute,Y: Same as ABSX but with Y
                AddressMode::ABSY => {
                    let lo = self.read_u8(self.pc) as u16;
                    let hi = self.read_u8(self.pc.wrapping_add(1)) as u16;

                    let base = (hi << 8) | lo;
                    let addr = base.wrapping_add(self.reg_y as u16);
                    let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                    if instr_type != InstructionType::Read || page_crossed {
                        let wrong_addr = (hi << 8) | ((lo + self.reg_y as u16) & 0xFF);
                        self.read_u8(wrong_addr);
                    }

                    Some(addr)
                }

                AddressMode::IND => {
                    let ptr_lo = self.read_u8(self.pc) as u16;
                    let ptr_hi = self.read_u8(self.pc.wrapping_add(1)) as u16;
                    let ptr = (ptr_hi << 8) | ptr_lo;
                    let lo = self.read_u8(ptr) as u16;
                    let hi_addr = if ptr_lo == 0xFF {
                        ptr & 0xFF00 // Wrap within same page
                    } else {
                        ptr + 1
                    };
                    let hi = self.read_u8(hi_addr) as u16;

                    Some((hi << 8) | lo)
                }

                AddressMode::INDX => {
                    let ptr = self.read_u8(self.pc);
                    self.read_u8(ptr as u16);

                    let ptr_x = ptr.wrapping_add(self.reg_x);
                    let lo = self.read_u8(ptr_x as u16) as u16;
                    let hi = self.read_u8(ptr_x.wrapping_add(1) as u16) as u16;

                    Some((hi << 8) | lo)
                }
                AddressMode::INDY => {
                    let ptr = self.read_u8(self.pc);
                    let lo = self.read_u8(ptr as u16) as u16;
                    let hi = self.read_u8(ptr.wrapping_add(1) as u16) as u16;

                    let base = (hi << 8) | lo;
                    let addr = base.wrapping_add(self.reg_y as u16);
                    let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                    if instr_type != InstructionType::Read || page_crossed {
                        let wrong_addr = (hi << 8) | ((lo + self.reg_y as u16) & 0xFF);
                        self.read_u8(wrong_addr);
                    }

                    Some(addr)
                }
                _ => None,
            },
        }
    }

    fn run_op(&mut self, op: Op) {
        match op.family {
            ADC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
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
            }
            AND => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            ASL => {
                if op.mode == Some(ACC) {
                    self.status.set(CpuFlags::CARRY, (self.reg_a & 0x80) != 0);
                    self.reg_a <<= 1;
                    self.update_nz(self.reg_a);
                    self.tick(1);
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let mut val = self.read_u8(addr);
                    self.write_u8(addr, val);
                    self.status.set(CpuFlags::CARRY, (val & 0x80) != 0);
                    val <<= 1;
                    self.update_nz(val);
                    self.write_u8(addr, val);
                }
            }
            BCC => self.branch_if(!self.status.contains(CpuFlags::CARRY)),
            BCS => self.branch_if(self.status.contains(CpuFlags::CARRY)),
            BEQ => self.branch_if(self.status.contains(CpuFlags::ZERO)),
            BIT => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                let res = self.reg_a & val;
                self.status.set(CpuFlags::ZERO, res == 0);
                self.status.set(CpuFlags::OVERFLOW, (val & 0x40) != 0);
                self.status.set(CpuFlags::NEGATIVE, (val & 0x80) != 0);
            }
            BMI => self.branch_if(self.status.contains(CpuFlags::NEGATIVE)),
            BNE => self.branch_if(!self.status.contains(CpuFlags::ZERO)),
            BPL => self.branch_if(!self.status.contains(CpuFlags::NEGATIVE)),
            BRK => {
                self.read_u8(self.pc);
                self.push_stack_u16(self.pc + 1);
                self.push_stack((self.status | CpuFlags::BREAK | CpuFlags::BREAK2).bits());
                self.status.insert(CpuFlags::INTR_DISABLE);
                self.pc = self.read_u16(0xFFFE);
            }
            BVC => self.branch_if(!self.status.contains(CpuFlags::OVERFLOW)),
            BVS => self.branch_if(self.status.contains(CpuFlags::OVERFLOW)),
            CLC => {
                self.status &= !CpuFlags::CARRY;
                self.tick(1);
            }
            CLD => {
                self.status &= !CpuFlags::DECIMAL;
                self.tick(1);
            }
            CLI => {
                self.status &= !CpuFlags::INTR_DISABLE;
                self.tick(1);
            }
            CLV => {
                self.status &= !CpuFlags::OVERFLOW;
                self.tick(1);
            }
            CMP => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let (res, car) = self.reg_a.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            CPX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let (res, car) = self.reg_x.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            CPY => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let (res, car) = self.reg_y.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            DEC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.write_u8(addr, val);
                let res = val.wrapping_sub(1);
                self.write_u8(addr, res);
                self.update_nz(res);
            }
            DEX => {
                self.reg_x = self.reg_x.wrapping_sub(1);
                self.update_nz(self.reg_x);
                self.tick(1);
            }
            DEY => {
                self.reg_y = self.reg_y.wrapping_sub(1);
                self.update_nz(self.reg_y);
                self.tick(1);
            }
            EOR => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a ^= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            INC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.write_u8(addr, val);
                let res = val.wrapping_add(1);
                self.write_u8(addr, res);
                self.update_nz(res);
            }
            INX => {
                self.reg_x = self.reg_x.wrapping_add(1);
                self.update_nz(self.reg_x);
                self.tick(1);
            }
            INY => {
                self.reg_y = self.reg_y.wrapping_add(1);
                self.update_nz(self.reg_y);
                self.tick(1);
            }
            JMP => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                self.pc = addr_opt.unwrap();
            }
            JSR => {
                let lo = self.read_u8(self.pc) as u16;
                self.pc = self.pc.wrapping_add(1);
                self.bus.read_u8(STACK_BASE + self.sp as u16);
                self.tick(1);
                self.push_stack_u16(self.pc);
                let hi = self.read_u8(self.pc) as u16;
                self.pc = (hi << 8) | lo;
            }
            LDA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a = self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            LDX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_x = self.read_u8(addr);
                self.update_nz(self.reg_x);
            }
            LDY => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_y = self.read_u8(addr);
                self.update_nz(self.reg_y);
            }
            LSR => {
                if op.mode == Some(ACC) {
                    self.status.set(CpuFlags::CARRY, (self.reg_a & 0x01) != 0);
                    self.reg_a >>= 1;
                    self.update_nz(self.reg_a);
                    self.tick(1);
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let mut val = self.read_u8(addr);
                    self.write_u8(addr, val);
                    self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
                    val >>= 1;
                    self.update_nz(val);
                    self.write_u8(addr, val);
                }
            }
            NOP => {
                self.tick(1);
            }
            ORA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a |= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            PHA => {
                self.push_stack(self.reg_a);
                self.tick(1);
            }
            PHP => {
                self.push_stack((self.status | CpuFlags::BREAK | CpuFlags::BREAK2).bits());
                self.tick(1);
            }
            PLA => {
                self.reg_a = self.pop_stack();
                self.update_nz(self.reg_a);
                self.tick(2);
            }
            PLP => {
                let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                status &= !CpuFlags::BREAK;
                status |= CpuFlags::BREAK2;
                self.status = status;
                self.tick(2);
            }
            ROL => {
                let ocar = self.status.contains(CpuFlags::CARRY);
                if op.mode == Some(ACC) {
                    let (res, car) = utils::rol(self.reg_a, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.reg_a = res;
                    self.update_nz(self.reg_a);
                    self.tick(1);
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let val = self.read_u8(addr);
                    self.write_u8(addr, val);
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
                    self.tick(1);
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let val = self.read_u8(addr);
                    self.write_u8(addr, val);
                    let (res, car) = utils::ror(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.write_u8(addr, res);
                }
            }
            RTI => {
                self.tick(2);
                let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                status &= !CpuFlags::BREAK;
                status |= CpuFlags::BREAK2;
                self.status = status;
                self.pc = self.pop_stack_u16();
            }
            RTS => {
                self.tick(2);
                self.pc = self.pop_stack_u16() + 1;
                self.tick(1);
            }
            SBC => {
                // ADC with inverted operand
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
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
            }
            SEC => {
                self.status |= CpuFlags::CARRY;
                self.tick(1);
            }
            SED => {
                self.status |= CpuFlags::DECIMAL;
                self.tick(1);
            }
            SEI => {
                self.status |= CpuFlags::INTR_DISABLE;
                self.tick(1);
            }
            STA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                self.write_u8(addr, self.reg_a);
            }
            STX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                self.write_u8(addr, self.reg_x);
            }
            STY => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                self.write_u8(addr, self.reg_y);
            }
            TAX => {
                self.reg_x = self.reg_a;
                self.update_nz(self.reg_x);
                self.tick(1);
            }
            TAY => {
                self.reg_y = self.reg_a;
                self.update_nz(self.reg_y);
                self.tick(1);
            }
            TSX => {
                self.reg_x = self.sp;
                self.update_nz(self.reg_x);
                self.tick(1);
            }
            TXA => {
                self.reg_a = self.reg_x;
                self.update_nz(self.reg_a);
                self.tick(1);
            }
            TXS => {
                self.sp = self.reg_x;
                self.tick(1);
            }
            TYA => {
                self.reg_a = self.reg_y;
                self.update_nz(self.reg_a);
                self.tick(1);
            }
            // Unofficial
            AAC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);
                self.status
                    .set(CpuFlags::CARRY, self.status.contains(CpuFlags::NEGATIVE));
            }
            AAX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                let res = self.reg_a & self.reg_x;
                self.write_u8(addr, res);
            }
            ARR => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
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
            }
            ASR => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let val = self.reg_a & self.read_u8(addr);
                self.reg_a = val >> 1;
                self.update_nz(self.reg_a);
                self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
            }
            ATX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a &= self.read_u8(addr);
                self.reg_x = self.reg_a;
                self.update_nz(self.reg_a);
            }
            AXA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                let res = self.reg_a & self.reg_x & 7;
                self.write_u8(addr, res);
            }
            AXS => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.reg_x &= self.reg_a;
                let sum = self.reg_x as u16 + (val ^ 0xFF) as u16 + 1;
                self.status.set(CpuFlags::CARRY, sum > 0xFF);
                self.reg_x = (sum & 0xFF) as u8;
                self.update_nz(self.reg_x);
            }
            DCP => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.write_u8(addr, val);
                let res = val.wrapping_sub(1);
                self.write_u8(addr, res);
                self.update_nz(res);
                let (res, car) = self.reg_a.overflowing_sub(res);
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            DOP => {
                let _ = self.operand_addr(op.mode, InstructionType::Read);
            }
            ISC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.write_u8(addr, val);
                let res = val.wrapping_add(1);
                self.write_u8(addr, res);
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
                self.exit_sig = true;
            }
            LAR => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.sp &= val;
                self.reg_a = self.sp;
                self.reg_x = self.sp;
                self.update_nz(self.reg_a);
            }
            LAX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.reg_a = val;
                self.reg_x = val;
                self.update_nz(val);
            }
            RLA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.write_u8(addr, val);
                let ocar = self.status.contains(CpuFlags::CARRY);
                let (res, car) = utils::rol(val, ocar);
                self.status.set(CpuFlags::CARRY, car);
                self.update_nz(res);
                self.write_u8(addr, res);
                self.reg_a &= res;
                self.update_nz(self.reg_a);
            }
            RRA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let ocar = self.status.contains(CpuFlags::CARRY);
                let val = self.read_u8(addr);
                self.write_u8(addr, val);
                let (res, car) = utils::ror(val, ocar);
                self.status.set(CpuFlags::CARRY, car);
                self.update_nz(res);
                self.write_u8(addr, res);
                let val = res;
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
            SLO => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let mut val = self.read_u8(addr);
                self.write_u8(addr, val);
                self.status.set(CpuFlags::CARRY, (val & 0x80) != 0);
                val <<= 1;
                self.update_nz(val);
                self.write_u8(addr, val);
                self.reg_a |= val;
                self.update_nz(self.reg_a);
            }
            SRE => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let mut val = self.read_u8(addr);
                self.write_u8(addr, val);
                self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
                val >>= 1;
                self.update_nz(val);
                self.write_u8(addr, val);
                self.reg_a ^= val;
                self.update_nz(self.reg_a);
            }
            SXA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                let hi = ((addr >> 8) as u8).wrapping_add(1);
                let res = self.reg_x & hi;
                self.write_u8(addr, res);
            }
            SYA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                let hi = ((addr >> 8) as u8).wrapping_add(1);
                let res = self.reg_y & hi;
                self.write_u8(addr, res);
            }
            TOP => {
                let _ = self.operand_addr(op.mode, InstructionType::Read);
            }
            XAA => {
                self.reg_a = self.reg_x;
                self.update_nz(self.reg_a);
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            XAS => {
                self.sp = self.reg_x & self.reg_a;
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                let hi = ((addr >> 8) as u8).wrapping_add(1);
                let res = self.sp & hi;
                self.write_u8(addr, res);
            }
        }
    }
}
