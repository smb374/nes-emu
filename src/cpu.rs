use std::sync::atomic::AtomicBool;

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

pub static GLOBAL_EXIT: AtomicBool = AtomicBool::new(false);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InterruptType {
    BRK,
    IRQ,
    NMI,
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
    nmi_delay: Option<bool>,
    nmi_latch: Option<u8>,
    addr: Option<u16>,
}

impl<'a> Mem for CPU<'a> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        self.bus.set_writing(false);
        self.tick();
        while !self.bus.rdy() {
            if !self.bus.is_dma_xfer() {
                self.bus.read_u8(addr);
            }
            self.bus.set_writing(false);
            self.tick();
        }
        self.bus.read_u8(addr)
    }
    fn write_u8(&mut self, addr: u16, val: u8) {
        self.bus.set_writing(true);
        self.bus.write_u8(addr, val);
        self.tick();
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
            nmi_delay: None,
            nmi_latch: None,
            addr: None,
        }
    }

    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.sp = STACK_RESET;
        self.status = CpuFlags::BREAK2 | CpuFlags::INTR_DISABLE;
        self.cycles = 0;

        for _ in 0..5 {
            self.tick();
        }
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
            if GLOBAL_EXIT.load(std::sync::atomic::Ordering::Acquire) {
                break;
            }

            cb(self);

            let opcode = self.read_u8(self.pc);
            self.pc += 1;
            let pc_cache = self.pc;
            if let Some(op) = OPS[opcode as usize] {
                let prev_intr_disable = self.status.contains(CpuFlags::INTR_DISABLE);
                self.run_op(op);
                if let Some(addr) = self.addr.take() {
                    log::trace!(
                        "{:04X} {} ${:04X} A:{:02X} X:{:02X} Y:{:02X} F:{:02X}",
                        pc_cache - 1,
                        op.mnemonic,
                        addr,
                        self.reg_a,
                        self.reg_x,
                        self.reg_y,
                        self.status.bits()
                    );
                } else {
                    log::trace!(
                        "{:04X} {}       A:{:02X} X:{:02X} Y:{:02X} F:{:02X}",
                        pc_cache - 1,
                        op.mnemonic,
                        self.reg_a,
                        self.reg_x,
                        self.reg_y,
                        self.status.bits()
                    );
                }
                if self.exit_sig {
                    break;
                }
                if pc_cache == self.pc {
                    self.pc += (op.len - 1) as u16;
                }

                if let Some(s) = self.nmi_latch.take() {
                    if s == 2 {
                        self.nmi_delay = Some(true);
                    } else {
                        self.interrupt_nmi();
                    }
                } else if self.nmi_delay == Some(false) {
                    self.interrupt_nmi();
                    self.nmi_delay = None;
                } else if self.irq_sig {
                    let interrupts_disabled = if op.family == RTI {
                        self.status.contains(CpuFlags::INTR_DISABLE)
                    } else {
                        prev_intr_disable
                    };

                    if !interrupts_disabled {
                        self.interrupt_irq();
                    }
                }

                self.irq_sig = false;
                if self.nmi_delay == Some(true) {
                    self.nmi_delay = Some(false);
                }
            } else {
                panic!("Unknown op: {}", opcode);
            }
        }
    }

    fn tick(&mut self) {
        self.exit_sig |= self.bus.tick();
        self.cycles += 1;
        if self.nmi_latch.is_none() {
            self.nmi_latch = self.bus.poll_nmi_status();
        }
    }

    fn interrupt_sequence(&mut self, intr_type: InterruptType) {
        let pc_buf = self.pc.to_le_bytes();
        self.poll_intr();
        self.push_stack(pc_buf[1]);
        self.poll_intr();
        self.push_stack(pc_buf[0]);
        self.poll_intr();

        let is_brk = intr_type == InterruptType::BRK;
        let mut status = self.status;
        status.set(CpuFlags::BREAK, is_brk);
        status.insert(CpuFlags::BREAK2);
        self.push_stack(status.bits());

        let vector = match intr_type {
            InterruptType::BRK | InterruptType::IRQ => {
                if let Some(s) = self.nmi_latch.take() {
                    if s == 2 {
                        self.nmi_delay = Some(true);
                        0xFFFE
                    } else {
                        0xFFFA
                    }
                } else {
                    0xFFFE
                }
            }
            InterruptType::NMI => 0xFFFA,
        };

        self.status.insert(CpuFlags::INTR_DISABLE);
        self.pc = self.read_u16(vector);
        match intr_type {
            InterruptType::BRK => log::info!("/BRK -> {:04X}", self.pc),
            InterruptType::IRQ => log::info!("/IRQ -> {:04X}", self.pc),
            InterruptType::NMI => log::info!("/NMI -> {:04X}", self.pc),
        }
    }

    fn interrupt_nmi(&mut self) {
        self.read_u8(self.pc);
        self.read_u8(self.pc);
        self.interrupt_sequence(InterruptType::NMI);
    }

    fn interrupt_irq(&mut self) {
        self.read_u8(self.pc);
        self.read_u8(self.pc);
        self.interrupt_sequence(InterruptType::IRQ);
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

    fn poll_intr(&mut self) {
        self.irq_sig |= self.bus.poll_irq_status();
    }

    fn branch_if(&mut self, condition: bool) {
        self.poll_intr();
        let offset = self.read_u8(self.pc) as i8 as i16;
        self.pc = self.pc.wrapping_add(1);
        if condition {
            self.bus.read_u8(self.pc);
            self.tick();

            let old_pc = self.pc;
            let new_pc = self.pc.wrapping_add_signed(offset);
            self.addr = Some(new_pc);

            if Self::page_crossed(old_pc, new_pc) {
                let wrong_pc = (old_pc & 0xFF00) | (new_pc & 0x00FF);
                self.poll_intr();
                self.bus.read_u8(wrong_pc);
                self.tick();
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
        let addr = match mode {
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
        };
        self.addr = addr;
        addr
    }

    fn run_op(&mut self, op: Op) {
        match op.family {
            ADC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
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
                self.poll_intr();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            ASL => {
                if op.mode == Some(ACC) {
                    self.status.set(CpuFlags::CARRY, (self.reg_a & 0x80) != 0);
                    self.reg_a <<= 1;
                    self.update_nz(self.reg_a);
                    self.poll_intr();
                    self.tick();
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let mut val = self.read_u8(addr);
                    self.write_u8(addr, val);
                    self.status.set(CpuFlags::CARRY, (val & 0x80) != 0);
                    val <<= 1;
                    self.update_nz(val);
                    self.poll_intr();
                    self.write_u8(addr, val);
                }
            }
            BCC => self.branch_if(!self.status.contains(CpuFlags::CARRY)),
            BCS => self.branch_if(self.status.contains(CpuFlags::CARRY)),
            BEQ => self.branch_if(self.status.contains(CpuFlags::ZERO)),
            BIT => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
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
                self.poll_intr();
                self.read_u8(self.pc);
                self.pc += 1;
                self.interrupt_sequence(InterruptType::BRK);
            }
            BVC => self.branch_if(!self.status.contains(CpuFlags::OVERFLOW)),
            BVS => self.branch_if(self.status.contains(CpuFlags::OVERFLOW)),
            CLC => {
                self.status &= !CpuFlags::CARRY;
                self.poll_intr();
                self.tick();
            }
            CLD => {
                self.status &= !CpuFlags::DECIMAL;
                self.poll_intr();
                self.tick();
            }
            CLI => {
                self.poll_intr();
                self.status &= !CpuFlags::INTR_DISABLE;
                self.tick();
            }
            CLV => {
                self.status &= !CpuFlags::OVERFLOW;
                self.poll_intr();
                self.tick();
            }
            CMP => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                let (res, car) = self.reg_a.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            CPX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                let (res, car) = self.reg_x.overflowing_sub(self.read_u8(addr));
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            CPY => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
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
                self.poll_intr();
                self.write_u8(addr, res);
                self.update_nz(res);
            }
            DEX => {
                self.reg_x = self.reg_x.wrapping_sub(1);
                self.update_nz(self.reg_x);
                self.poll_intr();
                self.tick();
            }
            DEY => {
                self.reg_y = self.reg_y.wrapping_sub(1);
                self.update_nz(self.reg_y);
                self.poll_intr();
                self.tick();
            }
            EOR => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.reg_a ^= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            INC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let val = self.read_u8(addr);
                self.write_u8(addr, val);
                let res = val.wrapping_add(1);
                self.poll_intr();
                self.write_u8(addr, res);
                self.update_nz(res);
            }
            INX => {
                self.reg_x = self.reg_x.wrapping_add(1);
                self.update_nz(self.reg_x);
                self.poll_intr();
                self.tick();
            }
            INY => {
                self.reg_y = self.reg_y.wrapping_add(1);
                self.update_nz(self.reg_y);
                self.poll_intr();
                self.tick();
            }
            JMP => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                self.poll_intr();
                self.pc = addr_opt.unwrap();
            }
            JSR => {
                let lo = self.read_u8(self.pc) as u16;
                self.pc = self.pc.wrapping_add(1);
                self.bus.read_u8(STACK_BASE + self.sp as u16);
                self.tick();
                self.push_stack_u16(self.pc);
                self.poll_intr();
                let hi = self.read_u8(self.pc) as u16;
                self.pc = (hi << 8) | lo;
                self.addr = Some(self.pc);
            }
            LDA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.reg_a = self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            LDX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.reg_x = self.read_u8(addr);
                self.update_nz(self.reg_x);
            }
            LDY => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.reg_y = self.read_u8(addr);
                self.update_nz(self.reg_y);
            }
            LSR => {
                if op.mode == Some(ACC) {
                    self.status.set(CpuFlags::CARRY, (self.reg_a & 0x01) != 0);
                    self.reg_a >>= 1;
                    self.update_nz(self.reg_a);
                    self.poll_intr();
                    self.tick();
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let mut val = self.read_u8(addr);
                    self.write_u8(addr, val);
                    self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
                    val >>= 1;
                    self.update_nz(val);
                    self.poll_intr();
                    self.write_u8(addr, val);
                }
            }
            NOP => {
                self.poll_intr();
                self.read_u8(self.pc);
            }
            ORA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.reg_a |= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            PHA => {
                self.push_stack(self.reg_a);
                self.poll_intr();
                self.tick();
            }
            PHP => {
                self.push_stack((self.status | CpuFlags::BREAK | CpuFlags::BREAK2).bits());
                self.poll_intr();
                self.tick();
            }
            PLA => {
                self.reg_a = self.pop_stack();
                self.update_nz(self.reg_a);
                self.tick();
                self.poll_intr();
                self.tick();
            }
            PLP => {
                // PLP: 4 cycles. (1) opcode, (2) dummy read, (3) inc SP, (4) pull flags.
                // Polling happens before the flags are pulled off the stack,
                // so it uses the OLD I flag value.
                self.tick(); // cycle 2: dummy read
                self.sp = self.sp.wrapping_add(1); // cycle 3: increment SP
                self.tick();
                self.poll_intr(); // poll with old I flag
                let mut status = CpuFlags::from_bits_retain(
                    self.read_u8(STACK_BASE + self.sp as u16), // cycle 4: pull flags
                );
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
                    self.poll_intr();
                    self.tick();
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let val = self.read_u8(addr);
                    self.write_u8(addr, val);
                    let (res, car) = utils::rol(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.poll_intr();
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
                    self.poll_intr();
                    self.tick();
                } else {
                    let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                    let addr = addr_opt.unwrap();
                    let val = self.read_u8(addr);
                    self.write_u8(addr, val);
                    let (res, car) = utils::ror(val, ocar);
                    self.status.set(CpuFlags::CARRY, car);
                    self.update_nz(res);
                    self.poll_intr();
                    self.write_u8(addr, res);
                }
            }
            RTI => {
                self.tick();
                self.tick();
                let mut status = CpuFlags::from_bits_retain(self.pop_stack());
                status &= !CpuFlags::BREAK;
                status |= CpuFlags::BREAK2;
                self.status = status;
                let pc_lo = self.pop_stack();
                self.poll_intr();
                let pc_hi = self.pop_stack();
                self.pc = u16::from_le_bytes([pc_lo, pc_hi]);
            }
            RTS => {
                self.tick();
                self.tick();
                self.pc = self.pop_stack_u16() + 1;
                self.poll_intr();
                self.tick();
            }
            SBC => {
                // ADC with inverted operand
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
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
                self.poll_intr();
                self.tick();
            }
            SED => {
                self.status |= CpuFlags::DECIMAL;
                self.poll_intr();
                self.tick();
            }
            SEI => {
                self.poll_intr();
                self.status |= CpuFlags::INTR_DISABLE;
                self.tick();
            }
            STA => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.write_u8(addr, self.reg_a);
            }
            STX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.write_u8(addr, self.reg_x);
            }
            STY => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.write_u8(addr, self.reg_y);
            }
            TAX => {
                self.reg_x = self.reg_a;
                self.update_nz(self.reg_x);
                self.poll_intr();
                self.tick();
            }
            TAY => {
                self.reg_y = self.reg_a;
                self.update_nz(self.reg_y);
                self.poll_intr();
                self.tick();
            }
            TSX => {
                self.reg_x = self.sp;
                self.update_nz(self.reg_x);
                self.poll_intr();
                self.tick();
            }
            TXA => {
                self.reg_a = self.reg_x;
                self.update_nz(self.reg_a);
                self.poll_intr();
                self.tick();
            }
            TXS => {
                self.sp = self.reg_x;
                self.poll_intr();
                self.tick();
            }
            TYA => {
                self.reg_a = self.reg_y;
                self.update_nz(self.reg_a);
                self.poll_intr();
                self.tick();
            }
            // Unofficial
            AAC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);
                self.status
                    .set(CpuFlags::CARRY, self.status.contains(CpuFlags::NEGATIVE));
            }
            AAX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Write);
                let addr = addr_opt.unwrap();
                let res = self.reg_a & self.reg_x;
                self.poll_intr();
                self.write_u8(addr, res);
            }
            ARR => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
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
                self.poll_intr();
                let val = self.reg_a & self.read_u8(addr);
                self.reg_a = val >> 1;
                self.update_nz(self.reg_a);
                self.status.set(CpuFlags::CARRY, (val & 0x01) != 0);
            }
            ATX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.reg_a |= 0xFF;
                self.poll_intr();
                self.reg_a &= self.read_u8(addr);
                self.reg_x = self.reg_a;
                self.update_nz(self.reg_a);
            }
            AXA => {
                match op.mode {
                    Some(ABSY) => {
                        let lo = self.read_u8(self.pc) as u16;
                        let hi = self.read_u8(self.pc.wrapping_add(1)) as u16;

                        let base = (hi << 8) | lo;
                        let addr = base.wrapping_add(self.reg_y as u16);
                        let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                        let wrong_addr = (hi << 8) | ((lo + self.reg_y as u16) & 0xFF);
                        self.read_u8(wrong_addr);

                        let res = self.reg_a & self.reg_x & ((hi as u8).wrapping_add(1));

                        let write_addr = if page_crossed {
                            ((res as u16) << 8) | (addr & 0xFF)
                        } else {
                            addr
                        };
                        self.poll_intr();
                        self.write_u8(write_addr, res);
                    }
                    Some(INDY) => {
                        let ptr = self.read_u8(self.pc);
                        let lo = self.read_u8(ptr as u16) as u16;
                        let hi = self.read_u8(ptr.wrapping_add(1) as u16) as u16;

                        let base = (hi << 8) | lo;
                        let addr = base.wrapping_add(self.reg_y as u16);
                        let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                        // Always do dummy read for write operations
                        let wrong_addr = (hi << 8) | ((lo + self.reg_y as u16) & 0xFF);
                        self.read_u8(wrong_addr);

                        let res = self.reg_a & self.reg_x & ((hi as u8).wrapping_add(1));

                        // If page crossed, the actual address written is also affected
                        let write_addr = if page_crossed {
                            ((res as u16) << 8) | (addr & 0xFF)
                        } else {
                            addr
                        };
                        self.poll_intr();
                        self.write_u8(write_addr, res);
                    }
                    _ => unreachable!(),
                }
            }
            AXS => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
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
                self.poll_intr();
                self.write_u8(addr, res);
                self.update_nz(res);
                let (res, car) = self.reg_a.overflowing_sub(res);
                self.status.set(CpuFlags::CARRY, !car);
                self.update_nz(res);
            }
            DOP => {
                let addr = self.operand_addr(op.mode, InstructionType::Read);
                self.poll_intr();
                self.read_u8(addr.unwrap_or(self.pc));
            }
            ISC => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Rmw);
                let addr = addr_opt.unwrap();
                let mut val = self.read_u8(addr);
                self.write_u8(addr, val);
                val = val.wrapping_add(1);
                self.poll_intr();
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
                self.exit_sig = true;
            }
            LAR => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                let val = self.read_u8(addr);
                self.sp &= val;
                self.reg_a = self.sp;
                self.reg_x = self.sp;
                self.update_nz(self.reg_a);
            }
            LAX => {
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
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
                self.poll_intr();
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
                self.poll_intr();
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
                self.poll_intr();
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
                self.poll_intr();
                self.write_u8(addr, val);
                self.reg_a ^= val;
                self.update_nz(self.reg_a);
            }
            SXA => {
                // SXA abs,Y - Store X & (H+1) where H is high byte of base address
                let lo = self.read_u8(self.pc) as u16;
                let hi = self.read_u8(self.pc.wrapping_add(1)) as u16;

                let base = (hi << 8) | lo;
                let addr = base.wrapping_add(self.reg_y as u16);
                let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                // Always do dummy read for write operations
                let wrong_addr = (hi << 8) | ((lo + self.reg_y as u16) & 0xFF);
                self.read_u8(wrong_addr);

                let res = self.reg_x & ((hi as u8).wrapping_add(1));

                // If page crossed, the actual address written is also affected
                let write_addr = if page_crossed {
                    ((res as u16) << 8) | (addr & 0xFF)
                } else {
                    addr
                };
                self.poll_intr();
                self.write_u8(write_addr, res);
            }
            SYA => {
                // SYA abs,X - Store Y & (H+1) where H is high byte of base address
                let lo = self.read_u8(self.pc) as u16;
                let hi = self.read_u8(self.pc.wrapping_add(1)) as u16;

                let base = (hi << 8) | lo;
                let addr = base.wrapping_add(self.reg_x as u16);
                let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                // Always do dummy read for write operations
                let wrong_addr = (hi << 8) | ((lo + self.reg_x as u16) & 0xFF);
                self.read_u8(wrong_addr);

                let res = self.reg_y & ((hi as u8).wrapping_add(1));

                // If page crossed, the actual address written is also affected
                let write_addr = if page_crossed {
                    ((res as u16) << 8) | (addr & 0xFF)
                } else {
                    addr
                };
                self.poll_intr();
                self.write_u8(write_addr, res);
            }
            TOP => {
                let addr = self.operand_addr(op.mode, InstructionType::Read);
                self.read_u8(addr.unwrap_or(self.pc));
            }
            XAA => {
                self.reg_a = self.reg_x;
                self.update_nz(self.reg_a);
                let addr_opt = self.operand_addr(op.mode, InstructionType::Read);
                let addr = addr_opt.unwrap();
                self.poll_intr();
                self.reg_a &= self.read_u8(addr);
                self.update_nz(self.reg_a);
            }
            XAS => {
                // XAS abs,Y - Transfer A AND X to SP, then store in memory
                self.sp = self.reg_x & self.reg_a;

                let lo = self.read_u8(self.pc) as u16;
                let hi = self.read_u8(self.pc.wrapping_add(1)) as u16;

                let base = (hi << 8) | lo;
                let addr = base.wrapping_add(self.reg_y as u16);
                let page_crossed = (base & 0xFF00) != (addr & 0xFF00);

                // Always do dummy read for write operations
                let wrong_addr = (hi << 8) | ((lo + self.reg_y as u16) & 0xFF);
                self.read_u8(wrong_addr);

                let res = self.sp & ((hi as u8).wrapping_add(1));

                // If page crossed, the actual address written is also affected
                let write_addr = if page_crossed {
                    ((res as u16) << 8) | (addr & 0xFF)
                } else {
                    addr
                };
                self.poll_intr();
                self.write_u8(write_addr, res);
            }
        }
    }
}
