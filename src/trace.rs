use crate::{
    Mem,
    cpu::CPU,
    opcodes::{AddressMode, OPS, OpFamily},
};

pub fn trace(cpu: &mut CPU) -> String {
    let code = cpu.read_u8(cpu.pc);
    let op = OPS[code as usize].unwrap();

    let begin = cpu.pc;
    let mut hex_dump = vec![];
    hex_dump.push(code);

    // Temporarily increment PC to match execution state when reading operand address
    let saved_pc = cpu.pc;
    cpu.pc += 1;

    let (mem_addr, stored_value) = match op.mode {
        Some(AddressMode::IMM) | None => (0, 0),
        _ => cpu
            .operand_addr(op.mode, crate::cpu::InstructionType::Read)
            .map_or((0, 0), |addr| (addr, cpu.read_u8(addr))),
    };

    // Restore PC
    cpu.pc = saved_pc;

    let tmp = match op.len {
        1 => match op.code {
            0x0a | 0x4a | 0x2a | 0x6a => format!("A "),
            _ => String::from(""),
        },
        2 => {
            let address: u8 = cpu.read_u8(begin + 1);
            hex_dump.push(address);

            match op.mode {
                Some(AddressMode::IMM) => format!("#${:02x}", address),
                Some(AddressMode::ZP) => format!("${:02x} = {:02x}", address, stored_value), // FIX: use `address` not `mem_addr`
                Some(AddressMode::ZPX) => format!(
                    "${:02x},X @ {:02x} = {:02x}",
                    address, mem_addr, stored_value
                ),
                Some(AddressMode::ZPY) => format!(
                    "${:02x},Y @ {:02x} = {:02x}",
                    address, mem_addr, stored_value
                ),
                Some(AddressMode::INDX) => format!(
                    "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                    address,
                    (address.wrapping_add(cpu.reg_x)),
                    mem_addr,
                    stored_value
                ),
                Some(AddressMode::INDY) => format!(
                    "(${:02x}),Y = {:04x} @ {:04x} = {:02x}",
                    address,
                    (mem_addr.wrapping_sub(cpu.reg_y as u16)),
                    mem_addr,
                    stored_value
                ),
                Some(AddressMode::REL) | None => {
                    // assuming local jumps: BNE, BVS, etc....
                    let address: usize =
                        (begin as usize + 2).wrapping_add((address as i8) as usize);
                    format!("${:04x}", address)
                }

                _ => panic!(
                    "unexpected Address mode {:?} has ops-len 2. code {:02x}",
                    op.mode, op.code
                ),
            }
        }
        3 => {
            let address_lo = cpu.read_u8(begin + 1);
            let address_hi = cpu.read_u8(begin + 2);
            hex_dump.push(address_lo);
            hex_dump.push(address_hi);

            let address = cpu.read_u16(begin + 1);

            match op.mode {
                None => {
                    format!("${:04x}", address) // This is already correct
                }
                Some(AddressMode::IND) => {
                    //jmp indirect
                    let jmp_addr = if address & 0x00FF == 0x00FF {
                        let lo = cpu.read_u8(address);
                        let hi = cpu.read_u8(address & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        cpu.read_u16(address)
                    };

                    format!("(${:04x}) = {:04x}", address, jmp_addr)
                }
                Some(AddressMode::ABS) => match op.family {
                    OpFamily::JMP | OpFamily::JSR => format!("${:04x}", address),
                    _ => format!("${:04x} = {:02x}", address, stored_value),
                },
                Some(AddressMode::ABSX) => format!(
                    "${:04x},X @ {:04x} = {:02x}",
                    address, mem_addr, stored_value
                ),
                Some(AddressMode::ABSY) => format!(
                    "${:04x},Y @ {:04x} = {:02x}",
                    address, mem_addr, stored_value
                ),
                _ => panic!(
                    "unexpected Address mode {:?} has ops-len 3. code {:02x}",
                    op.mode, op.code
                ),
            }
        }
        _ => String::from(""),
    };

    let hex_str = hex_dump
        .iter()
        .map(|z| format!("{:02x}", z))
        .collect::<Vec<String>>()
        .join(" ");
    let asm_str = format!("{:04x}  {:8} {: >4} {}", begin, hex_str, op.mnemonic, tmp)
        .trim()
        .to_string();

    format!(
        "{:47} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x} PPU:{: >3},{: >3} CYC:{}",
        asm_str,
        cpu.reg_a,
        cpu.reg_x,
        cpu.reg_y,
        cpu.status,
        cpu.sp,
        cpu.bus.ppu.scanline,
        cpu.bus.ppu.cycles,
        cpu.cycles
    )
    .to_ascii_uppercase()
}
