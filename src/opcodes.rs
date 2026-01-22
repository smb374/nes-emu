use AddressMode::*;
use OpFamily::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Op {
    pub code: u8,
    pub family: OpFamily,
    pub mode: Option<AddressMode>,
    pub len: u8,
    pub cycles: u16,
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
    ($code:expr, $family:ident, $mode:ident, $len:expr, $cycles:expr, $mnemonic:expr) => {
        Some(Op {
            code: $code,
            family: $family,
            mode: Some($mode),
            len: $len,
            cycles: $cycles,
            mnemonic: $mnemonic,
        })
    };
    ($code:expr, $family:ident, $len:expr, $cycles:expr, $mnemonic:expr) => {
        Some(Op {
            code: $code,
            family: $family,
            mode: None,
            len: $len,
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
