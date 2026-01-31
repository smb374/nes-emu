#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapperType {
    NROM,
    MMC1(MMC1State),
    UxROM(UxROMState),
    CNROM(CNROMState),
    MMC3(MMC3State),
}

impl MapperType {
    pub fn mapper_mirroring(&self) -> Option<Mirroring> {
        match self {
            MapperType::MMC1(s) => Some(s.mirroring()),
            MapperType::MMC3(s) => Some(if s.is_four_screen {
                Mirroring::FourScreen
            } else {
                if s.arr_select {
                    Mirroring::Horizontal
                } else {
                    Mirroring::Vertical
                }
            }),
            _ => None, // Use hardware mirroring from ROM header
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MMC1Subtype {
    /// Standard boards - normal PRG/CHR banking
    Standard,
    /// SEROM, SHROM, SH1ROM - 32 KiB unbanked PRG-ROM
    Unbanked,
    /// SNROM, SOROM, SUROM, SXROM - 8 KiB CHR, uses CHR registers for extended banking
    /// Handles all variants with 8K CHR (SNROM's write protect, SOROM/SXROM PRG-RAM banking, SUROM 512K PRG)
    Sxrom,
    /// SZROM - 16+ KiB CHR ROM, uses CHR bit 4 for PRG-RAM banking
    Szrom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MMC1State {
    // Internal shift register
    pub shift_register: u8,
    pub shift_count: u8,

    // Internal registers
    pub control: u8,    // $8000-$9FFF
    pub chr_bank_0: u8, // $A000-$BFFF
    pub chr_bank_1: u8, // $C000-$DFFF
    pub prg_bank: u8,   // $E000-$FFFF

    // Maps
    pub prg_map: [u8; 2],
    pub chr_map: [u8; 2],

    // Configuration
    is_a_variant: bool,
    subtype: MMC1Subtype,
    prg_pages: u8,
    chr_pages: u8,
}

impl MMC1State {
    pub fn new(mapper: u8, prg_pages: u8, chr_pages: u8) -> Self {
        let subtype = Self::detect_subtype(prg_pages, chr_pages);

        println!("subtype={:?}", subtype);

        Self {
            shift_register: 0,
            shift_count: 0,
            control: 0x0C,
            chr_bank_0: 0,
            chr_bank_1: 0,
            prg_bank: 0,
            prg_map: [0, prg_pages.saturating_sub(1)],
            chr_map: [0, 1],
            is_a_variant: mapper == 155,
            subtype,
            prg_pages,
            chr_pages,
        }
    }

    fn detect_subtype(prg_pages: u8, chr_pages: u8) -> MMC1Subtype {
        // Unbanked: 32 KiB PRG-ROM (2 pages)
        if prg_pages == 2 {
            return MMC1Subtype::Unbanked;
        }

        // SZROM: 16+ KiB CHR-ROM (2+ pages)
        if chr_pages >= 2 {
            return MMC1Subtype::Szrom;
        }

        // SXROM variants: exactly 8 KiB CHR (1 page) or CHR-RAM
        if chr_pages <= 1 {
            return MMC1Subtype::Sxrom;
        }

        MMC1Subtype::Standard
    }

    pub fn map_pages(&mut self) {
        // For Unbanked, PRG banking is completely disabled
        if matches!(self.subtype, MMC1Subtype::Unbanked) {
            self.prg_map = [0, 1];
            self.update_chr_mapping();
            return;
        }

        // Standard PRG banking
        match self.prg_mode() {
            0 | 1 => {
                // 32 KB mode
                let page = self.prg_bank & 0x0E;
                self.prg_map = [page, page + 1];
            }
            2 => {
                // Fix first bank at $8000, switch 16 KB at $C000
                let page = self.prg_bank & 0x0F;
                self.prg_map = [0, page];
            }
            3 => {
                // Fix last bank at $C000, switch 16 KB at $8000
                let page = self.prg_bank & 0x0F;
                self.prg_map = [page, self.prg_pages - 1];
            }
            _ => unreachable!(),
        }

        // Apply extended PRG-ROM banking for SXROM (covers SUROM's 512K support)
        if matches!(self.subtype, MMC1Subtype::Sxrom) {
            // CHR bank 0 bit 4 = PRG-ROM A18 (selects 256 KB half for 512 KB ROMs)
            let prg_outer_bank = ((self.chr_bank_0 >> 4) & 0x01) as u8;
            self.prg_map[0] = (prg_outer_bank << 4) | (self.prg_map[0] & 0x0F);
            self.prg_map[1] = (prg_outer_bank << 4) | (self.prg_map[1] & 0x0F);
        }

        self.update_chr_mapping();
    }

    fn update_chr_mapping(&mut self) {
        if self.chr_mode() {
            // 4 KiB mode
            let page0 = self.get_chr_page(0);
            let page1 = self.get_chr_page(1);
            self.chr_map = [page0, page1];
        } else {
            // 8 KiB mode
            let page = self.get_chr_page(0) & 0x1E;
            self.chr_map = [page, page + 1];
        }
    }

    fn get_chr_page(&self, bank: u8) -> u8 {
        let value = if bank == 0 {
            self.chr_bank_0
        } else {
            self.chr_bank_1
        };

        match self.subtype {
            MMC1Subtype::Sxrom => value & 0x01, // Only 8 KiB CHR, 1 bit for banking
            MMC1Subtype::Szrom => value & 0x0F, // Up to 128 KiB CHR, 4 bits
            _ => value & 0x1F,                  // Standard: up to 256 KiB CHR, 5 bits
        }
    }

    pub fn get_prg_ram_bank(&self) -> u8 {
        match self.subtype {
            MMC1Subtype::Sxrom => {
                // SXROM: CHR bank 0 bits 2-3 select PRG-RAM bank
                // SOROM: Only bit 3 matters (2 banks)
                // SUROM/SNROM: No PRG-RAM banking, always bank 0
                // For simplicity, just use bits 2-3 which works for all variants
                (self.chr_bank_0 >> 2) & 0x03
            }
            MMC1Subtype::Szrom => {
                // SZROM: CHR bank 0 bit 4 selects PRG-RAM bank (2 banks)
                (self.chr_bank_0 >> 4) & 0x01
            }
            _ => 0, // Standard boards: no PRG-RAM banking
        }
    }

    pub fn is_prg_ram_enabled(&self) -> bool {
        match self.subtype {
            MMC1Subtype::Sxrom => {
                // SXROM variants (including SNROM): CHR bank bit 4 is PRG-RAM enable
                // In 4KB CHR mode, both registers should match (but we simplify by checking bank 0)
                // 0 = enabled, 1 = disabled
                ((self.chr_bank_0 >> 4) & 0x01) == 0
            }
            _ if self.is_a_variant => {
                // MMC1A: PRG-RAM always enabled
                true
            }
            _ => {
                // MMC1B Standard: PRG bank register bit 4 controls enable
                // 0 = enabled, 1 = disabled
                ((self.prg_bank >> 4) & 0x01) == 0
            }
        }
    }

    pub fn mirroring(&self) -> Mirroring {
        match self.control & 0x03 {
            0 => Mirroring::SingleScreenLower,
            1 => Mirroring::SingleScreenUpper,
            2 => Mirroring::Vertical,
            3 => Mirroring::Horizontal,
            _ => unreachable!(),
        }
    }

    pub fn prg_mode(&self) -> u8 {
        (self.control >> 2) & 0x03
    }

    pub fn chr_mode(&self) -> bool {
        ((self.control >> 4) & 0x01) != 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MMC3State {
    pub banks: [u8; 8],
    pub chr_map: [u8; 8], // 1KB banks
    pub prg_map: [u8; 4], // 8KB banks
    pub command: u8,
    pub chr_swap: bool,
    pub prg_swap: bool,

    // IRQ state
    pub irq_latch: u8,
    pub irq_counter: u8,
    pub irq_reload: bool,
    pub irq_enabled: bool,

    // Other
    pub arr_select: bool,
    pub prg_ram_enable: bool,
    pub prg_banks: u8,
    pub chr_banks: u8,
    pub is_four_screen: bool,
}

impl MMC3State {
    pub fn new(prg_pages: u8, chr_pages: u8, is_four_screen: bool) -> Self {
        let last_page = (prg_pages * 2) - 1;
        let banks = if chr_pages == 0 { 8 } else { chr_pages * 8 };
        Self {
            banks: [0u8; 8],
            chr_map: [0, 1, 2, 3, 4, 5, 6, 7],
            prg_map: [0, 1, last_page - 1, last_page],
            command: 0,
            chr_swap: false,
            prg_swap: false,
            irq_latch: 0,
            irq_counter: 0,
            irq_reload: false,
            irq_enabled: false,
            arr_select: false,
            prg_ram_enable: true,
            prg_banks: prg_pages * 2,
            chr_banks: banks,
            is_four_screen,
        }
    }
    pub fn map_pages(&mut self) {
        if self.chr_swap {
            self.chr_map = [
                self.banks[2] % self.chr_banks,
                self.banks[3] % self.chr_banks,
                self.banks[4] % self.chr_banks,
                self.banks[5] % self.chr_banks,
                self.banks[0] % self.chr_banks,
                (self.banks[0] + 1) % self.chr_banks,
                self.banks[1] % self.chr_banks,
                (self.banks[1] + 1) % self.chr_banks,
            ];
        } else {
            self.chr_map = [
                self.banks[0] % self.chr_banks,
                (self.banks[0] + 1) % self.chr_banks,
                self.banks[1] % self.chr_banks,
                (self.banks[1] + 1) % self.chr_banks,
                self.banks[2] % self.chr_banks,
                self.banks[3] % self.chr_banks,
                self.banks[4] % self.chr_banks,
                self.banks[5] % self.chr_banks,
            ];
        }
        if self.prg_swap {
            self.prg_map = [
                self.prg_banks - 2,
                self.banks[7] % self.prg_banks,
                self.banks[6] % self.prg_banks,
                self.prg_banks - 1,
            ];
        } else {
            self.prg_map = [
                self.banks[6] % self.prg_banks,
                self.banks[7] % self.prg_banks,
                self.prg_banks - 2,
                self.prg_banks - 1,
            ];
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UxROMState {
    pub prg_bank: u8, // Switchable bank at $8000
}

impl Default for UxROMState {
    fn default() -> Self {
        Self { prg_bank: 0 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CNROMState {
    pub chr_bank: u8, // CHR bank select
}

impl Default for CNROMState {
    fn default() -> Self {
        Self { chr_bank: 0 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    SingleScreenLower,
    SingleScreenUpper,
    FourScreen,
}

impl Default for Mirroring {
    fn default() -> Self {
        Self::Horizontal
    }
}
