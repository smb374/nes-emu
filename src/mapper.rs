const PRG_RAM_8K: usize = 0x2000;
const PRG_RAM_32K: usize = 0x8000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MapperType {
    NROM(NROMState),
    MMC1(MMC1State),
    UxROM(UxROMState),
    CNROM(CNROMState),
    MMC3(MMC3State),
    AxROM(AxROMState),
}

impl MapperType {
    pub fn mapper_mirroring(&self) -> Option<Mirroring> {
        match self {
            MapperType::MMC1(s) => Some(s.mirroring()),
            MapperType::MMC3(s) => Some(if s.is_four_screen {
                Mirroring::FourScreen
            } else if s.arr_select {
                Mirroring::Horizontal
            } else {
                Mirroring::Vertical
            }),
            MapperType::AxROM(s) => Some(s.mirroring),
            _ => None, // Use hardware mirroring from ROM header
        }
    }

    pub fn read_prg_ram(&self, addr: u16) -> u8 {
        match self {
            MapperType::NROM(state) => state.read_prg_ram(addr),
            MapperType::MMC1(state) => state.read_prg_ram(addr),
            MapperType::UxROM(_) | MapperType::CNROM(_) | MapperType::AxROM(_) => 0,
            MapperType::MMC3(state) => state.read_prg_ram(addr),
        }
    }

    pub fn write_prg_ram(&mut self, addr: u16, val: u8) {
        match self {
            MapperType::NROM(state) => state.write_prg_ram(addr, val),
            MapperType::MMC1(state) => state.write_prg_ram(addr, val),
            MapperType::UxROM(_) | MapperType::CNROM(_) | MapperType::AxROM(_) => {}
            MapperType::MMC3(state) => state.write_prg_ram(addr, val),
        }
    }

    pub fn has_prg_ram(&self) -> bool {
        match self {
            MapperType::UxROM(_) | MapperType::CNROM(_) | MapperType::AxROM(_) => false,
            _ => true,
        }
    }

    pub fn get_prg_ram_data(&self) -> &[u8] {
        match self {
            MapperType::NROM(state) => &state.prg_ram,
            MapperType::MMC1(state) => &state.prg_ram,
            MapperType::MMC3(state) => &state.prg_ram,
            MapperType::UxROM(_) | MapperType::CNROM(_) | MapperType::AxROM(_) => &[],
        }
    }

    pub fn load_prg_ram_data(&mut self, data: &[u8]) -> Result<(), String> {
        match self {
            MapperType::NROM(state) => {
                let len = data.len().min(state.prg_ram.len());
                state.prg_ram[..len].copy_from_slice(&data[..len]);
                Ok(())
            }
            MapperType::MMC1(state) => {
                let len = data.len().min(state.prg_ram.len());
                state.prg_ram[..len].copy_from_slice(&data[..len]);
                Ok(())
            }
            MapperType::MMC3(state) => {
                let len = data.len().min(state.prg_ram.len());
                state.prg_ram[..len].copy_from_slice(&data[..len]);
                Ok(())
            }
            MapperType::UxROM(_) | MapperType::CNROM(_) | MapperType::AxROM(_) => {
                Err("Mapper does not have PRG-RAM".to_string())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NROMState {
    pub prg_ram: Box<[u8]>,
}

impl NROMState {
    pub fn new() -> Self {
        Self {
            prg_ram: vec![0u8; PRG_RAM_8K].into_boxed_slice(),
        }
    }

    fn read_prg_ram(&self, addr: u16) -> u8 {
        self.prg_ram[(addr - 0x6000) as usize % self.prg_ram.len()]
    }

    fn write_prg_ram(&mut self, addr: u16, val: u8) {
        let idx = (addr - 0x6000) as usize % self.prg_ram.len();
        self.prg_ram[idx] = val;
    }
}

impl Default for NROMState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MMC1Subtype {
    /// Standard boards - normal PRG/CHR banking
    Standard,
    /// SEROM, SHROM, SH1ROM - 32 KiB unbanked PRG-ROM
    Unbanked,
    /// SNROM, SOROM, SUROM, SXROM - 8 KiB CHR, uses CHR registers for extended banking
    Sxrom,
    /// SZROM - 16+ KiB CHR ROM, uses CHR bit 4 for PRG-RAM banking
    Szrom,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    // PRG-RAM (8KB to 32KB depending on board)
    pub prg_ram: Box<[u8]>,

    // Configuration
    is_a_variant: bool,
    subtype: MMC1Subtype,
    prg_pages: u8,
    chr_pages: u8,
}

impl MMC1State {
    pub fn new(mapper: u8, prg_pages: u8, chr_pages: u8) -> Self {
        let subtype = Self::detect_subtype(prg_pages, chr_pages);

        Self {
            shift_register: 0,
            shift_count: 0,
            control: 0x0C,
            chr_bank_0: 0,
            chr_bank_1: 0,
            prg_bank: 0,
            prg_map: [0, prg_pages.saturating_sub(1)],
            chr_map: [0, 1],
            prg_ram: vec![0u8; PRG_RAM_32K].into_boxed_slice(),
            is_a_variant: mapper == 155,
            subtype,
            prg_pages,
            chr_pages,
        }
    }

    fn detect_subtype(prg_pages: u8, chr_pages: u8) -> MMC1Subtype {
        if prg_pages == 2 {
            return MMC1Subtype::Unbanked;
        }
        if prg_pages >= 8 {
            if chr_pages == 1 {
                return MMC1Subtype::Sxrom;
            } else if chr_pages >= 2 && chr_pages <= 8 {
                return MMC1Subtype::Szrom;
            }
        }
        MMC1Subtype::Standard
    }

    fn read_prg_ram(&self, addr: u16) -> u8 {
        if !self.is_prg_ram_enabled() {
            return 0;
        }
        let bank = self.get_prg_ram_bank() as usize;
        let offset = (addr - 0x6000) as usize;
        let index = (bank * PRG_RAM_8K + offset) % self.prg_ram.len();
        self.prg_ram[index]
    }

    fn write_prg_ram(&mut self, addr: u16, val: u8) {
        if !self.is_prg_ram_enabled() {
            return;
        }
        let bank = self.get_prg_ram_bank() as usize;
        let offset = (addr - 0x6000) as usize;
        let index = (bank * PRG_RAM_8K + offset) % self.prg_ram.len();
        self.prg_ram[index] = val;
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

        // Apply extended PRG-ROM banking ONLY for 512KB+ ROMs (SUROM)
        if matches!(self.subtype, MMC1Subtype::Sxrom) && self.prg_pages > 16 {
            let prg_outer_bank = ((self.chr_bank_0 >> 4) & 0x01) as u8;
            self.prg_map[0] = (prg_outer_bank << 4) | (self.prg_map[0] & 0x0F);
            self.prg_map[1] = (prg_outer_bank << 4) | (self.prg_map[1] & 0x0F);
        }

        // Ensure banks are within valid range
        self.prg_map[0] %= self.prg_pages;
        self.prg_map[1] %= self.prg_pages;

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
            MMC1Subtype::Sxrom => value & 0x01,
            MMC1Subtype::Szrom => value & 0x0F,
            _ => value & 0x1F,
        }
    }

    pub fn get_prg_ram_bank(&self) -> u8 {
        match self.subtype {
            MMC1Subtype::Sxrom => (self.chr_bank_0 >> 2) & 0x03,
            MMC1Subtype::Szrom => (self.chr_bank_0 >> 4) & 0x01,
            _ => 0,
        }
    }

    pub fn is_prg_ram_enabled(&self) -> bool {
        match self.subtype {
            MMC1Subtype::Sxrom => ((self.chr_bank_0 >> 4) & 0x01) == 0,
            _ if self.is_a_variant => true,
            _ => ((self.prg_bank >> 4) & 0x01) == 0,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UxROMState {
    pub prg_bank: u8,
}

impl UxROMState {
    pub fn new() -> Self {
        Self { prg_bank: 0 }
    }
}

impl Default for UxROMState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CNROMState {
    pub chr_bank: u8,
}

impl CNROMState {
    pub fn new() -> Self {
        Self { chr_bank: 0 }
    }
}

impl Default for CNROMState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AxROMState {
    pub prg_bank: u8,
    pub mirroring: Mirroring,
}

impl AxROMState {
    pub fn new() -> Self {
        Self {
            prg_bank: 0,
            mirroring: Mirroring::SingleScreenLower,
        }
    }
}

impl Default for AxROMState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MMC3State {
    pub banks: [u16; 8],
    pub chr_map: [u16; 8], // 1KB banks
    pub prg_map: [u16; 4], // 8KB banks
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
    pub prg_ram_protect: bool,
    pub prg_banks: u16,
    pub chr_banks: u16,
    pub is_four_screen: bool,

    // PRG-RAM (8KB standard for MMC3)
    pub prg_ram: Box<[u8]>,
}

impl MMC3State {
    pub fn new(prg_pages: u8, chr_pages: u8, is_four_screen: bool) -> Self {
        let last_page = (prg_pages * 2) as u16 - 1;
        let banks = if chr_pages == 0 { 8 } else { chr_pages * 8 };
        Self {
            banks: [0u16; 8],
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
            prg_ram_protect: false,
            prg_banks: last_page + 1,
            chr_banks: banks as u16,
            is_four_screen,
            prg_ram: vec![0u8; PRG_RAM_8K].into_boxed_slice(),
        }
    }

    fn read_prg_ram(&self, addr: u16) -> u8 {
        if self.prg_ram_enable {
            self.prg_ram[(addr - 0x6000) as usize]
        } else {
            0
        }
    }

    fn write_prg_ram(&mut self, addr: u16, val: u8) {
        if self.prg_ram_enable && !self.prg_ram_protect {
            self.prg_ram[(addr - 0x6000) as usize] = val;
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
