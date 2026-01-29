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
            MapperType::MMC3(s) => Some(if s.arr_select {
                Mirroring::Horizontal
            } else {
                Mirroring::Vertical
            }),
            _ => None, // Use hardware mirroring from ROM header
        }
    }
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
}

impl MMC1State {
    pub fn new(prg_pages: u8) -> Self {
        Self {
            shift_register: 0,
            shift_count: 0,
            control: 0x0C, // Mode 3: fix last bank at $C000, switch at $8000
            chr_bank_0: 0,
            chr_bank_1: 0,
            prg_bank: 0,
            prg_map: [0, prg_pages - 1], // per PRG page
            chr_map: [0, 1],             // per half CHR page
        }
    }

    pub fn map_pages(&mut self, prg_pages: u8) {
        match self.prg_mode() {
            0 | 1 => {
                let page = (self.prg_bank >> 1) & 0x0F;
                self.prg_map = [page, page + 1];
            }
            2 => {
                let page = self.prg_bank & 0x0F;
                self.prg_map = [0, page];
            }
            3 => {
                let page = self.prg_bank & 0x0F;
                self.prg_map = [page, prg_pages - 1];
            }
            _ => unreachable!(),
        }
        if self.chr_mode() {
            let page0 = self.chr_bank_0 & 0x1F;
            let page1 = self.chr_bank_1 & 0x1F;
            self.chr_map = [page0, page1];
        } else {
            let page = (self.chr_bank_0 >> 1) & 0x0F;
            self.chr_map = [page * 2, page * 2 + 1];
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
        (self.control >> 4) & 0x01 != 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MMC3State {
    pub chr_page: [u8; 6],
    pub prg_page: [u8; 2],
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
    pub last_a12: bool,
}

impl MMC3State {
    pub fn new(prg_pages: u8) -> Self {
        let last_page = (prg_pages * 2) - 1;
        Self {
            chr_page: [0u8; 6],
            prg_page: [0u8; 2],
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
            last_a12: false,
        }
    }
    pub fn map_pages(&mut self, prg_pages: u8) {
        if self.chr_swap {
            self.chr_map = [
                self.chr_page[2],
                self.chr_page[3],
                self.chr_page[4],
                self.chr_page[5],
                self.chr_page[0],
                self.chr_page[0] + 1,
                self.chr_page[1],
                self.chr_page[1] + 1,
            ];
        } else {
            self.chr_map = [
                self.chr_page[0],
                self.chr_page[0] + 1,
                self.chr_page[1],
                self.chr_page[1] + 1,
                self.chr_page[2],
                self.chr_page[3],
                self.chr_page[4],
                self.chr_page[5],
            ];
        }
        let last_page = (prg_pages * 2) - 1;
        if self.prg_swap {
            self.prg_map = [last_page - 1, self.prg_page[1], self.prg_page[0], last_page];
        } else {
            self.prg_map = [self.prg_page[0], self.prg_page[1], last_page - 1, last_page];
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
