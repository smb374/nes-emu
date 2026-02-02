use crate::mapper::*;
use std::path::PathBuf;

const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
pub const PRG_ROM_PAGE_SIZE: usize = 0x4000; // 16KB
pub const CHR_ROM_PAGE_SIZE: usize = 0x2000; // 8KB

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct NesHeader {
    prg_pages: u8,
    chr_pages: u8,
    mapper: u8,
    is_hor_arr: bool,
    is_alt_arr: bool,
    has_trainer: bool,
    is_nes2: bool,
}

pub struct Rom {
    pub prg_rom: Box<[u8]>,
    pub chr_data: Box<[u8]>,
    pub hardware_mirroring: Mirroring, // From ROM header (solder pads)
    pub mapper: MapperType,
    pub trainer: Option<[u8; 512]>,
    pub irq_sig: bool,
    header: NesHeader,
    rom_path: PathBuf,
}

impl Rom {
    pub fn new(raw: &[u8], path: PathBuf) -> Result<Rom, String> {
        if &raw[0..4] != NES_TAG {
            return Err("File is not in iNES file format".to_string());
        }

        let mut header = NesHeader::default();

        header.mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);
        header.is_nes2 = ((raw[7] >> 2) & 0b11) == 2;
        header.is_hor_arr = raw[6] & 0b0001 != 0;
        header.is_alt_arr = raw[6] & 0b1000 != 0;
        header.prg_pages = raw[4];
        header.chr_pages = raw[5];
        header.has_trainer = raw[6] & 0b0100 != 0;

        // Hardware mirroring from solder pads
        let hardware_mirroring = match (header.is_alt_arr, header.is_hor_arr) {
            (false, false) => Mirroring::Horizontal,
            (false, true) => Mirroring::Vertical,
            (true, _) => Mirroring::FourScreen,
        };

        let trainer = if header.has_trainer {
            raw[16..16 + 512].try_into().ok()
        } else {
            None
        };

        let prg_rom_size = PRG_ROM_PAGE_SIZE * header.prg_pages as usize;
        let chr_data_size = if header.chr_pages == 0 {
            // CHR-RAM: 8KB default
            CHR_ROM_PAGE_SIZE
        } else {
            CHR_ROM_PAGE_SIZE * header.chr_pages as usize
        };

        let prg_rom_start = if header.has_trainer { 16 + 512 } else { 16 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        // Initialize mapper state - PRG-RAM is determined by hardware, not iNES header
        let mapper = match header.mapper {
            0 => MapperType::NROM(NROMState::new()),
            1 => MapperType::MMC1(MMC1State::new(
                header.mapper,
                header.prg_pages,
                header.chr_pages,
            )),
            2 => MapperType::UxROM(UxROMState::new()),
            3 => MapperType::CNROM(CNROMState::new()),
            4 => MapperType::MMC3(MMC3State::new(
                header.prg_pages,
                header.chr_pages,
                hardware_mirroring == Mirroring::FourScreen,
            )),
            7 => MapperType::AxROM(AxROMState::new()),
            _ => {
                return Err(format!(
                    "Mapper {} is not supported currently",
                    header.mapper
                ));
            }
        };

        // Load CHR data (or allocate CHR-RAM)
        let chr_data = if header.chr_pages == 0 {
            // CHR-RAM
            vec![0u8; chr_data_size].into_boxed_slice()
        } else {
            // CHR-ROM
            Box::from(&raw[chr_rom_start..(chr_rom_start + chr_data_size)])
        };

        Ok(Rom {
            prg_rom: Box::from(&raw[prg_rom_start..(prg_rom_start + prg_rom_size)]),
            chr_data,
            mapper,
            hardware_mirroring,
            trainer,
            header,
            irq_sig: false,
            rom_path: path,
        })
    }

    fn save_file_path(&self) -> PathBuf {
        let mut save_path = self.rom_path.with_extension("");
        let extension = save_path
            .extension()
            .unwrap_or_default()
            .to_str()
            .unwrap_or("");
        let new_extension = if extension.is_empty() {
            "sav".to_string()
        } else {
            format!("{}.sav", extension)
        };
        save_path.set_extension(new_extension);
        save_path
    }

    pub fn save_prg_ram(&self) -> Result<(), String> {
        if !self.mapper.has_prg_ram() {
            return Ok(()); // No PRG-RAM to save
        }

        let prg_ram_data = self.mapper.get_prg_ram_data();
        std::fs::write(&self.save_file_path(), prg_ram_data)
            .map_err(|e| format!("Failed to write save file: {}", e))?;

        Ok(())
    }

    pub fn load_prg_ram(&mut self) -> Result<(), String> {
        if !self.mapper.has_prg_ram() {
            return Ok(()); // No PRG-RAM to load
        }

        let save_path = self.save_file_path();

        if !save_path.exists() {
            return Ok(()); // No save file exists yet
        }

        let save_data =
            std::fs::read(&save_path).map_err(|e| format!("Failed to read save file: {}", e))?;

        self.mapper.load_prg_ram_data(&save_data)?;

        Ok(())
    }

    pub fn read_prg(&mut self, addr: u16) -> u8 {
        match addr {
            // PRG-RAM - delegated to mapper
            0x6000..=0x7FFF => self.mapper.read_prg_ram(addr),
            // PRG-ROM
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => 0,
        }
    }

    // Update write_prg to handle Bus Conflicts:
    pub fn write_prg(&mut self, addr: u16, val: u8) {
        match addr {
            0x6000..=0x7FFF => self.mapper.write_prg_ram(addr, val),
            0x8000..=0xFFFF => self.write_mapper_register(addr, val),
            _ => {}
        }
    }

    pub fn read_chr(&self, addr: u16) -> u8 {
        match self.mapper {
            MapperType::NROM(_) | MapperType::UxROM(_) | MapperType::AxROM(_) => {
                // Direct access (UxROM and AxROM use CHR-RAM)
                let idx = (addr as usize) % self.chr_data.len();
                self.chr_data[idx]
            }
            MapperType::CNROM(ref state) => {
                // 8KB CHR banking
                let bank_offset = (state.chr_bank as usize) * CHR_ROM_PAGE_SIZE;
                let idx = bank_offset + addr as usize;
                self.chr_data[idx % self.chr_data.len()]
            }
            MapperType::MMC1(ref state) => {
                let idx = (addr >> 12) & 0x1;
                let offset = addr - (addr & 0x1000);
                let page = state.chr_map[idx as usize];
                self.chr_data[page as usize * CHR_ROM_PAGE_SIZE / 2 + offset as usize]
            }
            MapperType::MMC3(ref state) => {
                let idx = (addr >> 10) & 0x7;
                let offset = addr - (addr & 0x1C00);
                let page = state.chr_map[idx as usize];
                self.chr_data[page as usize * CHR_ROM_PAGE_SIZE / 8 + offset as usize]
            }
        }
    }

    pub fn write_chr(&mut self, addr: u16, val: u8) {
        // Only works with CHR-RAM
        if self.header.chr_pages == 0 {
            self.chr_data[addr as usize] = val;
        }
    }

    pub fn mirroring(&self) -> Mirroring {
        // Mapper can override hardware mirroring
        self.mapper
            .mapper_mirroring()
            .unwrap_or(self.hardware_mirroring)
    }

    pub fn clock_scanline_irq(&mut self) {
        if let MapperType::MMC3(ref mut state) = self.mapper {
            if state.irq_counter == 0 || state.irq_reload {
                state.irq_counter = state.irq_latch;
                state.irq_reload = false;
            } else {
                state.irq_counter -= 1;
            }

            if state.irq_counter == 0 && state.irq_enabled {
                self.irq_sig = true;
            }
        }
    }

    fn read_prg_rom(&self, addr: u16) -> u8 {
        match self.mapper {
            MapperType::NROM(_) => {
                // Mirror if 16KB ROM
                self.prg_rom[(addr - 0x8000) as usize % self.prg_rom.len()]
            }
            MapperType::UxROM(ref state) => {
                match addr {
                    0x8000..=0xBFFF => {
                        // Switchable 16KB bank
                        let bank_offset = (state.prg_bank as usize) * PRG_ROM_PAGE_SIZE;
                        let idx = bank_offset + (addr - 0x8000) as usize;
                        self.prg_rom[idx]
                    }
                    0xC000..=0xFFFF => {
                        // Fixed to last bank
                        let last_bank = self.header.prg_pages as usize - 1;
                        let bank_offset = last_bank * PRG_ROM_PAGE_SIZE;
                        let idx = bank_offset + (addr - 0xC000) as usize;
                        self.prg_rom[idx]
                    }
                    _ => unreachable!(),
                }
            }
            MapperType::CNROM(_) => {
                // Fixed 32KB PRG-ROM
                self.prg_rom[(addr - 0x8000) as usize % self.prg_rom.len()]
            }
            MapperType::AxROM(ref state) => {
                // 32KB switchable bank at $8000-$FFFF
                let bank_offset = (state.prg_bank as usize) * (PRG_ROM_PAGE_SIZE * 2);
                let idx = bank_offset + (addr - 0x8000) as usize;
                self.prg_rom[idx % self.prg_rom.len()]
            }
            MapperType::MMC1(ref state) => {
                let idx = (addr >> 14) & 0x1;
                let offset = addr - (addr & 0xC000);
                let page = state.prg_map[idx as usize];
                self.prg_rom[page as usize * PRG_ROM_PAGE_SIZE + offset as usize]
            }
            MapperType::MMC3(ref state) => {
                let idx = (addr >> 13) & 0x3;
                let offset = addr - (addr & 0xE000);
                let page = state.prg_map[idx as usize];
                self.prg_rom[page as usize * PRG_ROM_PAGE_SIZE / 2 + offset as usize]
            }
        }
    }

    fn write_mapper_register(&mut self, addr: u16, val: u8) {
        match self.mapper {
            MapperType::NROM(_) => {
                // No registers
            }

            MapperType::UxROM(ref mut state) => {
                // Direct bank select
                state.prg_bank = val & 0x0F;
            }

            MapperType::CNROM(ref mut state) => {
                // Direct CHR bank select
                state.chr_bank = val & 0x03;
            }

            MapperType::AxROM(ref mut state) => {
                state.prg_bank = val & 0x07;
                state.mirroring = if (val & 0x10) == 0 {
                    Mirroring::SingleScreenLower
                } else {
                    Mirroring::SingleScreenUpper
                };
            }

            MapperType::MMC1(ref mut state) => {
                // Reset on bit 7 set
                if (val & 0x80) != 0 {
                    state.shift_register = 0;
                    state.shift_count = 0;
                    state.control |= 0x0C; // Lock PRG to mode 3
                    state.map_pages();
                    return;
                }

                // Shift in bit 0
                state.shift_register >>= 1;
                state.shift_register |= (val & 0x01) << 4;
                state.shift_count += 1;

                // On 5th write, copy to register
                if state.shift_count == 5 {
                    let reg_select = (addr >> 13) & 0x03;
                    match reg_select {
                        0 => state.control = state.shift_register,
                        1 => state.chr_bank_0 = state.shift_register,
                        2 => state.chr_bank_1 = state.shift_register,
                        3 => state.prg_bank = state.shift_register,
                        _ => unreachable!(),
                    }
                    state.map_pages();
                    state.shift_register = 0;
                    state.shift_count = 0;
                }
            }

            MapperType::MMC3(ref mut state) => match addr & 0xE001 {
                0x8000 => {
                    state.chr_swap = (val & 0x80) != 0;
                    state.prg_swap = (val & 0x40) != 0;
                    state.map_pages();
                    state.command = val & 0x07;
                }
                0x8001 => {
                    match state.command {
                        0 | 1 => state.banks[state.command as usize] = val as u16 & 0xFE,
                        2..=5 => state.banks[state.command as usize] = val as u16,
                        6 | 7 => state.banks[state.command as usize] = val as u16 & 0x3F,
                        _ => unreachable!(),
                    }
                    state.map_pages();
                }
                0xA000 => {
                    state.arr_select = (val & 0x01) != 0;
                }
                0xA001 => {
                    state.prg_ram_enable = (val & 0x80) != 0;
                    state.prg_ram_protect = (val & 0x40) != 0;
                }
                0xC000 => {
                    state.irq_latch = val;
                }
                0xC001 => {
                    state.irq_reload = true;
                    state.irq_counter = 0;
                }
                0xE000 => {
                    state.irq_enabled = false;
                    self.irq_sig = false; // Acknowledge IRQ
                }
                0xE001 => {
                    state.irq_enabled = true;
                }
                _ => unreachable!(),
            },
        }
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    struct TestRom {
        header: Vec<u8>,
        trainer: Option<Vec<u8>>,
        prg_rom: Vec<u8>,
        chr_rom: Vec<u8>,
    }

    fn create_rom(rom: TestRom) -> Vec<u8> {
        let mut result = Vec::with_capacity(
            rom.header.len()
                + rom.trainer.as_ref().map_or(0, |t| t.len())
                + rom.prg_rom.len()
                + rom.chr_rom.len(),
        );

        result.extend(&rom.header);
        if let Some(t) = rom.trainer {
            result.extend(t);
        }
        result.extend(&rom.prg_rom);
        result.extend(&rom.chr_rom);

        result
    }

    pub fn test_rom(program: Vec<u8>) -> Rom {
        let mut prg_rom_contents = program;
        prg_rom_contents.resize(2 * PRG_ROM_PAGE_SIZE, 0);

        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            prg_rom: prg_rom_contents,
            chr_rom: vec![2; 1 * CHR_ROM_PAGE_SIZE],
        });

        Rom::new(&test_rom, "".into()).unwrap()
    }

    #[test]
    fn test_rom_loading() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            prg_rom: vec![1; 2 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; 1 * CHR_ROM_PAGE_SIZE],
        });

        let rom = Rom::new(&test_rom, "".into()).unwrap();

        assert_eq!(rom.chr_data.len(), CHR_ROM_PAGE_SIZE);
        assert_eq!(rom.prg_rom.len(), 2 * PRG_ROM_PAGE_SIZE);
        assert!(matches!(rom.mapper, MapperType::CNROM(_)));
        assert_eq!(rom.hardware_mirroring, Mirroring::Vertical);
    }

    #[test]
    fn test_chr_ram() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x00, 0x20, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            prg_rom: vec![1; 2 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![], // No CHR-ROM = CHR-RAM
        });

        let rom = Rom::new(&test_rom, "".into()).unwrap();

        assert_eq!(rom.chr_data.len(), CHR_ROM_PAGE_SIZE); // 8KB CHR-RAM
        assert!(matches!(rom.mapper, MapperType::UxROM(_)));
    }

    #[test]
    fn test_mmc1_has_prg_ram() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x08, 0x00, 0x10, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            prg_rom: vec![1; 8 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![],
        });

        let rom = Rom::new(&test_rom, "".into()).unwrap();
        assert!(rom.mapper.has_prg_ram());
    }
}
