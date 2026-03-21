use cpal::Device;

use crate::{
    CPU_CYCLE, Mem,
    apu::{APU, TimedChannel},
    cartridge::Rom,
    joypad::Joypad,
    ppu::PPU,
};

//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       | SRAM          |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       | I/O Registers |
// | $2000-$2007   |       |               |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       | RAM           |
// |_ _ _ _ _ _ _ _| $0200 |               |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DMAState {
    Idle,
    Pending,
    DMCDummy,
    Alignment,
    Transfer,
}

impl Default for DMAState {
    fn default() -> Self {
        Self::Idle
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct IOBus {
    pub addr: u16,
    data: u8,
}

#[allow(unused)]
pub struct Bus<'call> {
    vram: [u8; 0x800],
    rom: Rom,
    pub ppu: PPU,
    apu: APU,

    cpu_writing: bool,
    rdy: bool,
    irq_line: bool,

    pub cpu_bus: IOBus,
    ppu_bus: u8,
    joypad1: Joypad,
    j1_data: Option<u8>,
    joypad2: Joypad,
    cb: Box<dyn FnMut(&PPU, &mut Joypad) -> bool + 'call>,

    oam_dma_state: DMAState,
    oam_dma_addr: u16,
    oam_dma_data: u8,

    dmc_dma_state: DMAState,
    dmc_dma_reload: bool,
    dmc_dma_addr: u16,
    dmc_dma_retry: bool,
}

impl<'call> Bus<'call> {
    pub fn new<F>(rom: Rom, device: Device, f: F) -> Self
    where
        F: FnMut(&PPU, &mut Joypad) -> bool + 'call,
    {
        let ppu = PPU::new();
        let apu = APU::new(device);
        let mut vram = [0xFF; 0x800];
        rand::fill(&mut vram);
        Self {
            vram,
            rom,
            ppu,
            apu,

            cpu_writing: false,
            rdy: true,
            irq_line: false,

            cpu_bus: IOBus::default(),
            ppu_bus: 0,
            joypad1: Joypad::new(),
            joypad2: Joypad::new(),
            cb: Box::new(f),
            j1_data: None,

            oam_dma_state: DMAState::default(),
            oam_dma_addr: 0,
            oam_dma_data: 0,

            dmc_dma_state: DMAState::default(),
            dmc_dma_reload: false,
            dmc_dma_addr: 0,
            dmc_dma_retry: false,
        }
    }

    pub fn tick(&mut self) -> bool {
        let pb = self.apu.put_cycle;
        self.apu.tick();
        let pa = self.apu.put_cycle;
        if self.apu.dmc.dma_sample
            && self.apu.dmc.dma_reload
            && self.dmc_dma_state == DMAState::Idle
        {
            self.dmc_dma_req(self.apu.dmc.current_address, true);
        }
        self.handle_dma();
        if self.apu.dmc.dma_sample
            && !self.apu.dmc.dma_reload
            && self.dmc_dma_state == DMAState::Idle
        {
            self.dmc_dma_req(self.apu.dmc.current_address, false);
        }

        if !pb && pa {
            // get -> put
            if let Some(dat) = self.j1_data.take() {
                self.joypad1.write(dat);
            }
        }
        let frame_before = self.ppu.frames;
        self.ppu.tick(&mut self.rom);
        let frame_after = self.ppu.frames;

        if frame_before != frame_after {
            self.ppu_bus = 0;
            if (self.cb)(&self.ppu, &mut self.joypad1) {
                return true;
            }
        }

        let mapper_irq = self.rom.irq_sig;
        self.rom.irq_sig = false;
        let apu_irq = self.apu.irq_sig;
        self.apu.irq_sig = self.apu.dmc.irq_flag;

        self.irq_line |= mapper_irq || apu_irq;
        false
    }

    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        self.ppu.nmi_interrupt.take()
    }

    pub fn poll_irq_status(&mut self) -> bool {
        let res = self.irq_line;
        self.irq_line = self.apu.dmc.irq_flag;
        res
    }

    pub fn save_prg_ram(&self) -> Result<(), String> {
        self.rom.save_prg_ram()
    }

    pub fn set_writing(&mut self, writing: bool) {
        self.cpu_writing = writing;
    }

    pub fn rdy(&self) -> bool {
        self.rdy
    }

    pub fn is_dma_xfer(&self) -> bool {
        self.oam_dma_state == DMAState::Transfer || self.dmc_dma_state == DMAState::Transfer
    }

    fn oam_dma_req(&mut self, page: u8) {
        self.ppu.oam_addr = 0;
        self.oam_dma_state = DMAState::Pending;
        self.oam_dma_addr = (page as u16) << 8;
        self.oam_dma_data = 0;
    }

    fn dmc_dma_req(&mut self, addr: u16, reload: bool) {
        self.dmc_dma_state = DMAState::Pending;
        self.dmc_dma_reload = reload;
        self.dmc_dma_addr = addr;
        self.dmc_dma_retry = false;
    }

    fn handle_dma(&mut self) {
        match (self.dmc_dma_state, self.oam_dma_state) {
            (DMAState::Idle, s) => match s {
                DMAState::Idle => {}
                DMAState::Pending => {
                    if !self.cpu_writing {
                        self.rdy = false;
                        if self.apu.put_cycle {
                            self.oam_dma_state = DMAState::Transfer;
                        } else {
                            self.oam_dma_state = DMAState::Alignment;
                        }
                    }
                }
                DMAState::Alignment => {
                    self.oam_dma_state = DMAState::Transfer;
                }
                DMAState::Transfer => {
                    if !self.apu.put_cycle {
                        self.oam_dma_data = self.read_u8(self.oam_dma_addr);
                    } else {
                        self.ppu.write_to_oam_data(self.oam_dma_data);
                        self.oam_dma_addr += 1;
                        if (self.oam_dma_addr & 0xFF) == 0 {
                            self.oam_dma_state = DMAState::Idle;
                            self.rdy = self.dmc_dma_state == DMAState::Idle;
                        }
                    }
                }
                _ => {}
            },
            (s, DMAState::Idle | DMAState::Pending) => match s {
                DMAState::Idle => {}
                DMAState::Pending => {
                    if !self.cpu_writing {
                        self.rdy = false;
                        self.dmc_dma_state = DMAState::DMCDummy;
                        log::info!(
                            "[DMC DMA] Pending -> Dummy PUT:{} CYC:{}",
                            self.apu.put_cycle,
                            CPU_CYCLE.get()
                        );
                    } else {
                        log::info!("[DMC DMA] Retry");
                        self.dmc_dma_retry = true;
                    }
                }
                DMAState::DMCDummy => {
                    if !self.apu.put_cycle {
                        self.dmc_dma_state = DMAState::Alignment;
                        log::info!(
                            "[DMC DMA] Dummy -> Alignment PUT:{} CYC:{}",
                            self.apu.put_cycle,
                            CPU_CYCLE.get()
                        );
                    } else if self.dmc_dma_retry {
                        self.dmc_dma_state = DMAState::Transfer;
                        log::info!(
                            "[DMC DMA] Dummy -> Transfer PUT:{} CYC:{}",
                            self.apu.put_cycle,
                            CPU_CYCLE.get()
                        );
                    }
                }
                DMAState::Alignment => {
                    self.dmc_dma_state = DMAState::Transfer;
                    log::info!(
                        "[DMC DMA] Alignment -> Transfer PUT:{} CYC:{}",
                        self.apu.put_cycle,
                        CPU_CYCLE.get()
                    );
                }
                DMAState::Transfer => {
                    let data = self.read_u8(self.dmc_dma_addr);
                    self.apu.dmc.update_sample(data);
                    log::info!(
                        "[DMC DMA] Transfer ${:04X} => ${:02X} bus=${:02X} PUT:{} CYC:{}",
                        self.dmc_dma_addr,
                        data,
                        self.cpu_bus.data,
                        self.apu.put_cycle,
                        CPU_CYCLE.get()
                    );
                    self.dmc_dma_state = DMAState::Idle;
                    if self.oam_dma_state == DMAState::Pending {
                        self.oam_dma_state = DMAState::Alignment;
                    } else {
                        self.rdy = true;
                    }
                }
            },
            (s, DMAState::Alignment) => match s {
                DMAState::Idle => self.oam_dma_state = DMAState::Transfer,
                DMAState::Pending => {
                    self.oam_dma_state = DMAState::Transfer;
                    self.dmc_dma_state = DMAState::DMCDummy;
                }
                DMAState::DMCDummy => {
                    if self.apu.put_cycle {
                        self.dmc_dma_state = DMAState::Transfer;
                    } else {
                        self.dmc_dma_state = DMAState::Alignment;
                    }
                }
                DMAState::Alignment => {
                    self.dmc_dma_state = DMAState::Transfer;
                }
                DMAState::Transfer => {
                    let data = self.read_u8(self.dmc_dma_addr);
                    self.apu.dmc.update_sample(data);
                    self.dmc_dma_state = DMAState::Idle;
                }
            },
            (DMAState::Pending, DMAState::Transfer) => {
                self.dmc_dma_state = DMAState::DMCDummy;
                if !self.apu.put_cycle {
                    self.oam_dma_data = self.read_u8(self.oam_dma_addr);
                } else {
                    self.ppu.write_to_oam_data(self.oam_dma_data);
                    self.oam_dma_addr += 1;
                    if (self.oam_dma_addr & 0xFF) == 0 {
                        self.oam_dma_state = DMAState::Idle;
                    }
                }
            }
            (DMAState::DMCDummy, DMAState::Transfer) => {
                if !self.apu.put_cycle {
                    self.dmc_dma_state = DMAState::Alignment;
                    self.oam_dma_data = self.read_u8(self.oam_dma_addr);
                } else {
                    self.dmc_dma_state = DMAState::Transfer;
                    self.ppu.write_to_oam_data(self.oam_dma_data);
                    self.oam_dma_addr += 1;
                    if (self.oam_dma_addr & 0xFF) == 0 {
                        self.oam_dma_state = DMAState::Idle;
                    }
                }
            }
            (DMAState::Alignment, DMAState::Transfer) => {
                self.dmc_dma_state = DMAState::Transfer;

                if !self.apu.put_cycle {
                    self.oam_dma_data = self.read_u8(self.oam_dma_addr);
                } else {
                    self.ppu.write_to_oam_data(self.oam_dma_data);
                    self.oam_dma_addr += 1;
                    if (self.oam_dma_addr & 0xFF) == 0 {
                        self.oam_dma_state = DMAState::Idle;
                    }
                }
            }
            (DMAState::Transfer, DMAState::Transfer) => {
                let data = self.read_u8(self.dmc_dma_addr);
                self.apu.dmc.update_sample(data);
                self.dmc_dma_state = DMAState::Idle;

                if self.oam_dma_state == DMAState::Transfer {
                    self.oam_dma_state = DMAState::Alignment;
                }
            }
            (_, DMAState::DMCDummy) => unreachable!(),
        }
    }
}

impl<'call> Mem for Bus<'call> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        self.cpu_bus.addr = addr;
        match addr {
            0x0000..=0x1FFF => {
                let mirror_down_addr = addr & 0x7FF;
                self.cpu_bus.data = self.vram[mirror_down_addr as usize];
                self.cpu_bus.data
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => {
                self.cpu_bus.data = self.ppu_bus;
                self.cpu_bus.data
            }
            0x2002 => {
                let status = self.ppu.read_status();
                self.ppu_bus = (status & 0xe0) | (self.ppu_bus & 0x1f);
                self.cpu_bus.data = self.ppu_bus;
                self.cpu_bus.data
            }
            0x2004 => {
                self.ppu_bus = self.ppu.read_oam_data();
                self.cpu_bus.data = self.ppu_bus;
                self.cpu_bus.data
            }
            0x2007 => {
                self.ppu_bus = self.ppu.read_data(&mut self.rom, self.ppu_bus);
                self.cpu_bus.data = self.ppu_bus;
                self.cpu_bus.data
            }
            0x2008..=0x3FFF => {
                let mirror_down_addr = addr & 0x2007;
                self.read_u8(mirror_down_addr)
            }

            0x4000..=0x4014 => self.cpu_bus.data,

            0x4015 => (self.cpu_bus.data & 0x20) | (self.apu.read_status() & 0xdf),

            0x4016 => {
                self.cpu_bus.data = (self.cpu_bus.data & 0xe0) | (self.joypad1.read() & 0x1f);
                self.cpu_bus.data
            }
            0x4017 => {
                self.cpu_bus.data &= 0xe0;
                self.cpu_bus.data
            }
            0x4018..=0x5FFF => self.cpu_bus.data,
            0x6000..=0xFFFF => {
                self.cpu_bus.data = self.rom.read_prg(addr);
                self.cpu_bus.data
            }
        }
    }

    fn write_u8(&mut self, addr: u16, data: u8) {
        self.cpu_bus.addr = addr;
        self.cpu_bus.data = data;
        match addr {
            0x0000..=0x1FFF => {
                let mirror_down_addr = addr & 0x7FF;
                self.vram[mirror_down_addr as usize] = data;
            }
            0x2000..=0x2007 => {
                self.ppu_bus = data;
                match addr {
                    0x2000 => {
                        self.ppu.write_to_ctrl(data);
                    }
                    0x2001 => {
                        self.ppu.write_to_mask(data);
                    }

                    0x2002 => {}

                    0x2003 => {
                        self.ppu.write_to_oam_addr(data);
                    }
                    0x2004 => {
                        self.ppu.write_to_oam_data(data);
                    }
                    0x2005 => {
                        self.ppu.write_to_scroll(data);
                    }

                    0x2006 => {
                        self.ppu.write_to_ppu_addr(&mut self.rom, data);
                    }
                    0x2007 => {
                        self.ppu.write_data(&mut self.rom, data);
                    }
                    _ => unreachable!(),
                }
            }

            0x2008..=0x3FFF => {
                let mirror_down_addr = addr & 0x2007;
                self.write_u8(mirror_down_addr, data);
            }

            0x4000 => self.apu.pulse1.update_volume(data),
            0x4004 => self.apu.pulse2.update_volume(data),

            0x4001 => self.apu.pulse1.update_sweep(data),
            0x4005 => self.apu.pulse2.update_sweep(data),

            0x4002 => self.apu.pulse1.update_period_lo(data),
            0x4006 => self.apu.pulse2.update_period_lo(data),

            0x4003 => self.apu.pulse1.update_period_hi(data),
            0x4007 => self.apu.pulse2.update_period_hi(data),

            0x4008 => self.apu.triag.update_counter(data),
            0x400A => self.apu.triag.update_period_lo(data),
            0x400B => self.apu.triag.update_period_hi(data),

            0x400C => self.apu.noise.update_volume(data),
            0x400E => self.apu.noise.update_period_lo(data),
            0x400F => self.apu.noise.update_period_hi(data),

            0x4010 => self.apu.dmc.update_control(data),
            0x4011 => self.apu.dmc.update_direct_load(data),
            0x4012 => self.apu.dmc.update_sample_address(data),
            0x4013 => self.apu.dmc.update_sample_length(data),

            // https://wiki.nesdev.com/w/index.php/PPU_programmer_reference#OAM_DMA_.28.244014.29_.3E_write
            0x4014 => {
                self.oam_dma_req(data);
            }

            0x4015 => self.apu.write_status(data),

            0x4016 => {
                self.j1_data = Some(data);
            }

            0x4017 => self.apu.write_frame_counter(data),

            0x6000..=0xFFFF => self.rom.write_prg(addr, data),

            _ => {
                // eprintln!("Ignoring mem write-access at {}", addr);
            }
        }
    }
}
