use bitflags::bitflags;

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct APUStatus: u8 {
        const PULSE_CHANNEL1    = 0b0000_0001;
        const PULSE_CHANNEL2    = 0b0000_0010;
        const TRIANGLE_CHANNEL  = 0b0000_0100;
        const NOISE_CHANNEL     = 0b0000_1000;
        const DMC_CHANNEL       = 0b0001_0000;
        const UNUSED            = 0b0010_0000;
        const FRAME_INTERRUPT   = 0b0100_0000;
        const INTERRUPT         = 0b1000_0000;
    }

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct FrameCounter: u8 {
        const IRQ_INHIBIT       = 0b0100_0000;
        const FIVE_STEP_MODE    = 0b1000_0000;

        const _ = !0;
    }
}

impl Default for APUStatus {
    fn default() -> Self {
        Self::from_bits_retain(0)
    }
}

impl Default for FrameCounter {
    fn default() -> Self {
        Self::from_bits_retain(0)
    }
}

impl APUStatus {
    pub fn update(&mut self, bits: u8) {
        *self = Self::from_bits_retain(bits);
    }
}

impl FrameCounter {
    pub fn update(&mut self, bits: u8) {
        *self = Self::from_bits_retain(bits);
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct PulseRegister {
    pub ctrl: u8,
    pub sweep: u8,
    pub tl: u8,
    pub th: u8,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TriangleRegister {
    pub ctrl: u8,
    pub tl: u8,
    pub th: u8,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct NoiseRegister {
    pub ctrl: u8,
    pub noise: u8,
    pub lcload: u8,
}

impl PulseRegister {
    pub fn duty(&self) -> u8 {
        (self.ctrl & 0xC0) >> 6
    }

    pub fn loop_envelope(&self) -> bool {
        self.ctrl & 0x20 != 0
    }

    pub fn const_volume(&self) -> bool {
        self.ctrl & 0x10 != 0
    }

    pub fn volume(&self) -> u8 {
        self.ctrl & 0x0F
    }

    pub fn sweep_enabled(&self) -> bool {
        self.sweep & 0x80 != 0
    }

    pub fn sweep_period(&self) -> u8 {
        self.sweep & 0x70 >> 4
    }

    pub fn sweep_neg(&self) -> bool {
        self.sweep & 0x08 != 0
    }

    pub fn sweep_shift(&self) -> u8 {
        self.sweep & 0x07
    }

    pub fn timer(&self) -> u16 {
        self.tl as u16 | (self.th as u16 & 0x07) << 8
    }

    pub fn length_counter_load(&self) -> u8 {
        self.th >> 3
    }
}

impl TriangleRegister {
    pub fn length_counter_disabled(&self) -> bool {
        self.ctrl & 0x80 != 0
    }

    pub fn linear_counter_reload(&self) -> u8 {
        self.ctrl & 0x7F
    }

    pub fn timer(&self) -> u16 {
        self.tl as u16 | (self.th as u16 & 0x07) << 8
    }

    pub fn length_counter_load(&self) -> u8 {
        self.th >> 3
    }
}

impl NoiseRegister {
    pub fn loop_envelope(&self) -> bool {
        self.ctrl & 0x20 != 0
    }

    pub fn const_volume(&self) -> bool {
        self.ctrl & 0x10 != 0
    }

    pub fn volume(&self) -> u8 {
        self.ctrl & 0x0F
    }

    pub fn loop_noise(&self) -> bool {
        self.noise & 0x80 != 0
    }

    pub fn noise_period(&self) -> u8 {
        self.noise & 0x0F
    }

    pub fn length_counter_load(&self) -> u8 {
        self.lcload >> 3
    }
}
