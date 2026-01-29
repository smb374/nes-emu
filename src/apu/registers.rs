use bitflags::bitflags;

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct APUStatus: u8 {
        const PULSE_CHANNEL1    = 0b0000_0001;
        const PULSE_CHANNEL2    = 0b0000_0010;
        const TRIAG_CHANNEL     = 0b0000_0100;
        const NOISE_CHANNEL     = 0b0000_1000;
        const DMC_CHANNEL       = 0b0001_0000;
        const UNUSED            = 0b0010_0000;
        const FRAME_INTERRUPT   = 0b0100_0000;
        const DMC_INTERRUPT     = 0b1000_0000;
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
        *self |= Self::from_bits_retain(bits & 0x1F);
    }
}

impl FrameCounter {
    pub fn emit_irq(&self) -> bool {
        !self.contains(Self::FIVE_STEP_MODE) && !self.contains(Self::IRQ_INHIBIT)
    }

    pub fn update(&mut self, bits: u8) {
        *self = Self::from_bits_retain(bits);
    }

    pub fn is_five_mode(&self) -> bool {
        self.contains(Self::FIVE_STEP_MODE)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct PulseRegister {
    pub envelope: u8,
    pub sweep: u8,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TriangleRegister {
    pub counter: u8,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct NoiseRegister {
    pub envelope: u8,
}
