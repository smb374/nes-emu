use crate::apu::registers::NoiseRegister;

#[derive(Debug, Default)]
pub struct NoiseChannel {
    // TODO: LFSR, timer, envelope, length counter
}

impl NoiseChannel {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn clock_timer(&mut self, _cycles: usize, _reg: &NoiseRegister) {}
    pub fn clock_envelope(&mut self, _reg: &NoiseRegister) {}
    pub fn clock_length(&mut self, _reg: &NoiseRegister) {}
    pub fn output(&self) -> u8 {
        0
    }
}
