use crate::apu::registers::TriangleRegister;

#[derive(Debug, Default)]
pub struct TriangleChannel {
    // TODO: 32-step sequencer, timer, linear counter, length counter
}

impl TriangleChannel {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn clock_timer(&mut self, _cycles: usize, _reg: &TriangleRegister) {}
    pub fn clock_linear_counter(&mut self, _reg: &TriangleRegister) {}
    pub fn clock_length(&mut self, _reg: &TriangleRegister) {}
    pub fn output(&self) -> u8 {
        0
    }
}
