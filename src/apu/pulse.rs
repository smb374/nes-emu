use super::registers::PulseRegister;

#[allow(unused)]
#[derive(Debug, Default)]
pub struct PulseChannel {
    is_pulse2: bool,
    // TODO: duty sequencer, timer, envelope, sweep, length counter
}

impl PulseChannel {
    pub fn new(is_pulse2: bool) -> Self {
        Self { is_pulse2 }
    }
    pub fn clock_timer(&mut self, _cycles: usize, _reg: &PulseRegister) {}
    pub fn clock_envelope(&mut self, _reg: &PulseRegister) {}
    pub fn clock_length(&mut self, _reg: &PulseRegister) {}
    pub fn clock_sweep(&mut self, _reg: &PulseRegister) {}
    pub fn output(&self) -> u8 {
        0
    }
}
