pub mod registers;

use registers::{APUStatus, FrameCounter};

use crate::apu::registers::{NoiseRegister, PulseRegister, TriangleRegister};

#[derive(Debug, Default)]
pub struct APU {
    pub pulse1_reg: PulseRegister,
    pub pulse2_reg: PulseRegister,
    pub triag_reg: TriangleRegister,
    pub noise_reg: NoiseRegister,
    pub status: APUStatus,
    pub frame_counter: FrameCounter,
}
