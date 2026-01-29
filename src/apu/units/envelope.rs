#[derive(Debug)]
pub struct Envelope {
    start_flag: bool,
    divider: u8,
    decay_level: u8,
    pub output: u8,
}

impl Default for Envelope {
    fn default() -> Self {
        Self {
            start_flag: true,
            divider: 0,
            decay_level: 0,
            output: 0,
        }
    }
}

impl Envelope {
    // Reg: --LC.VVVV
    pub fn clock(&mut self, reg_val: u8) {
        if self.start_flag {
            self.decay_level = 15;
            self.divider = reg_val & 0x0F;
            self.start_flag = false;
        }

        if self.divider == 0 {
            self.divider = reg_val & 0x0F;
            if self.decay_level == 0 && reg_val & 0b0010_0000 != 0 {
                self.decay_level = 15;
            } else {
                self.decay_level -= 1;
            }
        }

        if reg_val & 0b0001_0000 != 0 {
            self.output = reg_val & 0x0F;
        } else {
            self.output = self.decay_level;
        }
    }

    pub fn set_start(&mut self) {
        self.start_flag = true;
    }
}
