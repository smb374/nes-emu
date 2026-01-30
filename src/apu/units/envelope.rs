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
    pub fn clock(&mut self, loop_flag: bool, const_volume: bool, load_volume: u8) {
        if self.start_flag {
            self.decay_level = 15;
            self.divider = load_volume;
            self.start_flag = false;
        }

        if self.divider == 0 {
            self.divider = load_volume;
            if self.decay_level != 0 {
                self.decay_level -= 1;
            } else if loop_flag {
                self.decay_level = 15;
            }
        }

        if const_volume {
            self.output = load_volume;
        } else {
            self.output = self.decay_level;
        }
    }

    pub fn set_start(&mut self) {
        self.start_flag = true;
    }
}
