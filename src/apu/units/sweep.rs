#[derive(Debug)]
pub struct Sweep {
    use_2c: bool,
    divide: u8,
    reload: bool,
}

impl Sweep {
    pub fn new(use_2c: bool) -> Self {
        Self {
            use_2c,
            divide: 0,
            reload: true,
        }
    }

    // Reg: EPPP.NSSS, -> (new period, muted)
    pub fn clock(&mut self, reg_val: u8, timer: u16) -> (u16, bool) {
        let shamt = reg_val & 0x07;
        let negate = reg_val & 0x08 != 0;
        let enabled = reg_val & 0x80 != 0;

        let delta = timer >> shamt;
        let target = if negate {
            let neg_delta = (!delta).wrapping_add(if self.use_2c { 1 } else { 0 });
            timer.wrapping_add(neg_delta)
        } else {
            timer.wrapping_add(delta)
        };

        let muted = timer < 8 || target > 0x7FF;

        let ntimer = if self.divide == 0 && enabled && !muted && shamt > 0 {
            target
        } else {
            timer
        };

        if self.divide == 0 || self.reload {
            self.divide = (reg_val & 0x70) >> 4;
            self.reload = false;
        } else {
            self.divide -= 1;
        }

        (ntimer, muted)
    }
}
