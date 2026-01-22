const WIDTH: usize = 256;
const HIGHT: usize = 240;

pub struct Frame {
    pub data: [u8; WIDTH * HIGHT * 3],
}

impl Frame {
    pub fn new() -> Self {
        Frame {
            data: [0u8; WIDTH * HIGHT * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * WIDTH + x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = rgb.0;
            self.data[base + 1] = rgb.1;
            self.data[base + 2] = rgb.2;
        }
    }
}
