use std::f32::consts::PI;

pub struct Filter<const R: usize> {
    x: f32,
    y: f32,
    alpha: f32,
    high: bool,
}

impl<const R: usize> Filter<R> {
    pub fn new(f: f32, high: bool) -> Self {
        let rc: f32 = 1.0 / (2.0 * PI * f);
        let dt = 1.0 / R as f32;
        let alpha = if high { rc / (rc + dt) } else { dt / (rc + dt) };

        Self {
            x: 0.0,
            y: 0.0,
            alpha,
            high,
        }
    }

    pub fn filter(&mut self, input: f32) -> f32 {
        let output = if self.high {
            self.alpha * self.y + self.alpha * (input - self.x)
        } else {
            self.alpha * input + (1.0 - self.alpha) * self.y
        };

        self.x = input;
        self.y = output;
        output
    }
}
