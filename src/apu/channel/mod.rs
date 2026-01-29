pub mod noise;
pub mod pulse;
pub mod triag;

pub trait TimedChannel {
    fn update_period_lo(&mut self, val: u8);
    fn update_period_hi(&mut self, val: u8);
}
