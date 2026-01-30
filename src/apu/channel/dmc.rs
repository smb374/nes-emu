use crate::cartridge::Rom;

#[rustfmt::skip]
const RATE_TABLE: [u16; 16] = [
    428, 380, 340, 320, 286, 254, 226, 214,
    190, 160, 142, 128, 106,  84,  72,  54,
];

#[derive(Debug)]
pub struct DMCChannel {
    // Control register ($4010)
    irq_enabled: bool,
    loop_flag: bool,
    rate_index: u8,

    // Direct load register ($4011)
    pub output_level: u8,

    // Sample address ($4012)
    sample_address: u16,

    // Sample length ($4013)
    sample_length: u16,

    // Internal state
    timer_period: u16,
    timer_counter: u16,

    // Sample buffer
    sample_buffer: u8,
    sample_buffer_empty: bool,

    // Shift register
    shift_register: u8,
    bits_remaining: u8,
    silence_flag: bool,

    // Memory reader
    current_address: u16,
    bytes_remaining: u16,

    // IRQ flag
    pub irq_flag: bool,
}

impl DMCChannel {
    pub fn new() -> Self {
        Self {
            irq_enabled: false,
            loop_flag: false,
            rate_index: 0,
            output_level: 0,
            sample_address: 0xC000,
            sample_length: 0,
            timer_period: RATE_TABLE[0],
            timer_counter: RATE_TABLE[0],
            sample_buffer: 0,
            sample_buffer_empty: true,
            shift_register: 0,
            bits_remaining: 0,
            silence_flag: true,
            current_address: 0xC000,
            bytes_remaining: 0,
            irq_flag: false,
        }
    }

    /// Update control register ($4010)
    pub fn update_control(&mut self, val: u8) {
        self.irq_enabled = (val & 0x80) != 0;
        self.loop_flag = (val & 0x40) != 0;
        self.rate_index = val & 0x0F;
        self.timer_period = RATE_TABLE[self.rate_index as usize];

        if !self.irq_enabled {
            self.irq_flag = false;
        }
    }

    /// Update direct load register ($4011)
    pub fn update_direct_load(&mut self, val: u8) {
        self.output_level = val & 0x7F;
    }

    /// Update sample address register ($4012)
    pub fn update_sample_address(&mut self, val: u8) {
        // Sample address = %11AAAAAA.AA000000 = $C000 + (A * 64)
        self.sample_address = 0xC000 | ((val as u16) << 6);
    }

    /// Update sample length register ($4013)
    pub fn update_sample_length(&mut self, val: u8) {
        // Sample length = %LLLL.LLLL0001 = (L * 16) + 1 bytes
        self.sample_length = ((val as u16) << 4) | 1;
    }

    /// Start playback
    pub fn start(&mut self) {
        if self.bytes_remaining == 0 {
            self.restart_sample();
        }
    }

    pub fn stop(&mut self) {
        self.bytes_remaining = 0;
    }

    /// Restart sample from beginning
    fn restart_sample(&mut self) {
        self.current_address = self.sample_address;
        self.bytes_remaining = self.sample_length;
    }

    /// Clock the timer
    pub fn clock_timer(&mut self, cycles: usize, rom: &mut Rom) {
        for _ in 0..cycles {
            // Timer
            if self.timer_counter == 0 {
                self.timer_counter = self.timer_period;
                self.clock_output_unit();
            } else {
                self.timer_counter -= 1;
            }

            // Memory reader
            if self.sample_buffer_empty && self.bytes_remaining > 0 {
                // Read from CPU memory
                self.sample_buffer = rom.read_prg(self.current_address);
                self.sample_buffer_empty = false;

                // Advance address with wrapping
                if self.current_address == 0xFFFF {
                    self.current_address = 0x8000;
                } else {
                    self.current_address += 1;
                }

                self.bytes_remaining -= 1;

                if self.bytes_remaining == 0 {
                    if self.loop_flag {
                        self.restart_sample();
                    } else if self.irq_enabled {
                        self.irq_flag = true;
                    }
                }
            }
        }
    }

    /// Clock the output unit
    fn clock_output_unit(&mut self) {
        if !self.silence_flag {
            // Check bit 0 of shift register
            if (self.shift_register & 1) != 0 {
                // If output level <= 125, add 2
                if self.output_level <= 125 {
                    self.output_level += 2;
                }
            } else {
                // If output level >= 2, subtract 2
                if self.output_level >= 2 {
                    self.output_level -= 2;
                }
            }

            // Shift the register right
            self.shift_register >>= 1;
        }

        self.bits_remaining -= 1;

        if self.bits_remaining == 0 {
            self.bits_remaining = 8;

            if self.sample_buffer_empty {
                self.silence_flag = true;
            } else {
                self.silence_flag = false;
                self.shift_register = self.sample_buffer;
                self.sample_buffer_empty = true;
            }
        }
    }

    /// Get output sample
    pub fn output(&self) -> u8 {
        self.output_level
    }

    /// Check if sample is playing
    pub fn is_active(&self) -> bool {
        self.bytes_remaining > 0
    }

    /// Get bytes remaining for status register
    pub fn bytes_remaining(&self) -> u16 {
        self.bytes_remaining
    }

    /// Clear interrupt flag (when $4015 is read)
    pub fn clear_interrupt(&mut self) {
        self.irq_flag = false;
    }
}

impl Default for DMCChannel {
    fn default() -> Self {
        Self::new()
    }
}
