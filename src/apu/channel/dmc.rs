use crate::cartridge::Rom;

#[rustfmt::skip]
const DMC_RATE_TABLE: [u16; 16] = [
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54
];

#[derive(Debug, Default)]
pub struct DMCChannel {
    pub irq_enabled: bool,
    pub loop_flag: bool,
    timer_period: u16,
    output_level: u8,
    sample_address: u16,
    sample_length: u16,

    timer_counter: u16,
    pub irq_flag: bool,

    sample_buffer: u8,
    sample_buffer_empty: bool,

    current_address: u16,
    bytes_remaining: u16,

    shift_register: u8,
    bits_remaining: u8,
    silence_flag: bool,
}

impl DMCChannel {
    pub fn clock_timer(&mut self, cycles: usize, rom: &mut Rom) -> u16 {
        let mut stall_cycles = 0;

        for _ in 0..cycles {
            if self.timer_counter == 0 {
                self.timer_counter = self.timer_period;
                self.clock_output_unit();
            } else {
                self.timer_counter -= 1;
            }

            // Try to fill sample buffer if empty
            if self.sample_buffer_empty && self.bytes_remaining > 0 {
                let byte = rom.read_prg(self.current_address);
                self.load_sample_buffer(byte);
                stall_cycles += 4; // DMA stalls CPU for 4 cycles
            }
        }

        stall_cycles
    }

    fn clock_output_unit(&mut self) {
        // Start new output cycle if needed
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

        // Apply bit from shift register to output level
        if !self.silence_flag {
            if self.shift_register & 1 == 1 {
                // Increment output level (clamped to 127)
                if self.output_level <= 125 {
                    self.output_level += 2;
                }
            } else {
                // Decrement output level (clamped to 0)
                if self.output_level >= 2 {
                    self.output_level -= 2;
                }
            }
        }

        self.shift_register >>= 1;
        self.bits_remaining -= 1;
    }

    fn load_sample_buffer(&mut self, byte: u8) {
        self.sample_buffer = byte;
        self.sample_buffer_empty = false;

        // Advance DMA reader
        self.current_address = if self.current_address == 0xFFFF {
            0x8000
        } else {
            self.current_address + 1
        };

        self.bytes_remaining -= 1;

        if self.bytes_remaining == 0 {
            if self.loop_flag {
                self.restart_sample();
            } else if self.irq_enabled {
                self.irq_flag = true;
            }
        }
    }

    fn restart_sample(&mut self) {
        self.current_address = self.sample_address;
        self.bytes_remaining = self.sample_length;
    }

    pub fn output(&self) -> u8 {
        self.output_level
    }

    pub fn write_flags(&mut self, val: u8) {
        self.irq_enabled = (val & 0x80) != 0;
        self.loop_flag = (val & 0x40) != 0;

        if !self.irq_enabled {
            self.irq_flag = false;
        }

        let rate_index = val & 0x0F;
        self.timer_period = DMC_RATE_TABLE[rate_index as usize];
    }

    pub fn write_output_level(&mut self, val: u8) {
        self.output_level = val & 0x7F;
    }

    pub fn write_sample_address(&mut self, val: u8) {
        self.sample_address = 0xC000 + (val as u16 * 64);
    }

    pub fn write_sample_length(&mut self, val: u8) {
        self.sample_length = (val as u16 * 16) + 1;
    }

    pub fn start_playback(&mut self) {
        if self.bytes_remaining == 0 {
            self.restart_sample();
        }
    }

    pub fn stop_playback(&mut self) {
        self.bytes_remaining = 0;
    }

    pub fn bytes_remaining(&self) -> u16 {
        self.bytes_remaining
    }
}
