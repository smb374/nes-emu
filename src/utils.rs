pub fn ror(val: u8, car: bool) -> (u8, bool) {
    let res = (val >> 1) | ((car as u8) << 7);
    (res, val & 1 != 0)
}

pub fn rol(val: u8, car: bool) -> (u8, bool) {
    let res = (val << 1) | (car as u8);
    (res, val & 0x80 != 0)
}
