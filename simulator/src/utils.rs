#![allow(dead_code)]

use crate::reg::Reg;
use crate::reg_bank::RegBank;

pub fn set_zero_flag(register_bank: &mut RegBank, is_zero: bool) {
    if is_zero {
        register_bank[Reg::FL] &= 0xfe;
    } else {
        register_bank[Reg::FL] |= 0x01;
    }
}

pub fn set_carry_flag(register_bank: &mut RegBank, is_carry: bool) {
    if is_carry {
        register_bank[Reg::FL] |= 0x02;
    } else {
        register_bank[Reg::FL] &= 0xfd;
    }
}

pub fn set_negative_flag(register_bank: &mut RegBank, is_neg: bool) {
    if is_neg {
        register_bank[Reg::FL] |= 0x04;
    } else {
        register_bank[Reg::FL] &= 0xfb;
    }
}

pub fn get_zero_flag(register_bank: &RegBank) -> u8 {
    (register_bank[Reg::FL] & 0x01) as u8
}

pub fn get_carry_flag(register_bank: &RegBank) -> u8 {
    (register_bank[Reg::FL] & 0x02) as u8
}

pub fn get_negative_flag(register_bank: &RegBank) -> u8 {
    (register_bank[Reg::FL] & 0x04) as u8
}
