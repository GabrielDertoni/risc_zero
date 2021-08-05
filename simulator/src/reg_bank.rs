use crate::reg::{ REG_COUNT, Reg };

use std::ops::Index;
use std::ops::IndexMut;

pub struct RegBank {
    reg_list: [i16; REG_COUNT],
}

impl RegBank {
    pub const ZERO_FLAG_MASK: i16  = 0b00001;
    pub const CARRY_FLAG_MASK: i16 = 0b00010;
    pub const NEG_FLAG_MASK: i16   = 0b00100;
    pub const EQUAL_FLAG_MASK: i16 = 0b01000;
    pub const LESS_FLAG_MASK: i16  = 0b10000;

    pub fn new() -> RegBank {
        RegBank {
            reg_list: [0; REG_COUNT],
        }
    }

    pub fn set_zero_flag(&mut self, is_zero: bool) {
        if is_zero {
            self[Reg::FL] |= Self::ZERO_FLAG_MASK;
        } else {
            self[Reg::FL] &= !Self::ZERO_FLAG_MASK;
        }
    }

    pub fn set_carry_flag(&mut self, is_carry: bool) {
        if is_carry {
            self[Reg::FL] |= Self::CARRY_FLAG_MASK;
        } else {
            self[Reg::FL] &= !Self::CARRY_FLAG_MASK;
        }
    }

    pub fn set_negative_flag(&mut self, is_neg: bool) {
        if is_neg {
            self[Reg::FL] |= Self::NEG_FLAG_MASK;
        } else {
            self[Reg::FL] &= !Self::NEG_FLAG_MASK;
        }
    }

    pub fn set_equal_flag(&mut self, is_eq: bool) {
        if is_eq {
            self[Reg::FL] |= Self::EQUAL_FLAG_MASK;
        } else {
            self[Reg::FL] &= !Self::EQUAL_FLAG_MASK;
        }
    }

    pub fn set_less_flag(&mut self, is_less: bool) {
        if is_less {
            self[Reg::FL] |= Self::LESS_FLAG_MASK;
        } else {
            self[Reg::FL] &= !Self::LESS_FLAG_MASK;
        }
    }

    pub fn get_zero_flag(&self) -> bool {
        (self[Reg::FL] & Self::ZERO_FLAG_MASK) != 0
    }

    pub fn get_carry_flag(&self) -> bool {
        (self[Reg::FL] & Self::CARRY_FLAG_MASK) != 0
    }

    pub fn get_negative_flag(&self) -> bool {
        (self[Reg::FL] & Self::NEG_FLAG_MASK) != 0
    }

    pub fn get_equal_flag(&self) -> bool {
        (self[Reg::FL] & Self::EQUAL_FLAG_MASK) != 0
    }

    pub fn get_less_flag(&self) -> bool {
        (self[Reg::FL] & Self::LESS_FLAG_MASK) != 0
    }
}

impl Default for RegBank {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<Reg> for RegBank {
    type Output = i16;

    #[inline]
    fn index(&self, index: Reg) -> &i16 {
        &self.reg_list[index as usize]
    }
}

impl IndexMut<Reg> for RegBank {
    fn index_mut(&mut self, index: Reg) -> &mut i16 {
        &mut self.reg_list[index as usize]
    }
}
