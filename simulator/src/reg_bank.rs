use std::ops::{ Index, IndexMut };

use crate::reg::{ REG_COUNT, Reg };

pub struct RegBank {
    reg_list: [i16; REG_COUNT],
}

impl RegBank {
    const ZERO_FLAG_MASK: i16  = 0b001;
    const CARRY_FLAG_MASK: i16 = 0b010;
    const NEG_FLAG_MASK: i16   = 0b100;

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

    pub fn get_zero_flag(&self) -> bool {
        (self[Reg::FL] & Self::ZERO_FLAG_MASK) == 1
    }

    pub fn get_carry_flag(&self) -> bool {
        (self[Reg::FL] & Self::CARRY_FLAG_MASK) == 1
    }

    pub fn get_negative_flag(&self) -> bool {
        (self[Reg::FL] & Self::NEG_FLAG_MASK) == 1
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
