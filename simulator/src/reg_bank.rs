use std::ops::{ Index, IndexMut };

use crate::reg::{ REG_COUNT, Reg };

pub struct RegBank {
    reg_list: [i16; REG_COUNT],
}

impl RegBank {
    pub fn new() -> RegBank {
        RegBank {
            reg_list: [0; REG_COUNT],
        }
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
