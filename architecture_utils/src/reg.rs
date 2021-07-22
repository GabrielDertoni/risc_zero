pub const REG_COUNT: usize = 16;

// FL: [13b...|NEGATIVE|CARRY|ZERO]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    TMP =  0,
    HI  =  1,
    LO  =  2,
    SP  =  3,
    ADR =  4,
    ACC =  5,
    FL  =  6,
    R1  =  7,
    R2  =  8,
    R3  =  9,
    R4  = 10,
    R5  = 11,
    R6  = 12,
    R7  = 13,
    R8  = 14,
    R9  = 15,
}

impl Reg {
    pub fn from_addr(addr: u8) -> Reg {
        use Reg::*;

        match addr {
             0 => TMP,
             1 => HI,
             2 => LO,
             3 => SP,
             4 => ADR,
             5 => ACC,
             6 => FL,
             7 => R1,
             8 => R2,
             9 => R3,
            10 => R4,
            11 => R5,
            12 => R6,
            13 => R7,
            14 => R8,
            15 => R9,
            _  => panic!("Not a valid register: {}", addr),
        }
    }

    pub fn to_string(reg: Reg) -> &'static str {
        use Reg::*;

        match reg {
            TMP => "TMP",
            HI => "HI",
            LO => "LO",
            SP => "SP",
            ADR => "ADR",
            ACC => "ACC",
            FL => "FL",
            R1 => "R1",
            R2 => "R2",
            R3 => "R3",
            R4 => "R4",
            R5 => "R5",
            R6 => "R6",
            R7 => "R7",
            R8 => "R8",
            R9 => "R9",
        }
    }
}

use std::fmt;
use std::fmt::{ Display, Formatter };


impl<'a> Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Reg::*;

        let s = match self {
            TMP => "$tmp",
            HI  => "$hi",
            LO  => "$lo",
            SP  => "$sp",
            ADR => "$adr",
            ACC => "$acc",
            FL  => "$flg",
            R1  => "$r1",
            R2  => "$r2",
            R3  => "$r3",
            R4  => "$r4",
            R5  => "$r5",
            R6  => "$r6",
            R7  => "$r7",
            R8  => "$r8",
            R9  => "$r9",
        };

        write!(f, "{}", s)
    }
}
