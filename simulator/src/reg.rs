pub const REG_COUNT: usize = 16;

// FL: [...|ZERO]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    TMP,
    HI,
    LO,
    SP,
    ADR,
    ACC,
    FL,
    R1, R2, R3, R4, R5, R6, R7, R8, R9,
}

impl Reg {
    pub fn from_addr(addr: u8) -> Reg {
        use Reg::*;

        match addr {
            0  => TMP,
            1  => HI,
            2  => LO,
            3  => SP,
            4  => ADR,
            5  => ACC,
            6  => FL,
            7  => R1,
            8  => R2,
            9  => R3,
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

