
#[derive(Debug, PartialEq, Eq)]
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    Add(Reg, Reg),
    Mult(Reg, Reg),
    Mov(Reg, Reg),
    Div(Reg, Reg),
    Cle(Reg, Reg),
    Clt(Reg, Reg),
    Addi(Reg, u8),
    Lui(Reg, u8),
    Jmp(Reg),
    Beq(Reg),
    Bne(Reg),
    Ldb(Reg, Reg, u8),
    Stb(Reg, Reg, u8),
    Ldw(Reg, Reg, u8),
    Stw(Reg, Reg, u8),
}

impl From<u16> for Instruction {
    fn from(code: u16) -> Instruction {
        use Instruction::*;

        let opcode = (code >> 12) & 0xf;
        match opcode {
            0 => Noop,
            // Arithmetic
            1 => {
                let reg1 = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let reg2 = Reg::from_addr(((code >> 4) & 0xf) as u8);
                let opt = (code & 0xf) as u8;

                match opt {
                    // Add
                    0 => Add(reg1, reg2),
                    1 => Mult(reg1, reg2),
                    2 => Mov(reg1, reg2),
                    3 => Div(reg1, reg2),
                    4 => Cle(reg1, reg2),
                    5 => Clt(reg1, reg2),
                    _ => panic!("Unexpected arithmetic instruction: {}", opt),
                }
            },
            // Jump
            2 => {
                let reg = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let opt = (code & 0xf) as u8;

                match opt {
                    0 => Jmp(reg),
                    1 => Beq(reg),
                    2 => Bne(reg),
                    _ => panic!("Unexpected jump instruction: {}", opt),
                }
            },
            3 | 4 => {
                let reg = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let immediate: u8 = (code & 0xf) as u8;

                if code == 3 {
                    Addi(reg, immediate)
                } else {
                    Lui(reg, immediate)
                }
            },
            5 | 6 | 7 | 8 => {
                let reg1 = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let reg2 = Reg::from_addr(((code >> 4) & 0xf) as u8);
                let immediate = (code & 0xf) as u8;
                
                match opcode {
                    5 => Ldb(reg1, reg2, immediate),
                    6 => Stb(reg1, reg2, immediate),
                    7 => Ldw(reg1, reg2, immediate),
                    8 => Stw(reg1, reg2, immediate),
                    _ => unreachable!(),
                }
            },
            n => panic!("Unexpected opcode: {}", n),
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn decode_add() {
        // ADD  r1   r3   opts
        // 0001 0111 1001 0000

        assert_eq!(Instruction::from(0x1790), Instruction::Add(Reg::R1, Reg::R3));
    }
}
