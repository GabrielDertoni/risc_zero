use crate::reg::Reg;

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    Add(Reg, Reg),
    Mult(Reg, Reg),
    Mov(Reg, Reg),
    Div(Reg, Reg),
    And(Reg, Reg),
    Andi(Reg, u8),
    Or(Reg, Reg),
    Not(Reg),
    Shl(Reg, Reg),
    Shr(Reg, Reg),
    Ceq(Reg, Reg),
    Clt(Reg, Reg),
    Addi(Reg, u8), // Falta implementar
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
                    4 => And(reg1, reg2),
                    5 => Or(reg1, reg2),
                    6 => Not(reg1),
                    7 => Shl(reg1, reg2),
                    8 => Shr(reg1, reg2),
                    9 => Ceq(reg1, reg2),
                    10 => Clt(reg1, reg2),
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
