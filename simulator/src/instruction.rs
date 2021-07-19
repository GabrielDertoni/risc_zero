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
    Addi(Reg, u8), 
    Lui(Reg, u8),
    Jmp(Reg),
    Beq(Reg),
    Bne(Reg),
    Ldb(Reg, Reg, u8),
    Stb(Reg, Reg, u8),
    Ldw(Reg, Reg, u8),
    Stw(Reg, Reg, u8),
    Int,
    Hlt,
}

impl Instruction {
    fn decode(code: u16) -> Result<Instruction, String> {
        use Instruction::*;

        let opcode = (code >> 12) & 0xf;
        let decoded = match opcode {
            0 => Noop,

            // R-type instructions: Register to register related instructions
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
                    _  => return Err(
                        format!("Unexpected arithmetic instruction: {}", opt)
                    ),
                }
            },

            // J-type instructions: Branch related instructions
            2 => {
                let reg = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let opt = (code & 0xf) as u8;

                match opt {
                    0 => Jmp(reg),
                    1 => Beq(reg),
                    2 => Bne(reg),
                    _ => return Err(
                        format!("Unexpected jump instruction: {}", opt)
                    ),
                }
            },

            // I-type instructions: Immediate value related instructions
            3 | 4 | 5 => {
                let reg = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let immediate: u8 = (code & 0xf) as u8;

                match code {
                    3 => Addi(reg, immediate),
                    4 => Lui(reg, immediate),
                    5 => Andi(reg, immediate),
                    _ => unreachable!(),
                }
            },

            // M-type instructions: Memory related
            6 | 7 | 8 | 9 => {
                let reg1 = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let reg2 = Reg::from_addr(((code >> 4) & 0xf) as u8);
                let immediate = (code & 0xf) as u8;
                
                match opcode {
                    6 => Ldb(reg1, reg2, immediate),
                    7 => Stb(reg1, reg2, immediate),
                    8 => Ldw(reg1, reg2, immediate),
                    9 => Stw(reg1, reg2, immediate),
                    _ => unreachable!(),
                }
            },
            10 => Int,
            11 => Hlt,
            n => return Err(
                format!("Unexpected opcode: {}", n)
            ),
        };

        Ok(decoded)
    }

    fn encode(&self) -> u16 {
        use Instruction::*;

        let mut encoded: u16 = 0;

        match *self {
            Noop => (),

            // R - type instructions
            Not(dest) => {
                let opt = 6;
                let opcode = 1;
                encoded |= opcode << 12;
                encoded |= (dest as u16) << 8;
                encoded |= opt;
            }

            Add(dest, src) | Mult(dest, src) | Div(dest, src) | Mov(dest, src) |
            And(dest, src) | Or(dest, src)   | Shl(dest, src) |
            Shr(dest, src) | Ceq(dest, src)  | Clt(dest, src) => {

                let opt = match self {
                    Add(..)  =>  0,
                    Mult(..) =>  1,
                    Mov(..)  =>  2,
                    Div(..)  =>  3,
                    And(..)  =>  4,
                    Or(..)   =>  5,
                    Shl(..)  =>  7,
                    Shr(..)  =>  8,
                    Ceq(..)  =>  9,
                    Clt(..)  => 10,
                    _        => unreachable!(),
                };

                let opcode = 1;

                encoded |= opcode << 12;
                encoded |= (dest as u16) << 8;
                encoded |= ( src as u16) << 4;
                encoded |= opt;
            }

            // J - type instructions
            Beq(target) | Bne(target) | Jmp(target)  => {

                let opt = match self {
                    Jmp(..) => 0,
                    Beq(..) => 1,
                    Bne(..) => 2,
                    _       => unreachable!(),
                };
                
                let opcode = 2;

                encoded |= opcode << 12;
                encoded |= (target as u16) << 8;
                encoded |= opt;
            }

            // I - type instructions
            Addi(dest, imm) | Andi(dest, imm) | Lui(dest, imm) => {
                let opcode = match self {
                    Addi(..) => 3,
                    Lui(..)  => 4,
                    Andi(..) => 5,
                    _        => unreachable!(),
                };

                encoded |= opcode << 12;
                encoded |= (dest as u16) << 8;
                encoded |= imm as u16;
            }

            Ldb(dest, reg, imm) | Stb(dest, reg, imm) |
            Ldw(dest, reg, imm) | Stw(dest, reg, imm)  => {
                let opcode = match self {
                    Ldb(..) => 6,
                    Stb(..) => 7,
                    Ldw(..) => 8,
                    Stw(..) => 9,
                    _       => unreachable!(),
                };

                encoded |= opcode << 12;
                encoded |= (dest as u16) << 8;
                encoded |= ( reg as u16) << 4;
                encoded |= imm as u16;
            }

            Int  => {
                let opcode = 10;
                encoded |= opcode << 12;
            }

            Hlt => {
                let opcode = 11;
                encoded |= opcode << 12;
            }
        }

        encoded
    }
}

impl From<u16> for Instruction {
    fn from(code: u16) -> Instruction {
        Instruction::decode(code).unwrap()
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
