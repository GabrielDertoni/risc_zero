use crate::reg::Reg;

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    Add(Reg, Reg),
    Sub(Reg, Reg),
    Mult(Reg, Reg),
    Mov(Reg, Reg),
    Div(Reg, Reg),
    And(Reg, Reg),
    Andi(Reg, u8),
    Or(Reg, Reg),
    Not(Reg),
    Shl(Reg, Reg),
    Shr(Reg, Reg),
    Cmp(Reg, Reg),
    Addi(Reg, u8), 
    Lui(Reg, u8),
    Jmp(Reg),
    Beq(Reg),
    Bne(Reg),
    Blt(Reg),
    Ble(Reg),
    Bgt(Reg),
    Bge(Reg),
    Ldb(Reg, Reg, u8),
    Stb(Reg, Reg, u8),
    Ldw(Reg, Reg, u8),
    Stw(Reg, Reg, u8),
    Int,
    Hlt,
}

impl Instruction {
    pub fn decode(code: u16) -> Result<Instruction, String> {
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
                     1 => Sub(reg1, reg2),
                     2 => Mult(reg1, reg2),
                     3 => Mov(reg1, reg2),
                     4 => Div(reg1, reg2),
                     5 => And(reg1, reg2),
                     6 => Or(reg1, reg2),
                     7 => Not(reg1),
                     8 => Shl(reg1, reg2),
                     9 => Shr(reg1, reg2),
                    10 => Cmp(reg1, reg2),
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
                    3 => Blt(reg),
                    4 => Ble(reg),
                    5 => Bgt(reg),
                    6 => Bge(reg),
                    _ => return Err(
                        format!("Unexpected jump instruction with opt {}", opt)
                    ),
                }
            },

            // I-type instructions: Immediate value related instructions
            3 | 4 | 5 => {
                let reg = Reg::from_addr(((code >> 8) & 0xf) as u8);
                let immediate: u8 = (code & 0xff) as u8;

                match opcode {
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
                let immediate = (code & 0b1111) as u8;
                
                match opcode {
                    6 => Ldb(reg1, reg2, immediate),
                    7 => Stb(reg1, reg2, immediate),
                    8 => Ldw(reg1, reg2, immediate),
                    9 => Stw(reg1, reg2, immediate),
                    _ => unreachable!(),
                }
            },

            // Operational system related instructions
            10 => Int,
            11 => Hlt,
            n => return Err(
                format!("Unexpected opcode: {}", n)
            ),
        };

        Ok(decoded)
    }

    pub fn encode(&self) -> u16 {
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

            Add(dest, src) | Sub(dest, src)  | Mult(dest, src) | Div(dest, src) |
            Mov(dest, src) | And(dest, src)  | Or(dest, src)   | Shl(dest, src) |
            Shr(dest, src) | Cmp(dest, src)  => {

                let opt = match self {
                    Add(..)  =>  0,
                    Sub(..)  =>  1,
                    Mult(..) =>  2,
                    Mov(..)  =>  3,
                    Div(..)  =>  4,
                    And(..)  =>  5,
                    Or(..)   =>  6,
                    Shl(..)  =>  8,
                    Shr(..)  =>  9,
                    Cmp(..)  => 10,
                    _        => unreachable!(),
                };

                let opcode = 1;

                encoded |= opcode << 12;
                encoded |= (dest as u16) << 8;
                encoded |= ( src as u16) << 4;
                encoded |= opt;
            }

            // J - type instructions
            Jmp(target) | Beq(target) | Bne(target) | 
            Blt(target) | Ble(target) | Bgt(target) |
            Bge(target) => {

                let opt = match self {
                    Jmp(..) => 0,
                    Beq(..) => 1,
                    Bne(..) => 2,
                    Blt(..) => 3,
                    Ble(..) => 4,
                    Bgt(..) => 5,
                    Bge(..) => 6,
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

/* Vector decoding */

use std::iter::FromIterator;

pub struct InstructionDecodeIter<I> {
    iter: I,
}

impl<I> Iterator for InstructionDecodeIter<I>
where
    I: Iterator<Item = u8>,
{
    type Item = Result<Instruction, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let be_bytes = [
            self.iter.by_ref().next(),
            self.iter.by_ref().next(),
        ];

        match be_bytes {
            [Some(fst), Some(snd)] => {
                let code = u16::from_be_bytes([fst, snd]);
                Some(Instruction::decode(code))
            }

            [Some(_), None] =>
                Some(Err(format!("expected an even number of bytes"))),

            _ => None,
        }
    }
}

impl Instruction {
    pub fn decode_iter<I>(into_iter: I) -> impl Iterator<Item = Result<Instruction, String>>
    where
        I: IntoIterator<Item = u8>,
    {
        InstructionDecodeIter { iter: into_iter.into_iter() }
    }

    pub fn decode_slice<T, R>(to_slice: T) -> Result<R, String>
    where
        T: AsRef<[u8]>,
        R: FromIterator<Instruction>,
    {
        let mut chunks_iter = to_slice.as_ref().array_chunks();

        if chunks_iter.remainder().len() > 0 {
            return Err(format!("expected slice to have an even number of elements"));
        }

        chunks_iter.by_ref()
            .map(|&be_bytes| u16::from_be_bytes(be_bytes))
            .map(|encoded| Instruction::decode(encoded))
            .collect()
    }

    pub fn encode_slice<T, R>(to_slice: T) -> R
    where
        T: AsRef<[Instruction]>,
        R: FromIterator<u8>,
    {
        to_slice.as_ref()
            .iter()
            .map(Instruction::encode)
            .flat_map(u16::to_be_bytes)
            .collect()
    }
}

impl From<u16> for Instruction {
    fn from(code: u16) -> Instruction {
        Instruction::decode(code).unwrap()
    }
}

use std::fmt::{ Display, Formatter };

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use Instruction::*;

        match self {
            Noop                 => write!(f, "noop"),
            Not(reg1)            => write!(f, "not  {}", reg1),
            Add(reg1, reg2)      => write!(f, "add  {}, {}", reg1, reg2),
            Sub(reg1, reg2)      => write!(f, "sub  {}, {}", reg1, reg2),
            Mult(reg1, reg2)     => write!(f, "mult {}, {}", reg1, reg2),
            Mov(reg1, reg2)      => write!(f, "mov  {}, {}", reg1, reg2),
            Div(reg1, reg2)      => write!(f, "div  {}, {}", reg1, reg2),
            And(reg1, reg2)      => write!(f, "and  {}, {}", reg1, reg2),
            Or(reg1, reg2)       => write!(f, "or   {}, {}", reg1, reg2),
            Shl(reg1, reg2)      => write!(f, "shl  {}, {}", reg1, reg2),
            Shr(reg1, reg2)      => write!(f, "shr  {}, {}", reg1, reg2),
            Cmp(reg1, reg2)      => write!(f, "cmp  {}, {}", reg1, reg2),
            Addi(reg1, imm)      => write!(f, "addi {}, {}", reg1, imm),
            Andi(reg1, imm)      => write!(f, "andi {}, {}", reg1, imm),
            Lui(reg1, imm)       => write!(f, "lui  {}, {}", reg1, imm),
            Jmp(reg1)            => write!(f, "jmp  {}", reg1),
            Beq(reg1)            => write!(f, "beq  {}", reg1),
            Bne(reg1)            => write!(f, "bne  {}", reg1),
            Blt(reg1)            => write!(f, "blt  {}", reg1),
            Ble(reg1)            => write!(f, "ble  {}", reg1),
            Bgt(reg1)            => write!(f, "bgt  {}", reg1),
            Bge(reg1)            => write!(f, "bge  {}", reg1),
            Ldb(reg1, reg2, imm) => write!(f, "ldb  {}, {}({})", reg1, reg2, imm),
            Stb(reg1, reg2, imm) => write!(f, "stb  {}, {}({})", reg1, reg2, imm),
            Ldw(reg1, reg2, imm) => write!(f, "ldw  {}, {}({})", reg1, reg2, imm),
            Stw(reg1, reg2, imm) => write!(f, "stw  {}, {}({})", reg1, reg2, imm),
            Int                  => write!(f, "int"),
            Hlt                  => write!(f, "hlt"),
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
