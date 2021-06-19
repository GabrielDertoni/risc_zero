#![allow(dead_code)]

enum OpCode {
    NOOP,
    ARITH,
    ADDI,
    JUMP,
    LDB,
    STB,
    LDW,
    STW,
    LUI,
}

struct Inst(u16);

impl Inst {
    fn op_code(&self) -> OpCode {
        use OpCode::*;

        match (self.0 >> 12) & 0xf {
            0 => NOOP,
            1 => ARITH,
            2 => ADDI,
            3 => JUMP,
            4 => LDB,
            5 => STB,
            6 => LDW,
            7 => STW,
            8 => LUI,
            n => panic!("Unexpected opcode: {}", n),
        }
    }
}

fn main() {
    println!("Hello, world!");
}
