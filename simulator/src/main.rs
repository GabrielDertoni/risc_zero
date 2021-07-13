#![allow(dead_code)]

mod instruction;
mod reg;
mod reg_bank;

use instruction::*;
use reg::Reg;
use reg_bank::RegBank;

fn main() {
    let mut reg_bank = RegBank::new();
    let curr_instruction = Instruction::from(0x1790);

    match curr_instruction {
        // Arithmetic instructions
        Instruction::Mov(reg1, reg2)  => reg_bank[reg1] = reg_bank[reg2],
        Instruction::Add(reg1, reg2)  => reg_bank[reg1] += reg_bank[reg2],
        Instruction::Addi(reg1, imm)  => reg_bank[reg1] += imm as i16,
        Instruction::Mult(reg1, reg2) => reg_bank[reg1] *= reg_bank[reg2],
        Instruction::Div(reg1, reg2)  => {
            match reg_bank[reg2] {
                0   => panic!("Unexpected value for divider in: {}. Can't divide by zero.", Reg::to_string(reg2)),
                div => {
                    reg_bank[Reg::HI] = reg_bank[reg1] / div;
                    reg_bank[Reg::LO] = reg_bank[reg1] % div;
                }
            }
        },
        
        // Logical operators instructions
        Instruction::And(reg1, reg2) => reg_bank[reg1] &= reg_bank[reg2],
        Instruction::Or(reg1, reg2) => reg_bank[reg1] |= reg_bank[reg2],
        Instruction::Not(reg1) => reg_bank[reg1] = !reg_bank[reg1],
        Instruction::Shl(reg1, reg2) => reg_bank[reg1] <<= reg_bank[reg2],
        Instruction::Shr(reg1, reg2) => reg_bank[reg1] >>= reg_bank[reg2],

        // Comparators
        Instruction::Ceq(reg1, reg2) => {
            if reg_bank[reg1] == reg_bank[reg2] {
                reg_bank[Reg::FL] = reg_bank[Reg::FL] & 0xfe;
            } else {
                reg_bank[Reg::FL] = reg_bank[Reg::FL] | 0x01;
            }
        }
        Instruction::Clt(reg1, reg2) => {
            if reg_bank[reg1] < reg_bank[reg2] {
                reg_bank[Reg::FL] = reg_bank[Reg::FL] & 0xfe;
            } else {
                reg_bank[Reg::FL] = reg_bank[Reg::FL] | 0x01;
            }
        }

        _ => unreachable!(),
    };
}
