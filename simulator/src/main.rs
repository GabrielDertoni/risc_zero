#![allow(dead_code)]

mod instruction;
mod reg;
mod reg_bank;

use instruction::*;
use reg::Reg;
use reg_bank::RegBank;

// Valor temporário
const MEMORY_SIZE: usize = 1000;

fn main() {
    let mut reg_bank = RegBank::new();
    let mut stack_memory = [0 as u8; MEMORY_SIZE];
    reg_bank[Reg::SP] = 0;
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
                reg_bank[Reg::FL] &= 0xfe;
            } else {
                reg_bank[Reg::FL] |= 0x01;
            }
        }
        Instruction::Clt(reg1, reg2) => {
            if reg_bank[reg1] < reg_bank[reg2] {
                reg_bank[Reg::FL] &= 0xfe;
            } else {
                reg_bank[Reg::FL] |= 0x01;
            }
        }

        // Stack memory related instructions
        Instruction::Ldb(reg1, reg2, immediate) => {
            let memory_final_address = (reg_bank[reg2] as u8 + immediate/2) as usize;
            reg_bank[reg1] = stack_memory[memory_final_address] as i16;

            if reg2 == Reg::SP {
                reg_bank[Reg::SP] += 2;
            }
        }
        Instruction::Stb(reg1, reg2, immediate) => {
            let memory_final_address = (reg_bank[reg2] as u8 + immediate/2) as usize;
            stack_memory[memory_final_address] = reg_bank[reg1] as u8;

            if reg2 == Reg::SP {
                reg_bank[Reg::SP] += 1;
            }
        }
        Instruction::Ldw(reg1, reg2, immediate) => {
            let memory_final_address = (reg_bank[reg2] as u8 + immediate/2) as usize;
            let lower = stack_memory[memory_final_address];
            let upper = stack_memory[memory_final_address+1];

            reg_bank[reg1] = 0;
            reg_bank[reg1] |= upper as i16;
            reg_bank[reg1] <<= 8;
            reg_bank[reg1] |= lower as i16;

            if reg2 == Reg::SP {
                reg_bank[Reg::SP] -= 2;
            }
        }
        Instruction::Stw(reg1, reg2, immediate) => {
            let memory_final_address = (reg_bank[reg2] as u8 + immediate/2) as usize;
            stack_memory[memory_final_address] = reg_bank[reg1] as u8; 
            stack_memory[memory_final_address+1] = (reg_bank[reg1] >> 8) as u8; 

            if reg2 == Reg::SP {
                reg_bank[Reg::SP] += 2;
            }
        }
        Instruction::Lui(reg1, immediate) => reg_bank[reg1] = (immediate << 8) as i16,

        _ => unreachable!(),
    };
}
