#![allow(dead_code)]

mod instruction;

use instruction::*;

fn main() {
    let mut reg_list = [0; REG_NUMBER];
    let curr_instruction = Instruction::from(0x1790);

    match curr_instruction {
        // Arithmetic instructions
        Instruction::Mov(reg1, reg2) => reg_list[reg1 as usize] = reg_list[reg2 as usize],
        Instruction::Add(reg1, reg2) => reg_list[reg1 as usize] += reg_list[reg2 as usize],
        Instruction::Addi(reg1, immediate) => reg_list[reg1 as usize] += immediate as i32,
        Instruction::Mult(reg1, reg2) => reg_list[reg1 as usize] *= reg_list[reg2 as usize],
        Instruction::Div(reg1, reg2) => {
            match reg_list[reg2 as usize] {
                0 => panic!("Unexpected value for divider: {}. Can't divide by zero.", reg2 as i32),
                _ => {
                    reg_list[Reg::HI as usize] = reg_list[reg1 as usize] / reg_list[reg2 as usize];
                    reg_list[Reg::LO as usize] = reg_list[reg1 as usize] % reg_list[reg2 as usize];
                }
            }
        },
        
        // Logical operators instructions
        _ => unreachable!(),
    };
}
