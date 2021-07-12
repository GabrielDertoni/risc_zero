#![allow(dead_code)]

mod instruction;

use instruction::*;

fn main() {
    let mut register_list: Vec<i32> = vec![0; REG_NUMBER];
    let curr_instruction: Instruction = Instruction::from(0x1790);

    match curr_instruction {
        // Arithmetic instructions
        Instruction::Mov(reg1, reg2) => register_list[reg1 as usize] = register_list[reg2 as usize],
        Instruction::Add(reg1, reg2) => register_list[reg1 as usize] += register_list[reg2 as usize],
        Instruction::Addi(reg1, immediate) => register_list[reg1 as usize] += immediate as i32,
        Instruction::Mult(reg1, reg2) => register_list[reg1 as usize] *= register_list[reg2 as usize],
        Instruction::Div(reg1, reg2) => {
            match register_list[reg2 as usize] {
                0 => panic!("Unexpected value for divider: {}. Can't divide by zero.", reg2 as i32),
                _ => {
                    register_list[Reg::HI as usize] = register_list[reg1 as usize] / register_list[reg2 as usize];
                    register_list[Reg::LO as usize] = register_list[reg1 as usize] % register_list[reg2 as usize];
                }
            }
        },
        
        // Logical operators instructions
        _ => unreachable!(),
    };
}
