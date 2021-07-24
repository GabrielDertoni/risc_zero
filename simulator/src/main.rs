#![allow(dead_code)]

mod reg_bank;
mod os;

use architecture_utils::*;
use reg_bank::RegBank;
use os::*;
use std::env;
use std::fs::File;
use std::io::{Read, Write};

const MEMORY_SIZE: usize = 65_536;
const HEADER_FILE_SIZE: usize = 10;
const INSTRUCTION_SIZE: usize = 16;

fn main() {
    let args: Vec<String> = env::args().collect();
    let bin_filename = &args[1];

    // Reading file header
    let mut file = File::open(bin_filename).expect("Invalid provided filename.");
    let mut file_as_bytes = Vec::new();
    let _bytes_read = file.read_to_end(&mut file_as_bytes);
    let file_header = match FileHeader::decode(&file_as_bytes) {
        Ok(header)  => header,
        Err(error)  => panic!("{}", error),
    };

    // Reading .text and .data segment
    let mut memory = vec![0_u8; MEMORY_SIZE];
    let text_segment_slice = &file_as_bytes[FileHeader::SIZE as usize..file_header.data_seg_start as usize];
    memory[0..text_segment_slice.len()].copy_from_slice(text_segment_slice);
    let data_segment_slice = &file_as_bytes[file_header.data_seg_start as usize..file_header.data_seg_end as usize];
    memory[text_segment_slice.len()..(file_header.data_seg_end-FileHeader::SIZE as u16) as usize].copy_from_slice(data_segment_slice);

//    let decoded: Vec<Instruction> = Instruction::decode_slice(text_segment_slice).unwrap();
//    for inst in decoded {
//        writeln!(std::io::stdout(), "{}", inst);
//    }

    // Setting up reg_bank
    let mut reg_bank = RegBank::new();
    reg_bank[Reg::SP] = (memory.len()-1) as i16;
    let mut program_counter: usize = 0;

    // Executing instructions
    loop {
        let instruction_addr = u16::from_be_bytes([memory[program_counter], memory[program_counter+1]]);        
        let curr_instruction = match Instruction::decode(instruction_addr) {
            Ok(instr)   => instr,
            Err(error)  => panic!("{}", error),
        };
        program_counter += 2;
        // writeln!(std::io::stdout(), "{}", curr_instruction);
        // println!("r1: {}", reg_bank[Reg::R1]);
        // println!("r2: {}", reg_bank[Reg::R2]);
        // let mut s = String::new();
        // std::io::stdin().read_line(&mut s).unwrap();
        match curr_instruction {
            Instruction::Noop => (),

            // Arithmetic instructions
            Instruction::Mov(reg1, reg2)  => reg_bank[reg1] = reg_bank[reg2],
            Instruction::Add(reg1, reg2)  => {
                let (result, is_overflow) = reg_bank[reg1].overflowing_add(reg_bank[reg2]);
                reg_bank[reg1] = result;

                reg_bank.set_carry_flag(is_overflow);
                reg_bank.set_zero_flag(result == 0);
                reg_bank.set_negative_flag(result.is_negative());
            }
            Instruction::Addi(reg1, imm)  => {
                let (result, is_overflow) = reg_bank[reg1].overflowing_add(imm as i16);
                reg_bank[reg1] = result;

                reg_bank.set_carry_flag(is_overflow);
                reg_bank.set_zero_flag(result == 0);
                reg_bank.set_negative_flag(result.is_negative());
            }
            Instruction::Mult(reg1, reg2) => {
                let (result, is_overflow) = reg_bank[reg1].overflowing_mul(reg_bank[reg2]);
                reg_bank[reg1] = result;

                reg_bank.set_carry_flag(is_overflow);
                reg_bank.set_zero_flag(result == 0);
                reg_bank.set_negative_flag(result.is_negative());
            }
            Instruction::Div(reg1, reg2)  => {
                let (result, is_overflow) = reg_bank[reg1].overflowing_div_euclid(reg_bank[reg2]);
                reg_bank[Reg::HI] = result;

                reg_bank.set_carry_flag(is_overflow);
                reg_bank.set_zero_flag(result == 0);
                reg_bank.set_negative_flag(result.is_negative());

                let (result, is_overflow) = reg_bank[reg1].overflowing_rem_euclid(reg_bank[reg2]);
                reg_bank[Reg::LO] = result;
                reg_bank.set_carry_flag(is_overflow);
            },
            
            // Logical operators instructions
            Instruction::And(reg1, reg2)       => reg_bank[reg1] &=  reg_bank[reg2],
            Instruction::Andi(reg1, immediate) => reg_bank[reg1] &=  immediate as i16,
            Instruction::Or(reg1, reg2)        => reg_bank[reg1] |=  reg_bank[reg2],
            Instruction::Not(reg1)             => reg_bank[reg1] =  !reg_bank[reg1],
            Instruction::Shl(reg1, reg2)       => reg_bank[reg1] <<= reg_bank[reg2],
            Instruction::Shr(reg1, reg2)       => reg_bank[reg1] >>= reg_bank[reg2],

            // Comparators
            Instruction::Ceq(reg1, reg2) => {
                let comparator_result: bool = reg_bank[reg1] == reg_bank[reg2];
                reg_bank.set_zero_flag(comparator_result);
            }
            Instruction::Clt(reg1, reg2) => {
                let comparator_result: bool = reg_bank[reg1] < reg_bank[reg2];
                reg_bank.set_zero_flag(comparator_result);
            }

            // Memory related instructions
            Instruction::Ldb(reg1, reg2, immediate) => {
                let memory_final_address = (reg_bank[reg2] + immediate as i16) as usize;
                reg_bank[reg1] = memory[memory_final_address] as i16;
            }
            Instruction::Stb(reg1, reg2, immediate) => {
                let memory_final_address = (reg_bank[reg2] + immediate as i16) as usize;
                memory[memory_final_address] = reg_bank[reg1] as u8;
            }
            Instruction::Ldw(reg1, reg2, immediate) => {
                let memory_final_address = (reg_bank[reg2] + immediate as i16) as usize;
                let upper = memory[memory_final_address];
                let lower = memory[memory_final_address-1];

                reg_bank[reg1] = i16::from_be_bytes([upper, lower]);
            }
            Instruction::Stw(reg1, reg2, immediate) => {
                let be_vec = reg_bank[reg1].to_be_bytes();
                let addr = (reg_bank[reg2] + immediate as i16) as usize;
                memory[addr..addr + 2].copy_from_slice(&be_vec);
            }
            Instruction::Lui(reg1, immediate) => reg_bank[reg1] = i16::from_be_bytes([immediate, 0x00]),

            // Branch instructions
            Instruction::Beq(reg1) => {
                if reg_bank.get_zero_flag() {
                    program_counter = reg_bank[reg1] as usize;
                }
            }
            Instruction::Bne(reg1) => {
                if !reg_bank.get_zero_flag() {
                    program_counter = reg_bank[reg1] as usize;
                }
            }
            Instruction::Jmp(reg1) => program_counter = reg_bank[reg1] as usize,

            // Operational system instructions
            Instruction::Int => {
                match reg_bank[Reg::ACC] {
                    0 => read_character(&mut reg_bank),
                    1 => print!("{}", reg_bank[Reg::R1].to_le_bytes()[0] as char),
                    n => panic!("Unexpected system call: {}.", n),
                }
            }
            Instruction::Hlt => break,
        }
    }
    println!();
}
