#![allow(dead_code)]

mod reg_bank;
mod os;
mod utils;

use std::fs::File;
use std::io::Read;
use clap::clap_app;

use architecture_utils::*;
use reg_bank::RegBank;
use os::run_interrupt;

const MEMORY_SIZE: usize = 65_536;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = clap_app!(risc_zero =>
        (version: "0.1.0")
        (author: "Natan Sanches <natanhsanches@gmail.com>")
        (about: "Main simulator for risc_zero architecture")
        (@arg BIN: +required "A valid risc_zero binary")
    ).get_matches();

    // Load ZERO binary file
    let bin_filename = matches.value_of("BIN").unwrap();
    let mut file = File::open(bin_filename).expect("Invalid provided filename.");
    // Decode file header
    let file_header = FileHeader::read_from(&mut file).expect("Invalid file header");

    // Match and load text/data segment in memory
    let mut memory = vec![0_u8; MEMORY_SIZE];
    file.read(&mut memory[..file_header.data_seg_start as usize])?;
    file.read(&mut memory[file_header.data_seg_start as usize ..])?;

    run(memory, file_header.entry_point as usize)?;

    // Executing instructions
    println!();

    Ok(())
}


fn run(mut memory: Vec<u8>, entry_point: usize) -> Result<(), String> {
    let mut reg_bank = RegBank::new();
    let mut pc = entry_point;

    reg_bank[Reg::SP] = (memory.len() - 1) as i16;

    loop {
        let inst_word = u16::from_be_bytes([memory[pc], memory[pc + 1]]);        
        pc += 2;

        let curr_instruction = Instruction::decode(inst_word)?;

        match curr_instruction {
            Instruction::Noop => (),

            // Arithmetic instructions
            Instruction::Mov(reg1, reg2) => reg_bank[reg1] = reg_bank[reg2],
            Instruction::Add(reg1, reg2) => {
                let (result, is_overflow) = reg_bank[reg1].overflowing_add(reg_bank[reg2]);
                reg_bank[reg1] = result;

                reg_bank.set_carry_flag(is_overflow);
                reg_bank.set_zero_flag(result == 0);
                reg_bank.set_negative_flag(result.is_negative());
            }
            Instruction::Addi(reg1, imm) => {
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
            Instruction::Div(reg1, reg2) => {
                let (result, is_overflow) = reg_bank[reg1].overflowing_div_euclid(reg_bank[reg2]);
                reg_bank[Reg::HI] = result;

                reg_bank.set_carry_flag(is_overflow);
                reg_bank.set_zero_flag(result == 0);
                reg_bank.set_negative_flag(result.is_negative());

                let (result, is_overflow) = reg_bank[reg1].overflowing_rem_euclid(reg_bank[reg2]);
                reg_bank[Reg::LO] = result;
                reg_bank.set_carry_flag(is_overflow);
            }

            // Logical operators instructions
            Instruction::And(reg1, reg2)       => reg_bank[reg1] &= reg_bank[reg2],
            Instruction::Andi(reg1, immediate) => reg_bank[reg1] &= immediate as i16,
            Instruction::Or(reg1, reg2)        => reg_bank[reg1] |= reg_bank[reg2],
            Instruction::Not(reg1)             => reg_bank[reg1] = !reg_bank[reg1],
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
                let lower = memory[memory_final_address - 1];

                reg_bank[reg1] = i16::from_be_bytes([upper, lower]);
            }
            Instruction::Stw(reg1, reg2, immediate) => {
                let be_vec = reg_bank[reg1].to_be_bytes();
                let addr = (reg_bank[reg2] + immediate as i16) as usize;
                memory[addr..addr + 2].copy_from_slice(&be_vec);
            }
            Instruction::Lli(reg1, immediate) => reg_bank[reg1] = i16::from_be_bytes([0x00, immediate]),
            Instruction::Lui(reg1, immediate) => reg_bank[reg1] = i16::from_be_bytes([immediate, 0x00]),

            // Branch instructions
            Instruction::Beq(reg1) => {
                if !reg_bank.get_zero_flag() {
                    pc = reg_bank[reg1] as usize;
                }
            }
            Instruction::Bne(reg1) => {
                if reg_bank.get_zero_flag() {
                    pc = reg_bank[reg1] as usize;
                }
            }
            Instruction::Jmp(reg1) => pc = reg_bank[reg1] as usize,

            // Operational system instructions
            Instruction::Int => run_interrupt(&mut reg_bank)?,
            Instruction::Hlt => break,
        }
    }

    Ok(())
}
