#![allow(dead_code)]

mod reg_bank;
mod os;
mod utils;

use architecture_utils::*;
use reg_bank::RegBank;
use utils::*;
use std::fs::File;
use std::io::Read;
use clap::clap_app;

const MEMORY_SIZE: usize = 65_536;

fn main() -> std::io::Result<()> {
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

    // Setup register bank, stack pointer and program counter
    let mut reg_bank = RegBank::new();
    reg_bank[Reg::SP] = (memory.len()-1) as i16;
    let mut program_counter: usize = file_header.entry_point as usize;

    // Executing instructions
    loop {
        let inst_word = u16::from_be_bytes([memory[program_counter], memory[program_counter+1]]);        
        let curr_instruction = Instruction::decode(inst_word).unwrap();
        program_counter += 2;

        if execute_instruction(curr_instruction, &mut reg_bank, &mut memory, &mut program_counter) {
            break;
        }
    }
    println!();

    Ok(())
}
