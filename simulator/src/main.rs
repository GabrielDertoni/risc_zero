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

fn main() {
    let matches = clap_app!(risc_zero =>
        (version: "0.1.0")
        (author: "Natan Sanches <natanhsanches@gmail.com>")
        (about: "Main simulator for risc_zero architecture")
        (@arg BIN: +required "A valid risc_zero binary")
    ).get_matches();

    // Load ZERO binary file
    let bin_filename = matches.value_of("BIN").unwrap();
    let mut file = File::open(bin_filename).expect("Invalid provided filename.");
    let mut file_as_bytes = Vec::new();
    match file.read_to_end(&mut file_as_bytes) {
        Ok(n)   => n,
        Err(e)  => panic!("{}", e),
    };

    // Decode file header
    let file_header = match FileHeader::decode(&file_as_bytes) {
        Ok(header)  => header,
        Err(error)  => panic!("{}", error),
    };

    // Match and load text/data segment in memory
    let mut memory = vec![0_u8; MEMORY_SIZE];
    let text_segment_slice = &file_as_bytes[FileHeader::SIZE as usize..file_header.data_seg_start as usize];
    memory[0..text_segment_slice.len()].copy_from_slice(text_segment_slice);
    let data_segment_slice = &file_as_bytes[file_header.data_seg_start as usize..file_header.data_seg_end as usize];
    memory[text_segment_slice.len()..(file_header.data_seg_end-FileHeader::SIZE as u16) as usize].copy_from_slice(data_segment_slice);

    // Setup register bank, stack pointer and program counter
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

        if execute_instruction(curr_instruction, &mut reg_bank, &mut memory, &mut program_counter) {
            break;
        }
    }
    println!();
}
