use crate::reg_bank::RegBank;
use crate::cpu_state::CPUState;
use std::io;
use architecture_utils::reg::Reg;

pub const READ_CHAR: i16 = 1;
pub const READ_INTEGER: i16 = 2;
pub const PRINT_CHAR: i16 = 3;
pub const PRINT_DECIMAL: i16 = 4;
pub const PRINT_BINARY: i16 = 5;
pub const PRINT_HEX: i16 = 6;

pub fn read_integer(reg_bank: &mut RegBank) {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Invalid input from stdin.");
    
    let trimmed_input = input.trim();
    match trimmed_input.parse::<i16>() {
        Ok(n)   => reg_bank[Reg::R1] = n,
        Err(..) => println!("Input is not a valid integer."),
    }
}

pub fn read_character(reg_bank: &mut RegBank) {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Invalid input from stdin.");

    let trimmed_input = input.trim();
    match trimmed_input.parse::<char>() {
        Ok(n)   => reg_bank[Reg::R1] = n as i16,
        Err(..) => println!("Input is not a valid character."),
    }
}

pub fn match_syscall(curr_state: &mut CPUState) {
    match curr_state.reg_bank[Reg::ACC] {
        READ_CHAR     => read_character(&mut curr_state.reg_bank),
        READ_INTEGER  => read_integer(&mut curr_state.reg_bank),
        PRINT_CHAR    => print!("{}", curr_state.reg_bank[Reg::R1].to_le_bytes()[0] as char),
        PRINT_DECIMAL => print!("{}", curr_state.reg_bank[Reg::R1]),
        PRINT_BINARY  => print!("{:b}", curr_state.reg_bank[Reg::R1]),
        PRINT_HEX     => print!("{:x}", curr_state.reg_bank[Reg::R1]),
        n             => panic!("Unexpected system call: {}.", n),
    }
}
