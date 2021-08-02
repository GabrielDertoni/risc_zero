use crate::reg_bank::RegBank;
use std::io;
use architecture_utils::reg::Reg;

pub const READ_CHAR: i16 = 1;
pub const READ_INTEGER: i16 = 2;
pub const PRINT_CHAR: i16 = 3;
pub const PRINT_DECIMAL: i16 = 4;
pub const PRINT_BINARY: i16 = 5;
pub const PRINT_HEX: i16 = 6;

pub fn run_interrupt(reg_bank: &mut RegBank) -> Result<(), String> {
    match reg_bank[Reg::ACC] {
        READ_CHAR     => read_character(reg_bank),
        READ_INTEGER  => read_integer(reg_bank),
        PRINT_CHAR    => print!("{}", reg_bank[Reg::R1].to_le_bytes()[0] as char),
        PRINT_DECIMAL => print!("{}", reg_bank[Reg::R1]),
        PRINT_BINARY  => print!("{:b}", reg_bank[Reg::R1]),
        PRINT_HEX     => print!("{:x}", reg_bank[Reg::R1]),
        n             => return Err(format!("Unexpected system call: {}.", n)),
    }
    Ok(())
}

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

    match input.trim().parse::<char>() {
        Ok(n)   => reg_bank[Reg::R1] = n as i16,
        Err(..) => println!("Input is not a valid character."),
    }
}
