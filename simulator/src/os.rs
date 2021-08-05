use crate::reg_bank::RegBank;
use std::io::{ self, BufRead, BufReader, ErrorKind, Read, Write };
use architecture_utils::reg::Reg;

pub const READ_CHAR: i16     = 1;
pub const READ_INTEGER: i16  = 2;
pub const PRINT_CHAR: i16    = 3;
pub const PRINT_DECIMAL: i16 = 4;
pub const PRINT_BINARY: i16  = 5;
pub const PRINT_HEX: i16     = 6;
pub const GET_RAND: i16      = 7;

pub fn run_interrupt<IO>(reg_bank: &mut RegBank, mut io_device: IO) -> io::Result<()>
where
    IO: Read + Write,
{
    match reg_bank[Reg::ACC] {
        READ_CHAR     => read_character(reg_bank, io_device),
        READ_INTEGER  => read_integer(reg_bank, io_device),
        PRINT_CHAR    => write!(io_device, "{}", reg_bank[Reg::R1] as u8 as char),
        PRINT_DECIMAL => write!(io_device, "{}", reg_bank[Reg::R1]),
        PRINT_BINARY  => write!(io_device, "{:b}", reg_bank[Reg::R1]),
        PRINT_HEX     => write!(io_device, "{:x}", reg_bank[Reg::R1]),
        GET_RAND      => Ok(reg_bank[Reg::ACC] = rand::random()),
        n             => {
            Err(io::Error::new(ErrorKind::Unsupported, format!("Unexpected system call: {}.", n)))
        },
    }
}

pub fn read_integer<IO>(reg_bank: &mut RegBank, io_device: IO) -> io::Result<()>
where
    IO: Read + Write,
{
    let mut input = String::new();
    let mut buf_reader = BufReader::new(io_device);
    buf_reader.read_line(&mut input)?;
    
    match input.trim().parse::<i16>() {
        Ok(n)  => Ok(reg_bank[Reg::R1] = n),
        Err(e) => Err(io::Error::new(ErrorKind::InvalidInput, e)),
    }
}

pub fn read_character<IO>(reg_bank: &mut RegBank, mut io_device: IO) -> io::Result<()>
where
    IO: Read + Write,
{
    let mut byte = [0; 1];
    io_device.read(&mut byte)?;
    reg_bank[Reg::R1] = byte[0] as i16;
    Ok(())
}
