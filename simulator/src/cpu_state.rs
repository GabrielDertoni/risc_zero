use std::fs::File;

use crate::reg_bank::RegBank;
use architecture_utils::*;
use std::io::{ Read, Write };
use crate::os::*;

pub const SCREEN_WIDTH: usize       = 60;
pub const SCREEN_HEIGHT: usize      = 20;
pub const KEYBOARD_START: usize     = SCREEN_HEIGHT * SCREEN_WIDTH;
pub const KEYBOARD_QUEUE_END: usize = KEYBOARD_START + 8;
pub const TEXT_START: usize         = KEYBOARD_START + 8 + 4;
pub const MEMORY_SIZE: usize        = 65_536;

pub struct CPUState {
    pub file: File,
    pub header: FileHeader,
    pub memory: Vec<u8>,
    pub reg_bank: RegBank,
    pub pc: usize,
}

impl CPUState {
    pub fn new(filename: &str) -> Result<CPUState, std::io::Error> {
        let mut file = File::open(filename).expect("Invalid provided file");
        let header = FileHeader::read_from(&mut file).expect("Invalid file header");
        let mut memory = vec![0_u8; MEMORY_SIZE];

        // Load text/data segment into memory
        let text_len = header.data_seg_start as usize - FileHeader::SIZE as usize;
        let data_start = TEXT_START + text_len;
        file.read(&mut memory[TEXT_START..data_start])?;
        file.read(&mut memory[data_start..])?;
        memory[..KEYBOARD_START].fill(b' ');
        memory[KEYBOARD_START..TEXT_START    ].fill(0);
        memory[TEXT_START - 4..TEXT_START - 2].copy_from_slice(&(KEYBOARD_START as u16).to_be_bytes());
        memory[TEXT_START - 2..TEXT_START    ].copy_from_slice(&(KEYBOARD_START as u16).to_be_bytes());

        // Setup initial register bank
        let mut reg_bank = RegBank::new();
        reg_bank[Reg::SP] = (memory.len() - 2) as i16;

        // Points PC to the main label
        let pc = header.entry_point as usize;
        Ok(
            CPUState {
                file,
                header,
                memory,
                reg_bank,
                pc,
            }
        )
    }

    pub fn simulate<IO: Read + Write>(&mut self, io_device: IO) -> std::io::Result<bool> {
        use Instruction::*;

        let inst_word = u16::from_be_bytes([self.memory[self.pc], self.memory[self.pc+1]]);        
        let curr_instruction = Instruction::decode(inst_word).unwrap();
        self.pc += 2;

        match curr_instruction {
            Noop => (),

            // Arithmetic instructions
            Mov(reg1, reg2)  => self.reg_bank[reg1] = self.reg_bank[reg2],
            Add(reg1, reg2)  => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_add(self.reg_bank[reg2]);
                self.reg_bank[reg1] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());
            }
            Sub(reg1, reg2) => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_sub(self.reg_bank[reg2]);
                self.reg_bank[reg1] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());
            }
            Addi(reg1, imm)  => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_add(imm as i16);
                self.reg_bank[reg1] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());
            }
            Mult(reg1, reg2) => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_mul(self.reg_bank[reg2]);
                self.reg_bank[reg1] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());
            }
            Div(reg1, reg2)  => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_div_euclid(self.reg_bank[reg2]);
                self.reg_bank[Reg::HI] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());

                let (result, is_overflow) = self.reg_bank[reg1].overflowing_rem_euclid(self.reg_bank[reg2]);
                self.reg_bank[Reg::LO] = result;
                self.reg_bank.set_carry_flag(is_overflow);
            },
            
            // Logical operators instructions
            And(reg1, reg2)       => self.reg_bank[reg1] &=  self.reg_bank[reg2],
            Andi(reg1, immediate) => self.reg_bank[reg1] &=  immediate as i16,
            Or(reg1, reg2)        => self.reg_bank[reg1] |=  self.reg_bank[reg2],
            Not(reg1)             => self.reg_bank[reg1] =  !self.reg_bank[reg1],
            Shl(reg1, reg2)       => self.reg_bank[reg1] <<= self.reg_bank[reg2],
            Shr(reg1, reg2)       => self.reg_bank[reg1] >>= self.reg_bank[reg2],

            // Comparators
            Cmp(reg1, reg2) => {
                self.reg_bank.set_zero_flag(self.reg_bank[reg1] == self.reg_bank[reg2]);
                self.reg_bank.set_negative_flag(self.reg_bank[reg1] < self.reg_bank[reg2]);
            }

            // Memory related instructions
            Ldb(reg1, reg2, immediate) => {
                let memory_final_address = (self.reg_bank[reg2] + immediate as i16) as usize;
                self.reg_bank[reg1] = self.memory[memory_final_address] as i16;
            }
            Stb(reg1, reg2, immediate) => {
                let memory_final_address = (self.reg_bank[reg2] + immediate as i16) as usize;
                self.memory[memory_final_address] = self.reg_bank[reg1] as u8;
            }
            Ldw(reg1, reg2, immediate) => {
                let memory_final_address = ((self.reg_bank[reg2] as u16 as i32) + immediate as i32) as usize;
                let upper = self.memory[memory_final_address];
                let lower = self.memory[memory_final_address + 1];

                self.reg_bank[reg1] = i16::from_be_bytes([upper, lower]);
            }
            Stw(reg1, reg2, immediate) => {
                let be_vec = self.reg_bank[reg1].to_be_bytes();

                // TODO: Find a better way of doing this!
                let addr = ((self.reg_bank[reg2] as u16 as i32) + immediate as i32) as usize;
                self.memory[addr..addr + 2].copy_from_slice(&be_vec);
            }
            Lui(reg1, immediate) => self.reg_bank[reg1] = i16::from_be_bytes([immediate, 0x00]),

            // Branch instructions
            Jmp(reg1) => self.pc = self.reg_bank[reg1] as usize,
            Beq(reg1) => if self.reg_bank.get_zero_flag() {
                self.pc = self.reg_bank[reg1] as usize;
            }
            Bne(reg1) => if !self.reg_bank.get_zero_flag() {
                self.pc = self.reg_bank[reg1] as usize;
            }
            Blt(reg1) => if self.reg_bank.get_negative_flag() {
                self.pc = self.reg_bank[reg1] as usize;
            }
            Ble(reg1) => if self.reg_bank.get_negative_flag() || self.reg_bank.get_zero_flag() {
                self.pc = self.reg_bank[reg1] as usize;
            }
            Bgt(reg1) => if !self.reg_bank.get_negative_flag() && !self.reg_bank.get_zero_flag() {
                self.pc = self.reg_bank[reg1] as usize;
            }
            Bge(reg1) => if !self.reg_bank.get_negative_flag() {
                self.pc = self.reg_bank[reg1] as usize;
            }

            // Operational system instructions
            Int => run_interrupt(&mut self.reg_bank, io_device)?,
            Hlt => return Ok(false),
        }
        Ok(true)
    }

    pub fn write_keyboard_input(&mut self, key: u8) {
        let queue_end = u16::from_be_bytes([self.memory[KEYBOARD_QUEUE_END], self.memory[KEYBOARD_QUEUE_END + 1]]);
        self.memory[queue_end as usize] = key;
        let queue_end = ((queue_end + 1) & 0b111) + (queue_end & 0xfff8);
        self.memory[KEYBOARD_QUEUE_END..KEYBOARD_QUEUE_END + 2].copy_from_slice(&queue_end.to_be_bytes());
    }
}

