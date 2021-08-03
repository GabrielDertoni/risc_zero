use std::fs::File;

use crate::reg_bank::RegBank;
use architecture_utils::*;
use std::io::Read;
use crate::os::*;

const MEMORY_SIZE: usize = 65_536;

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
        file.read(&mut memory[..header.data_seg_start as usize - FileHeader::SIZE as usize])?;
        file.read(&mut memory[header.data_seg_start as usize - FileHeader::SIZE as usize ..])?;

        // Setup initial register bank
        let mut reg_bank = RegBank::new();
        reg_bank[Reg::SP] = (memory.len()-1) as i16;

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

    pub fn simulate(&mut self) -> bool {
        let inst_word = u16::from_be_bytes([self.memory[self.pc], self.memory[self.pc+1]]);        
        let curr_instruction = Instruction::decode(inst_word).unwrap();
        self.pc += 2;

        match curr_instruction {
            Instruction::Noop => (),

            // Arithmetic instructions
            Instruction::Mov(reg1, reg2)  => self.reg_bank[reg1] = self.reg_bank[reg2],
            Instruction::Add(reg1, reg2)  => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_add(self.reg_bank[reg2]);
                self.reg_bank[reg1] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());
            }
            Instruction::Addi(reg1, imm)  => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_add(imm as i16);
                self.reg_bank[reg1] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());
            }
            Instruction::Mult(reg1, reg2) => {
                let (result, is_overflow) = self.reg_bank[reg1].overflowing_mul(self.reg_bank[reg2]);
                self.reg_bank[reg1] = result;

                self.reg_bank.set_carry_flag(is_overflow);
                self.reg_bank.set_zero_flag(result == 0);
                self.reg_bank.set_negative_flag(result.is_negative());
            }
            Instruction::Div(reg1, reg2)  => {
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
            Instruction::And(reg1, reg2)       => self.reg_bank[reg1] &=  self.reg_bank[reg2],
            Instruction::Andi(reg1, immediate) => self.reg_bank[reg1] &=  immediate as i16,
            Instruction::Or(reg1, reg2)        => self.reg_bank[reg1] |=  self.reg_bank[reg2],
            Instruction::Not(reg1)             => self.reg_bank[reg1] =  !self.reg_bank[reg1],
            Instruction::Shl(reg1, reg2)       => self.reg_bank[reg1] <<= self.reg_bank[reg2],
            Instruction::Shr(reg1, reg2)       => self.reg_bank[reg1] >>= self.reg_bank[reg2],

            // Comparators
            Instruction::Ceq(reg1, reg2) => {
                let comparator_result: bool = self.reg_bank[reg1] == self.reg_bank[reg2];
                self.reg_bank.set_equal_flag(comparator_result);
            }
            Instruction::Clt(reg1, reg2) => {
                let comparator_result: bool = self.reg_bank[reg1] < self.reg_bank[reg2];
                self.reg_bank.set_less_flag(comparator_result);
            }

            // Memory related instructions
            Instruction::Ldb(reg1, reg2, immediate) => {
                let memory_final_address = (self.reg_bank[reg2] + immediate as i16) as usize;
                self.reg_bank[reg1] = self.memory[memory_final_address] as i16;
            }
            Instruction::Stb(reg1, reg2, immediate) => {
                let memory_final_address = (self.reg_bank[reg2] + immediate as i16) as usize;
                self.memory[memory_final_address] = self.reg_bank[reg1] as u8;
            }
            Instruction::Ldw(reg1, reg2, immediate) => {
                let memory_final_address = (self.reg_bank[reg2] + immediate as i16) as usize;
                let upper = self.memory[memory_final_address];
                let lower = self.memory[memory_final_address-1];

                self.reg_bank[reg1] = i16::from_be_bytes([upper, lower]);
            }
            Instruction::Stw(reg1, reg2, immediate) => {
                let be_vec = self.reg_bank[reg1].to_be_bytes();
                let addr = (self.reg_bank[reg2] + immediate as i16) as usize;
                self.memory[addr..addr + 2].copy_from_slice(&be_vec);
            }
            Instruction::Lli(reg1, immediate) => self.reg_bank[reg1] = i16::from_be_bytes([0x00, immediate]),
            Instruction::Lui(reg1, immediate) => self.reg_bank[reg1] = i16::from_be_bytes([immediate, 0x00]),

            // Branch instructions
            Instruction::Beq(reg1) => {
                if !self.reg_bank.get_equal_flag() {
                    self.pc = self.reg_bank[reg1] as usize;
                }
            }
            Instruction::Bne(reg1) => {
                if self.reg_bank.get_equal_flag() {
                    self.pc = self.reg_bank[reg1] as usize;
                }
            }
            Instruction::Jmp(reg1) => self.pc = self.reg_bank[reg1] as usize,

            // Operational system instructions
            Instruction::Int => run_interrupt(&mut self.reg_bank).unwrap(),
            Instruction::Hlt => return false,
        }
        true
    }
}

impl Iterator for CPUState {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.simulate() {
            Some(self.pc)
        } else {
            None
        }
    }
}

