#![allow(dead_code)]

enum Op {
    ADD,
    ADDI,
    MULT,
    DIV,
    MOV,
    AND,
    ANDI,
    OR,
    NOT,
    SHL,
    SHR,
    BEQ,
    BNE,
    CEQ,
    CLT,
    LDB,
    STB,
    LDW,
    STW,
    LUI,
}

type Inst = u16;

fn main() {
    println!("Hello, world!");
}
