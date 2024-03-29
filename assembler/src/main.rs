#![feature(box_patterns)]
#![feature(btree_drain_filter)]
#![feature(assert_matches)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_uninit_array)]

use std::fs;

use clap::clap_app;

mod ast;
mod parser;
mod codegen;
mod error;
mod utils;

use crate::codegen::Assembler;

fn main() -> Result<(), Box<dyn std::error::Error>> {

    let matches = clap_app!(zasmc =>
        (version: "0.1.0")
        (author: "Gabriel Dertoni <gab.dertoni@gmail.com>")
        (about: "A assembler for the ZASM assembly language")
        (@arg SOURCE: +required "The ZASM source file to compile")
        (@arg output: -o --output +takes_value "File to output the assembled binary")
        (@arg offset: --offset +takes_value "Memory offset")
    ).get_matches();

    // Ok, SOURCE is required.
    let src_file = matches.value_of("SOURCE").unwrap();
    let out = matches.value_of("output").unwrap_or("a.out");
    let offset: u16 = matches.value_of("offset")
        .map(str::parse)
        .unwrap_or(Ok(0))?;

    let file = fs::File::create(out)?;
    let mut assembler = Assembler::new(file, offset);

    assembler.assemble_program(src_file)
        .unwrap_or_else(|err| {
            eprintln!("{}", err);
        });

    Ok(())
}
