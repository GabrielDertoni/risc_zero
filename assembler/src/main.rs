#![allow(dead_code)]
#![feature(box_patterns)]
#![feature(btree_drain_filter)]
#![feature(assert_matches)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_uninit_array)]

use clap::clap_app;

use std::io::Write;
use std::fs;

mod ast;
mod parser;
mod codegen;
mod utils;

// use crate::parser::parse_zasm;
// use crate::codegen::Assembler;

fn main() -> std::io::Result<()> {

    let matches = clap_app!(tapec =>
        (version: "0.1.0")
        (author: "Gabriel Dertoni <gab.dertoni@gmail.com>")
        (about: "A compiler for the ZASM programming language")
        (@arg SOURCE: +required "The ZASM source file to compile")
        (@arg output: -o --output +takes_value "Output compiled tape")
        (@arg expand: -E --expand "Compile and also print the desugared code")
    ).get_matches();

    // Ok, SOURCE is required.
    let src_file = matches.value_of("SOURCE").unwrap();
    let out = matches.value_of("output").unwrap_or("a.out");
    let expand = matches.is_present("expand");

    let source = fs::read_to_string(src_file)?;

    Ok(())
}
