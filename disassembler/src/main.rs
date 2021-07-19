
use std::fs;
use std::io::{ Read, Write };

use architecture_utils::Instruction;

use clap::clap_app;

fn main() -> std::io::Result<()> {
    let matches = clap_app!(tapec =>
        (version: "0.1.0")
        (author: "Gabriel Dertoni <gab.dertoni@gmail.com>")
        (about: "A disassembler for the risc_zero architecture")
        (@arg BIN: +required "The risc_zero binary")
        (@arg output: -o --output +takes_value "File to write the disassembler output")
        // (@arg expand: -E --expand "Compile and also print the desugared code")
    ).get_matches();

    let bin_fname = matches.value_of("BIN").unwrap();
    let out = matches.value_of("output").unwrap_or("-");

    let mut bin_file = fs::File::open(bin_fname)?;
    let mut bin = Vec::new();

    bin_file.read_to_end(&mut bin)?;

    let instructions = Instruction::decode_iter(bin);

    let mut out_file: Box<dyn Write> = if out == "-" {
        Box::new(std::io::stdout())
    } else {
        Box::new(fs::File::create(out)?)
    };

    for decoded in instructions {
        match decoded {
            Ok(instruction) => writeln!(out_file, "{}", instruction)?,
            Err(e)          => eprintln!("{}", e),
        }
    }

    Ok(())
}
