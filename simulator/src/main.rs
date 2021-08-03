#![allow(dead_code)]

mod reg_bank;
mod os;
mod ui;
mod cpu_state;

use architecture_utils::*;
use cpu_state::CPUState;
use ui::draw;
use termion::input::MouseTerminal;
use termion::raw::IntoRawMode;
use clap::clap_app;

use core::time;
use std::{io, thread};
use tui::Terminal;
use tui::backend::TermionBackend;

const TICK_RATE: u64 = 500;

fn main() -> std::io::Result<()> {
    let matches = clap_app!(risc_zero =>
        (version: "0.1.0")
        (author: "Natan Sanches <natanhsanches@gmail.com>")
        (about: "Main simulator for risc_zero architecture")
        (@arg BIN: +required "A valid risc_zero binary")
    ).get_matches();

    // Set up application's terminal
    let stdout = io::stdout().into_raw_mode()?;
    let stdout = MouseTerminal::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    // Load ZERO binary file
    let bin_filename = matches.value_of("BIN").unwrap();

    // Build simulator from header
    let mut curr_state = CPUState::new(bin_filename).unwrap();
    let number: usize = curr_state.header.data_seg_end as usize - FileHeader::SIZE as usize + 1;
    curr_state.memory[number..number+4800].fill('#' as u8);

    // Executing instructions
    loop {
        terminal.draw(|f| draw(f, &curr_state))?;
        thread::sleep(time::Duration::from_millis(TICK_RATE));
        if curr_state.next().is_none() {
            break;
        }
    }
    println!();

    Ok(())
}
