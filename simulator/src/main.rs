#![allow(dead_code)]

mod reg_bank;
mod event;
mod os;
mod ui;
mod cpu_state;

// Architecture related stuff
use architecture_utils::*;
use cpu_state::CPUState;

// General stuff
use clap::clap_app;
use std::{error::Error, io, time::Duration};

// UI related stuff
use ui::draw;
use termion::input::MouseTerminal;
use termion::raw::IntoRawMode;
use termion::event::Key;
use tui::Terminal;
use tui::backend::TermionBackend;
use crate::event::{Config, Events, Event};

const TICK_RATE: u64 = 500;

fn main() -> Result<(), Box<dyn Error>> {
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

    let events = Events::with_config(Config {
        tick_rate: Duration::from_millis(TICK_RATE),
        ..Config::default()
    });

    // Load ZERO binary file
    let bin_filename = matches.value_of("BIN").unwrap();

    // Build simulator from header
    let mut curr_state = CPUState::new(bin_filename).unwrap();
    let number: usize = curr_state.header.data_seg_end as usize - FileHeader::SIZE as usize + 1;
    curr_state.memory[number..number+1200].fill('.' as u8);

    // Executing instructions
    let mut continuous_execution = false;
    loop {
        terminal.draw(|f| draw(f, &curr_state))?;
        match events.next()? {
            Event::Input(key) => match key {
                Key::Up => {
                    continuous_execution = true;
                    break;
                }
                Key::Right => {
                    if curr_state.next().is_none() {
                        break;
                    }
                }
                Key::Down => {
                    break;
                }
                _ => {}
            },
            Event::Tick => {}
        }
    }

    if continuous_execution {
        loop {
            terminal.draw(|f| draw(f, &curr_state))?;
            match events.next()? {
                Event::Input(key) => match key {
                    Key::Down => {
                        break;
                    }
                    _ => {}
                }
                Event::Tick => {
                    if curr_state.next().is_none() {
                        break;
                    }
                }
            }
        }
    }
    println!();

    Ok(())
}
