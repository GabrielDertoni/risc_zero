#![allow(dead_code)]

mod reg_bank;
mod event;
mod os;
mod ui;
mod cpu_state;
mod io_device;
mod emulation_frame;

// Architecture related stuff
use architecture_utils::*;

// External imports
use clap::clap_app;
use std::{error::Error, io, time::Duration};
use termion::input::MouseTerminal;
use termion::raw::IntoRawMode;
use termion::event::Key;
use tui::Terminal;
use tui::backend::TermionBackend;

// Local
use ui::draw_home;
use io_device::Console;
use event::{Config, Events, Event};
use cpu_state::CPUState;

fn main() -> Result<(), Box<dyn Error>> {
    let matches = clap_app!(risc_zero =>
        (version: "0.1.0")
        (author: "Natan Sanches <natanhsanches@gmail.com>")
        (about: "Main simulator for risc_zero architecture")
        (@arg BIN: +required "A valid risc_zero binary")
        (@arg tick_rate: -r --tick_rate +takes_value "Tick rate")
    ).get_matches();

    let tick_rate: u64 = matches.value_of("tick_rate")
        .and_then(|val| val.parse().ok())
        .unwrap_or(250);

    // Load ZERO binary file
    let bin_filename = matches.value_of("BIN").unwrap();

    // Build simulator from header
    let mut curr_state = CPUState::new(bin_filename).unwrap();

    let mut console = Console::new();

    // Set up application's terminal
    let stdout = io::stdout().into_raw_mode()?;
    let stdout = MouseTerminal::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let events = Events::with_config(Config {
        tick_rate: Duration::from_millis(tick_rate),
        ..Config::default()
    });

    let mut tick_count = 0;

    // Executing instructions
    let mut continuous_execution = false;
    'outer: loop {
        match events.next()? {
            Event::Input(key) => match key {
                Key::Up => continuous_execution = !continuous_execution,
                Key::Right if !continuous_execution => {
                    if !curr_state.simulate(&mut console)? {
                        break;
                    }
                }

                Key::Char(chr) => curr_state.write_keyboard_input(chr as u8),
                Key::Backspace => curr_state.write_keyboard_input(0x08),
                Key::Delete    => curr_state.write_keyboard_input(0x7f),

                Key::Esc | Key::Down => break,
                _                    => (),
            },
            Event::Tick => {
                if continuous_execution {
                    for _ in 0..8 {
                        if !curr_state.simulate(&mut console)? {
                            break 'outer;
                        }
                    }
                }

                if tick_count % 11 == 0 {
                    terminal.draw(|f| draw_home(f, &curr_state, &mut console))?;
                }

                tick_count += 1;
            }
        }
    }
    terminal.clear()?;

    Ok(())
}
