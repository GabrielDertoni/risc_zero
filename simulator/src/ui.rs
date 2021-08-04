use std::fmt::Write;

use tui::style::*;
use tui::text::Spans;
use tui::widgets::{Block, BorderType, Borders, List, ListItem, Paragraph, Row, Table};
use tui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use tui::backend::Backend;
use crate::reg::Reg;
use crate::cpu_state::CPUState;
use architecture_utils::instruction::Instruction;
use architecture_utils::bin_header::FileHeader;
use tui::Frame;
use std::str;

pub fn draw<B: Backend>(frame: &mut Frame<B>, curr_state: &CPUState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints (
            [
                Constraint::Length(6),
                Constraint::Min(10),
                Constraint::Length(3),
            ].as_ref()
        )
        .split(frame.size());
    draw_reg_block(frame, chunks[0], curr_state);
    draw_lower_block(frame, chunks[1], curr_state);
    draw_how_to_block(frame, chunks[2]);
}

fn draw_reg_block<B>(frame: &mut Frame<B>, area: Rect, curr_state: &CPUState) 
where 
    B: Backend,
{
    let reg_table = Table::new([
        Row::new(
            [
                format!(" {}\t: {}", Reg::ADR.to_string(), curr_state.reg_bank[Reg::ADR].to_string()),
                format!("{}\t: {}", Reg::ACC.to_string(), curr_state.reg_bank[Reg::ACC].to_string()),
                format!("$pc\t: {}", curr_state.pc),
                format!("{}\t: {}", Reg::FL.to_string(), curr_state.reg_bank[Reg::FL].to_string()),
            ]
        ),
        Row::new(
            [
                format!(" {}\t: {}", Reg::SP.to_string(), (curr_state.reg_bank[Reg::SP] as u16).to_string()),
                format!("{}\t: {}", Reg::HI.to_string(), curr_state.reg_bank[Reg::HI].to_string()),
                format!("{}\t: {}", Reg::LO.to_string(), curr_state.reg_bank[Reg::LO].to_string()),
                format!("{}\t: {}", Reg::R1.to_string(), curr_state.reg_bank[Reg::R1].to_string()),
            ]
        ),
        Row::new(
            [
                format!(" {}\t: {}", Reg::R2.to_string(), curr_state.reg_bank[Reg::R2].to_string()),
                format!("{}\t: {}", Reg::R3.to_string(), curr_state.reg_bank[Reg::R3].to_string()),
                format!("{}\t: {}", Reg::R4.to_string(), curr_state.reg_bank[Reg::R4].to_string()),
                format!("{}\t: {}", Reg::R5.to_string(), curr_state.reg_bank[Reg::R5].to_string()),
            ]
        ),
        Row::new(
            [
                format!(" {}\t: {}", Reg::R6.to_string(), curr_state.reg_bank[Reg::R6].to_string()),
                format!("{}\t: {}", Reg::R7.to_string(), curr_state.reg_bank[Reg::R7].to_string()),
                format!("{}\t: {}", Reg::R8.to_string(), curr_state.reg_bank[Reg::R8].to_string()),
                format!("{}\t: {}", Reg::R9.to_string(), curr_state.reg_bank[Reg::R9].to_string()),
            ]
        ),
    ])
    .block(Block::default().title("Registers").borders(Borders::ALL).border_type(BorderType::Rounded))
    .column_spacing(1)
    .widths(
        [
            Constraint::Percentage(25), 
            Constraint::Percentage(25), 
            Constraint::Percentage(25), 
            Constraint::Percentage(25),
        ].as_ref()
    )
    .style(Style::default().fg(Color::White));
    frame.render_widget(reg_table, area);
}

fn itemize_instructions<'a>(curr_state: &CPUState) -> Vec<ListItem<'a>> {
    let text_segment_slice = &curr_state.memory[..curr_state.header.data_seg_start as usize - FileHeader::SIZE as usize];
    let decoded_slice: Vec<Instruction> = Instruction::decode_slice(text_segment_slice).unwrap();

    let mut listitem_vec: Vec<ListItem> = Vec::new();
    let mut counter = 0;
    for instruction in decoded_slice {
        let mut final_string = String::new();
        write!(final_string, " {:#06x}: {}", counter, instruction).unwrap();
        listitem_vec.push(ListItem::new(final_string));
        counter += 2;
    }
    listitem_vec
}

// meio gambiarra, preciso arrumar depois
// resolução 1:3
fn itemize_emulation<'a>(curr_state: &'a CPUState) -> Vec<Spans<'a>> {
    let mut emulation_lines: Vec<Spans> = Vec::new();
    let mut first_line_byte: usize = (curr_state.header.data_seg_end as usize - FileHeader::SIZE as usize) + 1;
    emulation_lines.push(Spans::from("\n"));
    for _ in 0..20 {
        emulation_lines.push(Spans::from(
                str::from_utf8(&curr_state.memory[first_line_byte..first_line_byte+60]).unwrap()
        ));
        first_line_byte += 60;
    }
    emulation_lines
}

fn draw_lower_block<B>(frame: &mut Frame<B>, area: Rect, curr_state: &CPUState) 
where 
    B: Backend,
{
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .margin(0)
        .constraints (
            [
                Constraint::Percentage(25),
                Constraint::Percentage(75),
            ].as_ref()
        )
        .split(area);

    let instruction_vec = itemize_instructions(curr_state);
    let instruction_list = List::new(instruction_vec)
        .block(Block::default().title("Instructions").borders(Borders::ALL).border_type(BorderType::Rounded))
        .style(Style::default().fg(Color::White))
        .highlight_style(Style::default().add_modifier(Modifier::ITALIC))
        .highlight_symbol(">>");
    frame.render_widget(instruction_list, chunks[0]);

    let emulation_vec = itemize_emulation(curr_state);
    let emulation = Paragraph::new(emulation_vec)
        .block(Block::default().title("Emulation").borders(Borders::ALL).border_type(BorderType::Rounded))
        .style(Style::default().fg(Color::Gray))
        .alignment(Alignment::Center);
    frame.render_widget(emulation, chunks[1]);
}

fn draw_how_to_block<B>(frame: &mut Frame<B>, area: Rect)
where
    B: Backend
{
    let skip_state_msg = " <Right arrow>: Next state";
    let automatic_skip_msg = "<Up arrow>: Automatic state skip";
    let close_msg = "<Down arrow>: Quit";

    let how_to = Table::new([
        Row::new([skip_state_msg, automatic_skip_msg, close_msg]),
    ])
    .block(Block::default().title("Guide").borders(Borders::ALL).border_type(BorderType::Rounded))
    .column_spacing(1)
    .widths(
        [
            Constraint::Percentage(33), 
            Constraint::Percentage(33), 
            Constraint::Percentage(33), 
        ].as_ref()
    )
    .style(Style::default().fg(Color::White));
    frame.render_widget(how_to, area);
}
