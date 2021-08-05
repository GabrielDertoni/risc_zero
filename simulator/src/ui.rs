use tui::style::*;
use tui::text::{ Spans, Text };
use tui::widgets::{ Block, BorderType, Borders, List, ListItem, Paragraph, Row, Table };
use tui::layout::{ Alignment, Constraint, Direction, Layout, Rect };
use tui::backend::Backend;
use tui::Frame;
use architecture_utils::instruction::Instruction;
use architecture_utils::bin_header::FileHeader;
use crate::reg::Reg;
use crate::cpu_state::{ CPUState, TEXT_START, SCREEN_HEIGHT, SCREEN_WIDTH };
use crate::io_device::Console;

pub fn draw<B: Backend>(frame: &mut Frame<B>, curr_state: &CPUState, io_device: &Console) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints(
            [
                Constraint::Length(6),
                Constraint::Min(10),
                Constraint::Length(3),
            ]
        )
        .split(frame.size());
    draw_reg_block(frame, chunks[0], curr_state);
    draw_lower_block(frame, chunks[1], curr_state, io_device);
    draw_how_to_block(frame, chunks[2]);
}

fn draw_reg_block<B>(frame: &mut Frame<B>, area: Rect, curr_state: &CPUState) 
where 
    B: Backend,
{
    let reg_table = Table::new([
        Row::new(
            [
                format!(" {}\t: {:#06x}", Reg::ADR, curr_state.reg_bank[Reg::ADR]),
                format!("{}\t: {}", Reg::ACC, curr_state.reg_bank[Reg::ACC]),
                format!("{}\t: {1:} ({1:#06x})", Reg::TMP, curr_state.reg_bank[Reg::TMP]),
                format!("{}\t: {:#07b}", Reg::FL, curr_state.reg_bank[Reg::FL]),
            ]
        ),
        Row::new(
            [
                format!(" {}\t: {}", Reg::SP, (curr_state.reg_bank[Reg::SP] as u16)),
                format!("{}\t: {}", Reg::HI, curr_state.reg_bank[Reg::HI]),
                format!("{}\t: {}", Reg::LO, curr_state.reg_bank[Reg::LO]),
                format!("{}\t: {}", Reg::R1, curr_state.reg_bank[Reg::R1]),
            ]
        ),
        Row::new(
            [
                format!(" {}\t: {}", Reg::R2, curr_state.reg_bank[Reg::R2]),
                format!("{}\t: {}", Reg::R3, curr_state.reg_bank[Reg::R3]),
                format!("{}\t: {}", Reg::R4, curr_state.reg_bank[Reg::R4]),
                format!("{}\t: {}", Reg::R5, curr_state.reg_bank[Reg::R5]),
            ]
        ),
        Row::new(
            [
                format!(" {}\t: {}", Reg::R6, curr_state.reg_bank[Reg::R6]),
                format!("{}\t: {}", Reg::R7, curr_state.reg_bank[Reg::R7]),
                format!("{}\t: {}", Reg::R8, curr_state.reg_bank[Reg::R8]),
                format!("{}\t: {}", Reg::R9, curr_state.reg_bank[Reg::R9]),
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

fn itemize_instructions(curr_state: &CPUState) -> Vec<ListItem> {
    let text_len = curr_state.header.data_seg_start as usize - FileHeader::SIZE as usize;
    let text_segment_slice = &curr_state.memory[TEXT_START..TEXT_START + text_len];
    let decoded_slice: Vec<Instruction> = Instruction::decode_slice(text_segment_slice).unwrap();

    let mut listitem_vec: Vec<ListItem> = Vec::new();
    for (i, instruction) in decoded_slice[(curr_state.pc - TEXT_START)/2..].iter().enumerate() {
        if i == 0 {
            let style = Style::default()
                .fg(Color::Black)
                .bg(Color::White)
                .add_modifier(Modifier::BOLD);

            let text = Text::styled(format!("{:#06x}: {}", curr_state.pc + i * 2, instruction), style);
            listitem_vec.push(ListItem::new(text));
        } else {
            let text = format!("{:#06x}: {}", curr_state.pc + i * 2, instruction);
            listitem_vec.push(ListItem::new(text));
        }
    }
    listitem_vec
}

// meio gambiarra, preciso arrumar depois
// resolução 1:3
fn itemize_emulation(curr_state: &CPUState) -> Vec<Spans> {
    let mut emulation_lines: Vec<Spans> = Vec::new();
    emulation_lines.push(Spans::from("\n"));
    for i in 0..SCREEN_HEIGHT {
        let first_line_byte = i * SCREEN_WIDTH;
        emulation_lines.push(Spans::from(
                String::from_utf8_lossy(&curr_state.memory[first_line_byte..first_line_byte + SCREEN_WIDTH]).to_string()
        ));
    }
    emulation_lines
}

fn draw_lower_block<B>(frame: &mut Frame<B>, area: Rect, curr_state: &CPUState, io_device: &Console) 
where 
    B: Backend,
{
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .margin(0)
        .constraints (
            [
                Constraint::Percentage(15),
                Constraint::Percentage(20),
                Constraint::Percentage(65),
            ]
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
    frame.render_widget(emulation, chunks[2]);

    let output_string = String::from_utf8(io_device.output.clone()).unwrap();
    let console_vec: Vec<_> = io_device.log.iter()
        .map(|line| ListItem::new(line.as_str()))
        .chain(std::iter::once(ListItem::new(output_string)))
        .rev()
        .collect();

    let console = List::new(console_vec)
        .block(Block::default().title("Console").borders(Borders::ALL).border_type(BorderType::Rounded))
        .style(Style::default().fg(Color::White));
    frame.render_widget(console, chunks[1]);
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
