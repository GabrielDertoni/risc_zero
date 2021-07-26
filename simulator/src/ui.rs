use tui::widgets::{Block, BorderType, Borders};
use tui::layout::{Layout, Constraint, Direction, Rect};
use tui::backend::Backend;
use tui::Frame;

pub fn draw<B: Backend>(frame: &mut Frame<B>) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints (
            [
                Constraint::Percentage(20),
                Constraint::Percentage(80),
            ].as_ref()
        )
        .split(frame.size());
    draw_reg_block(frame, chunks[0]);
    draw_lower_block(frame, chunks[1]);
}

fn draw_reg_block<B>(frame: &mut Frame<B>, area: Rect) 
where 
    B: Backend,
{
    let block = Block::default()
        .title("Registers")
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded);
    frame.render_widget(block, area);
}

fn draw_lower_block<B>(frame: &mut Frame<B>, area: Rect) 
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
    let block = Block::default()
        .title("Instructions")
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded);
    frame.render_widget(block, chunks[0]);
    let block = Block::default()
        .title("Emulation")
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded);
    frame.render_widget(block, chunks[1]);
}
