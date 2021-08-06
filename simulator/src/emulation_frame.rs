use tui::{buffer::Buffer, layout::Rect, style::Style, widgets::{Block, Widget}};
use crate::cpu_state::{ SCREEN_WIDTH, SCREEN_HEIGHT };

pub struct EmulationFrame<'a> {
    symb_block: Vec<String>,
    block: Option<Block<'a>>,
}

impl<'a> Default for EmulationFrame<'a> {
    fn default() -> EmulationFrame<'a> {
        EmulationFrame {
            symb_block: Vec::new(),
            block: None
        }
    }
}

impl<'a> EmulationFrame<'a> {
    pub fn set_content(mut self, content: Vec<String>) -> EmulationFrame<'a> {
        self.symb_block = content;
        self
    }

    pub fn block(mut self, block: Block<'a>) -> EmulationFrame<'a> {
        self.block = Some(block);
        self
    }
}

impl<'a> Widget for EmulationFrame<'a> {
    fn render(mut self, area: Rect, buf: &mut Buffer) {
        let text_area = match self.block.take() {
            Some(block) => {
                let inner_area = block.inner(area);
                block.render(area, buf);
                inner_area
            }
            None => area,
        };

        let top_border: String = format!("╭{}╮", std::iter::repeat("─").take(60).collect::<String>());
        let low_border: String = format!("╰{}╯", std::iter::repeat("─").take(60).collect::<String>());
        buf.set_string((text_area.left() + text_area.right() - SCREEN_WIDTH as u16)/2, 
                       (text_area.top() + text_area.bottom() - SCREEN_HEIGHT as u16)/2 - 1, 
                       top_border , Style::default());
        buf.set_string((text_area.left() + text_area.right() - SCREEN_WIDTH as u16)/2, 
                       (text_area.top() + text_area.bottom() + SCREEN_HEIGHT as u16)/2, 
                       low_border , Style::default());
        for (i, string) in self.symb_block.iter().enumerate() {
            buf.set_string((text_area.left() + text_area.right() - SCREEN_WIDTH as u16)/2, 
                           (text_area.top() + text_area.bottom() - SCREEN_HEIGHT as u16)/2 + i as u16,
                           format!("│{}│", string), Style::default())
        }
    }
}
