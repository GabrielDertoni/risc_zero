use std::collections::VecDeque;
use std::io::{ self, Read, Write };

pub struct Console {
    pub log: Vec<String>,
    pub output: Vec<u8>,
    pub input: VecDeque<u8>,
}

impl Console {
    pub fn new() -> Console {
        Console {
            log:    Vec::new(),
            output: Vec::new(),
            input:  VecDeque::new(),
        }
    }
}

impl Read for Console {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        for (i, el) in buf.iter_mut().enumerate() {
            if let Some(front) = self.input.pop_front() {
                *el = front;
            } else {
                return Ok(i);
            }
        }
        Ok(buf.len())
    }
}


impl Write for Console {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = Write::write(&mut self.output, buf)?;
        if self.output.ends_with(b"\n") {
            self.log.push(String::from_utf8(std::mem::take(&mut self.output)).unwrap())
        }
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        Write::flush(&mut self.output)
    }
}
