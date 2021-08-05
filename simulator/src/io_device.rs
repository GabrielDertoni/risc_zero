use std::io::{ self, Read, Write };

pub struct IODevice {
    pub log: Vec<String>,
    pub output: Vec<u8>,
    pub input: Vec<u8>,
}

impl IODevice {
    pub fn new() -> IODevice {
        IODevice {
            log: Vec::new(),
            output: Vec::new(),
            input: Vec::new(),
        }
    }
}


impl Read for IODevice {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        Read::read(&mut self.input.as_slice(), buf)
    }
}

impl Write for IODevice {
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
