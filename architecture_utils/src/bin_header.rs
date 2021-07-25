use std::convert::TryInto;
use std::str;
use std::io::{ Write, Read };

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileHeader {
    pub magic_number: u16,
    pub signature: u32,
    pub data_seg_start: u16,
    pub data_seg_end: u16,
    pub entry_point: u16,
}

const BIN_MAGIC_NUMBER: u16 = 0x5a;
const BIN_SIGNATURE: &'static str = "ZERO";

impl FileHeader {
    pub const SIZE: u64 = 12;

    pub fn new(data_seg_start: u16, data_seg_end: u16, entry_point: u16) -> FileHeader {
        let encoded_string = BIN_SIGNATURE.as_bytes()
            .try_into()
            .unwrap();

        FileHeader {
            magic_number: BIN_MAGIC_NUMBER,
            signature: u32::from_be_bytes(encoded_string),
            data_seg_start,
            data_seg_end,
            entry_point,
        }
    }

    pub fn decode(bin_header: &[u8]) -> Result<FileHeader, String> {
        let vec_slice = &bin_header[0..2];
        let magic_number = u16::from_be_bytes(vec_slice.try_into().unwrap());
        if magic_number != BIN_MAGIC_NUMBER {
            return Err(format!("Unexpected magic number: {}", magic_number));
        }

        let vec_slice = &bin_header[2..6];
        let signature = u32::from_be_bytes(vec_slice.try_into().unwrap());
        if str::from_utf8(vec_slice).expect("Invalid UTF-8 signature.") != BIN_SIGNATURE {
            return Err(format!("Unexpected sign value: {}", signature));
        }

        let vec_slice = &bin_header[6..8];
        let data_seg_start = u16::from_be_bytes(vec_slice.try_into().unwrap());

        let vec_slice = &bin_header[8..10];
        let data_seg_end = u16::from_be_bytes(vec_slice.try_into().unwrap());

        let vec_slice = &bin_header[10..12];
        let entry_point = u16::from_be_bytes(vec_slice.try_into().unwrap());

        Ok(FileHeader {
            magic_number,
            signature,
            data_seg_start,
            data_seg_end,
            entry_point,
        })
    }

    pub fn encode(&self) -> [u8; FileHeader::SIZE as usize] {
        let mut encoded_list: [u8; FileHeader::SIZE as usize] = Default::default();
        encoded_list[0..2].copy_from_slice(&self.magic_number.to_be_bytes());
        encoded_list[2..6].copy_from_slice(&self.signature.to_be_bytes());
        encoded_list[6..8].copy_from_slice(&self.data_seg_start.to_be_bytes());
        encoded_list[8..10].copy_from_slice(&self.data_seg_end.to_be_bytes());
        encoded_list[10..12].copy_from_slice(&self.entry_point.to_be_bytes());
        
        encoded_list
    }

    pub fn read_from<R: Read>(mut reader: R) -> Result<FileHeader, String> {
        let mut buf = [0; FileHeader::SIZE as usize];
        reader.read_exact(&mut buf).map_err(|e| e.to_string())?;
        FileHeader::decode(&buf)
    }

    pub fn write_to<W: Write>(&self, mut writer: W) -> std::io::Result<usize> {
        writer.write(&self.magic_number.to_be_bytes())?;
        writer.write(&self.signature.to_be_bytes())?;
        writer.write(&self.data_seg_start.to_be_bytes())?;
        writer.write(&self.data_seg_end.to_be_bytes())?;
        writer.write(&self.entry_point.to_be_bytes())
    }
}
