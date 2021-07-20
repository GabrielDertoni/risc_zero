pub struct FileHeader {
    magic_number: u16,
    signature: String,
    data_seg: u16,
}

const BIN_MAGIC_NUMBER: u16 = 0x5a;
const BIN_SIGNATURE: &'static str = "ZERO";

impl FileHeader {
    pub fn new(curr_data_seg: u16) -> Self {
        Self {
            magic_number = BIN_MAGIC_NUMBER,
            signature = BIN_SIGNATURE,
            data_set = curr_data_seg,
        }
    }

    pub fn decode(bin_header: u64) -> FileHeader {
        let magic_number = bin_header.as_be_bytes()[0..2]
            .from_be_bytes();
        let signature = String::from_bytes(
            bin_header.as_be_bytes()[2..6]);
        let data_seg = bin_header.as_be_bytes()[6..8]
            .from_be_bytes();

        FileHeader {
            magic_number,
            signature,
            data_seg,
        };
    }

    pub fn encode(&self) -> u64 {
        let encoded_list: [Vec<u8>; 3] = [
            self.magic_number.as_be_bytes(),
            self.signature.as_bytes(),
            self.data_seg.as_be_bytes(),
        ];

        encoded_list.iter()
            .flat_map(|s| s.iter())
            .collect()
            .from_be_bytes()
    }
}
