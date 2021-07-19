
pub mod instruction;
pub mod reg;

pub use crate::instruction::*;
pub use crate::reg::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
