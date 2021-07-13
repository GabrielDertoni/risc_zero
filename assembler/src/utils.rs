
use std::mem::MaybeUninit;


pub trait IterExt: Iterator
where
    Self: Sized,
    Self::Item: Sized,
{
    fn to_array<const N: usize>(mut self) -> Option<[Self::Item; N]> {
        let mut arr = MaybeUninit::uninit_array();

        for i in 0..N {
            if let Some(val) = self.next() {
                arr[i].write(val);
            } else {
                return None;
            }
        }

        unsafe { Some(MaybeUninit::array_assume_init(arr)) }
    }
}

impl<I: Iterator> IterExt for I {}
