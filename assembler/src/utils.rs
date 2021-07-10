
use std::mem::MaybeUninit;


pub trait IterExt: Iterator
where
    Self: Sized,
    Self::Item: Sized,
{
    fn take_collect_n<const N: usize>(self) -> Option<[Self::Item; N]> {
        unsafe {
            let arr: [Self::Item; N] = MaybeUninit::uninit().assume_init();

            for i in 0..N {
                if let Some(val) = self.next() {
                    arr[i] = val;
                } else {
                    return None;
                }
            }

            Some(arr)
        }
    }

    fn take_collect_some<const N: usize>(self) -> [Option<Self::Item>; N] {
        unsafe {
            let arr: [Option<Self::Item>; N] = MaybeUninit::uninit().assume_init();
            
            let mut fused = self.fuse();

            for i in 0..N {
                arr[i] = fused.next();
            }
            arr
        }
    }
}

impl<I: Iterator> IterExt for I {}
