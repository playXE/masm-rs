pub mod code_ptr;
pub mod executable_memory_handle;
pub mod virtual_memory;


use core::ops::*;
use std::mem::size_of;
use num_traits::{FromPrimitive, NumOps, One, ToPrimitive, Zero};
pub fn is_power_of_two<
    T: BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Not<Output = T>
        + PartialEq
        + Copy
        + Zero
        + One
        + Sub<Output = T>,
>(
    x: T,
) -> bool {
    (x & (x - T::one())) == T::zero() && (x != T::zero())
}

pub fn shift_for_power_of_two<
    T: Shr<T, Output = T>
        + Copy
        + Eq
        + One
        + Zero
        + Not<Output = T>
        + BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Sub<Output = T>
        + PartialOrd,
>(
    mut x: T,
) -> i32 {
    assert!(is_power_of_two(x));
    let mut num_shifts = 0;
    while x > T::one() {
        num_shifts += 1;
        x = x >> T::one();
    }
    num_shifts
}

pub fn is_aligned<
    T: BitOr<T, Output = T>
        + BitAnd<T, Output = T>
        + Not<Output = T>
        + PartialEq
        + Copy
        + Zero
        + One
        + Sub<Output = T>
        + ToPrimitive,
>(
    x: T,
    alignment: usize,
    offset: usize,
) -> bool {
    assert!(is_power_of_two(alignment));
    assert!(offset < alignment);
    let x = x.to_isize();
    let x = match x {
        Some(x) => x,
        _ => return false,
    };
    (x & (alignment as isize - 1)) == offset as isize
}

pub fn is_aligned_ptr<
    T,
    U: BitOr<U, Output = U> + BitAnd<U, Output = U> + Not<Output = U> + PartialEq + Copy + Zero + One,
>(
    x: *const T,
    alignment: usize,
    offset: usize,
) -> bool {
    is_aligned(x as usize, alignment, offset)
}

pub fn round_down<
    T: BitOr<T, Output = T> + BitAnd<T, Output = T> + Not<Output = T> + Copy + FromPrimitive + ToPrimitive,
>(
    x: T,
    alignment: isize,
) -> T {
    assert!(is_power_of_two(alignment));
    T::from_isize(x.to_isize().unwrap() & -alignment).unwrap()
}


pub fn round_down_unchecked<
    T: BitOr<T, Output = T> + BitAnd<T, Output = T> + Not<Output = T> + Copy + FromPrimitive + ToPrimitive,
>(
    x: T,
    alignment: isize,
) -> T {
    T::from_isize(x.to_isize().unwrap() & -alignment).unwrap()
}

pub fn round_down_ptr<T, U: BitOr<U, Output = U> + Not<Output = U> + Copy>(
    x: *const T,
    alignment: isize,
) -> *const T {
    round_down(x as usize, alignment) as *const T
}

pub fn round_up<
    T: One
        + NumOps
        + BitOr<T, Output = T>
        + Not<Output = T>
        + Copy
        + BitAnd<T, Output = T>
        + FromPrimitive + ToPrimitive,
>(
    x: T,
    alignment: usize,
    offset: usize,
) -> T {
    assert!(offset < alignment);
    round_down(
        x + T::from_usize(alignment).unwrap() - T::one() + T::from_usize(offset).unwrap(),
        alignment as _,
    ) - T::from_usize(offset).unwrap()
}


pub fn round_up_unchecked<
    T: One
        + NumOps
        + BitOr<T, Output = T>
        + Not<Output = T>
        + Copy
        + BitAnd<T, Output = T>
        + FromPrimitive + ToPrimitive,
>(
    x: T,
    alignment: usize,
    offset: usize,
) -> T {
    round_down_unchecked(
        x + T::from_usize(alignment).unwrap() - T::one() + T::from_usize(offset).unwrap(),
        alignment as _,
    ) - T::from_usize(offset).unwrap()
}

pub fn round_up_ptr<T, U: BitOr<U, Output = U> + Not<Output = U> + Copy>(
    x: *const T,
    alignment: usize,
    offset: usize,
) -> *const T {
    round_up(x as usize, alignment, offset) as *const T
}

pub fn round_up_to_power_of_two(x: usize) -> usize {
    let mut x = x - 1;
    x = x | (x >> 1);
    x = x | (x >> 2);
    x = x | (x >> 4);
    x = x | (x >> 8);
    x = x | (x >> 16);
    #[cfg(target_pointer_width = "64")]
    {
        x = x | (x >> 32);
    }

    x + 1
}

pub fn round_up_usize(x: usize, alignment: usize, offset: usize) -> usize {
    round_up_unchecked(x, alignment, offset)
}

pub const fn log2i_graceful(value: usize) -> isize {
    if value == 0 {
        return -1;
    }

    let bits = size_of::<usize>() * 8;

    bits as isize - value.leading_zeros() as isize - 1
}