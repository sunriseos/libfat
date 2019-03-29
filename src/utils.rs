use core::ops::{BitAnd, Not};
use num_traits::Num;

/// Align the address to the next alignment.
///
/// The given number should be a power of two to get coherent results!
///
/// # Panics
///
/// Panics on underflow if align is 0.
/// Panics on overflow if the expression `addr + (align - 1)` overflows.
pub fn align_up<T: Num + Not<Output = T> + BitAnd<Output = T> + Copy>(addr: T, align: T) -> T {
    align_down(addr + (align - T::one()), align)
}

/// Align the address to the previous alignment.
///
/// The given number should be a power of two to get coherent results!
///
/// # Panics
///
/// Panics on underflow if align is 0.
pub fn align_down<T: Num + Not<Output = T> + BitAnd<Output = T> + Copy>(addr: T, align: T) -> T {
    addr & !(align - T::one())
}

/// Retrieve the parent of a given path.
pub fn get_parent(path: &str) -> (&str, &str) {
    let separator_index_opt = path.rfind('/');

    if let Some(separator_index) = separator_index_opt {
        let (first, last) = path.split_at(separator_index);
        (first, &last[1..])
    } else {
        ("", path)
    }
}

/// Permite to split a path.
pub fn split_path(path: &str) -> (&str, Option<&str>) {
    let mut path_split = path.trim_matches('/').splitn(2, '/');

    // unwrap will never fail here
    let comp = path_split.next().unwrap();
    let rest_opt = path_split.next();

    (comp, rest_opt)
}
