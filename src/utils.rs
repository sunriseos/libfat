use core::ops::{BitAnd, Not};
use num_traits::Num;

pub fn align_up<T: Num + Not<Output = T> + BitAnd<Output = T> + Copy>(addr: T, align: T) -> T {
    align_down(addr + (align - T::one()), align)
}

pub fn align_down<T: Num + Not<Output = T> + BitAnd<Output = T> + Copy>(addr: T, align: T) -> T {
    addr & !(align - T::one())
}

pub fn get_parent(path: &str) -> (&str, &str) {
    let separator_index_opt = path.rfind('/');

    if let Some(separator_index) = separator_index_opt {
        let (first, last) = path.split_at(separator_index);
        (first, &last[1..])
    } else {
        ("", path)
    }
}

pub fn split_path(path: &str) -> (&str, Option<&str>) {
    let mut path_split = path.trim_matches('/').splitn(2, '/');

    // unwrap will never fail here
    let comp = path_split.next().unwrap();
    let rest_opt = path_split.next();

    (comp, rest_opt)
}
