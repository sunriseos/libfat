//! Utils used in the crate.

use crate::filesystem::FatFileSystem;
use core::ops::{BitAnd, Not};
use num_traits::Num;
use storage_device::StorageDevice;

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

/// A simple FileSystemIterator wrapper that implement Iterator
pub struct GenericFileSystemIterator<'a, S: StorageDevice, T: FileSystemIterator<S>> {
    /// A reference to the filesystem.
    fs: &'a FatFileSystem<S>,
    /// The filesystem iterator
    inner: T,
}

impl<'a, S: StorageDevice, T: FileSystemIterator<S>> GenericFileSystemIterator<'a, S, T> {
    /// Create a new GenericFileSystemIterator
    pub fn new(fs: &'a FatFileSystem<S>, inner: T) -> Self {
        GenericFileSystemIterator { fs, inner }
    }
}

impl<'a, S: StorageDevice, T: FileSystemIterator<S>> Iterator
    for GenericFileSystemIterator<'a, S, T>
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next(self.fs)
    }
}

/// A simple iterator that take a reference to a FatFileSystem in next.
///
/// Note:
///
/// This permit to cir
pub trait FileSystemIterator<S: StorageDevice>: Sized {
    /// The type of the elements being iterated over.
    type Item;

    /// Advances the iterator and returns the next value.
    fn next(&mut self, filesystem: &FatFileSystem<S>) -> Option<Self::Item>;

    /// Returns the `n`th element of the iterator.
    fn nth(&mut self, filesystem: &FatFileSystem<S>, mut n: usize) -> Option<Self::Item> {
        while let Some(x) = self.next(filesystem) {
            if n == 0 {
                return Some(x);
            }
            n -= 1;
        }
        None
    }

    /// Convert the FileSystemIterator to a regular iterator.
    fn to_iterator<'a>(
        self,
        filesystem: &'a FatFileSystem<S>,
    ) -> GenericFileSystemIterator<'a, S, Self> {
        GenericFileSystemIterator::new(filesystem, self)
    }
}
