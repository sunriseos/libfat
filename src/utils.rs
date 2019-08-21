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
///
/// Returns a tuple of the parts before and after the cut.
///
/// If the path doesn't have parent, return ("", path).
pub fn get_parent(path: &str) -> (&str, &str) {
    // First we need to trim until we find a '/'
    let mut start_index = 0;

    for (index, entry) in path.chars().enumerate() {
        if entry != '/' {
            if index > 0 {
                start_index = index - 1;
            }
            break;
        }
    }

    // make the &str a bit more sane.
    let path = (&path[start_index..]).trim_end_matches('/');
    let separator_index_opt = path.rfind('/');

    if let Some(separator_index) = separator_index_opt {
        let (first, last) = path.split_at(separator_index);
        (first.trim_end_matches('/'), &last[1..])
    } else {
        ("", path)
    }
}

/// Splits a path at the first `/` it encounters.
///
/// Returns a tuple of the parts before and after the cut.
///
/// # Notes:
/// - The rest part can contain duplicates '/' in the middle of the path. This should be fine as you should call split_path to parse the rest part.
///
/// ```ignore
/// use libfat::utils::split_path;
/// let my_path = "/top_level_dir/middle_dir/sub_dir";
/// let (before, after) = split_path(my_path);
/// assert!(before == "/top_level_dir");
/// assert!(after.unwrap() == "middle_dir/sub_dir");
/// ```
///
/// If no `/` is encountered, the "after" part is None:
///
/// ```ignore
/// use libfat::utils::split_path;
/// let my_path = "/top_level_dir";
/// let (before, after) = split_path(my_path);
/// assert!(before == "/top_level_dir");
/// assert!(after.is_none());
/// ```
pub fn split_path(path: &str) -> (&str, Option<&str>) {
    let mut path_split = path.trim_matches('/').splitn(2, '/');

    // unwrap will never fail here
    let comp = path_split.next().unwrap();
    let rest_opt = path_split.next().and_then(|x| Some(x.trim_matches('/')));

    (comp, rest_opt)
}

/// A simple FileSystemIterator wrapper that implement Iterator.
///
/// This permit to expose all Iterator methods not availaible in FileSystemIterator.
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
/// This permit to avoid consuming self in trivial cases.
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

#[cfg(test)]
mod tests {
    use super::get_parent;
    use super::split_path;
    #[test]
    fn test_get_parent() {
        assert_eq!(
            get_parent("/my_root_dir/my_other_dir/my_child_dir"),
            ("/my_root_dir/my_other_dir", "my_child_dir")
        );

        assert_eq!(
            get_parent("my_root_dir/my_other_dir/my_child_file"),
            ("my_root_dir/my_other_dir", "my_child_file")
        );

        assert_eq!(get_parent("/"), ("", ""));
        assert_eq!(get_parent("/."), ("", "."));
        assert_eq!(get_parent("/.."), ("", ".."));

        assert_eq!(get_parent(""), ("", ""));
        assert_eq!(get_parent("."), ("", "."));
        assert_eq!(get_parent(".."), ("", ".."));

        assert_eq!(get_parent("/ect/lol"), ("/ect", "lol"));
        assert_eq!(get_parent("/ect/lol/"), ("/ect", "lol"));
        assert_eq!(get_parent("////////////ect///////lol"), ("/ect", "lol"));
        assert_eq!(get_parent("/ect/lol//////test"), ("/ect/lol", "test"));
        assert_eq!(get_parent("/ect"), ("", "ect"));
        assert_eq!(get_parent("/ect/"), ("", "ect"));
        assert_eq!(get_parent("/ect//"), ("", "ect"));
        assert_eq!(get_parent("//ect//"), ("", "ect"));
        assert_eq!(get_parent("////////////ect"), ("", "ect"));
        assert_eq!(get_parent("////////////ect/"), ("", "ect"));
        assert_eq!(get_parent("///////lol"), ("", "lol"));
    }

    #[test]
    fn test_split_paths() {
        assert_eq!(split_path("/"), ("", None));
        assert_eq!(split_path("/ect"), ("ect", None));
        assert_eq!(split_path("/ect/"), ("ect", None));
        assert_eq!(split_path("/ect//"), ("ect", None));
        assert_eq!(split_path("//ect//"), ("ect", None));
        assert_eq!(split_path("////////////ect"), ("ect", None));
        assert_eq!(split_path("////////////ect/"), ("ect", None));
        assert_eq!(split_path("/ect/lol/"), ("ect", Some("lol")));
        assert_eq!(split_path("///////lol"), ("lol", None));
        assert_eq!(
            split_path("////////////ect///////lol"),
            ("ect", Some("lol"))
        );

        assert_eq!(
            split_path("////////////ect///////lol/////test"),
            // This is fine as we are still under another directory and the result of the next call is going to be trimmed
            ("ect", Some("lol/////test"))
        );
        assert_eq!(
            split_path("////////////ect///////lol/"),
            ("ect", Some("lol"))
        );
    }
}
