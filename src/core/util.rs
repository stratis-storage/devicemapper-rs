// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{
    io::{Cursor, Write},
    mem::size_of,
    slice, str,
};

use crate::result::DmResult;

/// The smallest number divisible by `align_to` and at least `num`.
/// Precondition: `align_to` is a power of 2.
/// Precondition: `num` + `align_to` < usize::MAX + 1.
#[inline]
pub fn align_to(num: usize, align_to: usize) -> usize {
    let agn = align_to - 1;

    (num + agn) & !agn
}

/// Convert from a &[i8] to a &[u8].
pub fn byte_slice_from_c_str(c_str: &[i8]) -> &[u8] {
    unsafe { slice::from_raw_parts(c_str as *const _ as *const u8, c_str.len()) }
}

/// Return a String parsed from the C string up to the first \0, or None
pub fn str_from_c_str(slc: &[i8]) -> Option<&str> {
    let slc = byte_slice_from_c_str(slc);
    str_from_byte_slice(slc)
}

/// Return a String parsed from the byte slice up to the first \0, or None
pub fn str_from_byte_slice(slc: &[u8]) -> Option<&str> {
    slc.iter()
        .position(|c| *c == b'\0')
        .and_then(|i| str::from_utf8(&slc[..i]).ok())
}

/// Return a mutable slice from the mutable C string provided as input
pub fn mut_slice_from_c_str(c_str: &mut [i8]) -> &mut [u8] {
    unsafe { slice::from_raw_parts_mut(c_str as *mut _ as *mut u8, c_str.len()) }
}

/// Convert the C struct into a properly-sized byte slice
fn slice_from_c_struct<T>(strct: &T) -> &[u8] {
    unsafe { slice::from_raw_parts(strct as *const _ as *const u8, size_of::<T>()) }
}

/// Serialize a sequence of C structs into a byte vector
pub fn serialize<T>(
    cur: &mut Cursor<Vec<u8>>,
    strct: &T,
    alignment: Option<usize>,
) -> DmResult<()> {
    cur.write_all(slice_from_c_struct(strct))?;
    if let Some(a) = alignment {
        cur.set_position(align_to(cur.position() as usize, a) as u64);
    }

    Ok(())
}
