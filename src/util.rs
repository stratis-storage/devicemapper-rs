// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![allow(missing_docs)]

use result::{DmResult, DmError};
use types::Bytes;
use std::fs::File;
use std::os::unix::prelude::AsRawFd;

/// send IOCTL via blkgetsize64
ioctl!(read blkgetsize64 with 0x12, 114; u64);

/// The smallest number divisible by `align_to` and at least `num`.
/// Precondition: `align_to` is a power of 2.
/// Precondition: `num` + `align_to` < usize::MAX + 1.
pub fn align_to(num: usize, align_to: usize) -> usize {
    let agn = align_to - 1;

    (num + agn) & !agn
}

/// get the size of a given block device file
pub fn blkdev_size(file: &File) -> DmResult<Bytes> {
    let mut val: u64 = 0;

    match unsafe { blkgetsize64(file.as_raw_fd(), &mut val) } {
        Err(x) => Err(DmError::Nix(x)),
        Ok(_) => Ok(Bytes(val)),
    }
}

/// Return slc up to the first \0, or None
pub fn slice_to_null(slc: &[u8]) -> Option<&[u8]> {
    slc.iter().position(|c| *c == b'\0').map(|i| &slc[..i])
}
