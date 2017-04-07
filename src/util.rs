// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use result::{DmResult, DmError};
use types::Bytes;
use std::fs::File;
use std::os::unix::prelude::AsRawFd;

/// send IOCTL via blkgetsize64
ioctl!(read blkgetsize64 with 0x12, 114; u64);

/// TODO document
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
