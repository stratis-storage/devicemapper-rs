// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![allow(missing_docs)]

use super::errors::{Error, Result};

/// The smallest number divisible by `align_to` and at least `num`.
/// Precondition: `align_to` is a power of 2.
/// Precondition: `num` + `align_to` < usize::MAX + 1.
pub fn align_to(num: usize, align_to: usize) -> usize {
    let agn = align_to - 1;

    (num + agn) & !agn
}

/// Return slc up to the first \0, or None
pub fn slice_to_null(slc: &[u8]) -> Option<&[u8]> {
    slc.iter().position(|c| *c == b'\0').map(|i| &slc[..i])
}

/// Chain an error, err(), after result of op().
pub fn chain_error<E, F, R>(op: F, err: E) -> Result<R>
    where F: FnOnce() -> Result<R>,
          E: FnOnce() -> Error
{
    op().map_err(|e| Error::with_chain(e, err()))
}
