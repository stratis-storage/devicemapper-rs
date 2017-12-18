// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// Allow the clippy error cast_lossless in this module.
// Otherwise, clippy will suggest that "as u64" be converted to "64::from".
// Unfortunately, the locations it suggests are all in macros, and u64
// does not implement From<usize>. It is preferable to use the macros
// uniformly for both usize and the other u* types.
// I don't think that casting from usize to u64 could be lossy, unless the
// code is running on a machine with 128 bit pointers, so this is not a
// pressing worry.

use std::fmt;
use std::str::FromStr;

use devicemapper::{Sectors, TargetTypeBuf};


/// The trait for properties of the params string of TargetType
pub trait TargetParams: fmt::Debug + fmt::Display + Eq + FromStr + PartialEq {}

impl TargetParams for String {}

/// One line of a device mapper table.
#[derive(Debug, PartialEq)]
pub struct TargetLine<T: TargetParams> {
    /// The start of the segment
    pub start: Sectors,
    /// The length of the segment
    pub length: Sectors,
    /// The target type
    pub target_type: TargetTypeBuf,
    /// The target specific parameters
    pub params: T,
}
