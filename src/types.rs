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
#![allow(cast_lossless)]

use consts::SECTOR_SIZE;

use std::ascii::AsciiExt;
use std::borrow::Borrow;
use std::fmt;
use std::iter::Sum;
use std::mem::transmute;
use std::ops::{Deref, Div, Mul, Rem, Add};

use serde;

use super::deviceinfo::{DM_NAME_LEN, DM_UUID_LEN};
use super::result::{DmError, DmResult, ErrorEnum};

/// a kernel defined block size constant for a DM meta device
/// defined in drivers/md/persistent-data/dm-space-map-metadata.h line 12
const META_BLOCK_SIZE: Sectors = Sectors(8);

// division by self
macro_rules! self_div {
    ($T: ident) => {
        impl Div<$T> for $T {
            type Output = u64;
            fn div(self, rhs: $T) -> u64 {
                self.0 / *rhs
            }
        }
    }
}

// macros for implementing serialize and deserialize on all types
macro_rules! serde {
    ($T: ident) => {
        impl serde::Serialize for $T {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where S: serde::Serializer
            {
                serializer.serialize_u64(**self)
            }
        }

        impl <'de> serde::Deserialize<'de> for $T {
            fn deserialize<D>(deserializer: D) -> Result<$T, D::Error>
                where D: serde::de::Deserializer<'de>
            {
                Ok($T(serde::Deserialize::deserialize(deserializer)?))
            }
        }
    }
}

// macros for implementing Sum on all types
macro_rules! sum {
    ($T: ident) => {
        impl Sum for $T {
            fn sum<I: Iterator<Item = $T>>(iter: I) -> $T {
                iter.fold($T::default(), Add::add)
            }
        }
    }
}

// macros for unsigned operations on Sectors and Bytes
macro_rules! unsigned_div {
    ($t: ty, $T: ident) => {
        impl Div<$t> for $T {
            type Output = $T;
            fn div(self, rhs: $t) -> $T {
                $T(self.0 / rhs as u64)
            }
        }
    }
}

macro_rules! unsigned_mul {
    ($t: ty, $T: ident) => {
        impl Mul<$t> for $T {
            type Output = $T;
            fn mul(self, rhs: $t) -> $T {
                $T(self.0 * rhs as u64)
            }
        }

        impl Mul<$T> for $t {
            type Output = $T;
            fn mul(self, rhs: $T) -> $T {
                $T(self as u64 * rhs.0)
            }
        }
    }
}

macro_rules! unsigned_rem {
    ($t: ty, $T: ident) => {
        impl Rem<$t> for $T {
            type Output = $T;
            fn rem(self, rhs: $t) -> $T {
                $T(self.0 % rhs as u64)
            }
        }
    }
}

macro_rules! checked_add {
    ($T: ident) => {
        /// Add two items of type $T, return None if overflow.
        pub fn checked_add(&self, other: $T) -> Option<$T> {
            self.0.checked_add(other.0).map($T)
        }
    }
}

custom_derive! {
    #[derive(NewtypeAdd, NewtypeAddAssign,
             NewtypeDeref,
             NewtypeFrom,
             NewtypeSub, NewtypeSubAssign,
             Debug, Default, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
    /// A type for Data Blocks as used by the thin pool.
    pub struct DataBlocks(pub u64);
}

self_div!(DataBlocks);
serde!(DataBlocks);

unsigned_mul!(u64, DataBlocks);
unsigned_mul!(u32, DataBlocks);
unsigned_mul!(u16, DataBlocks);
unsigned_mul!(u8, DataBlocks);
unsigned_mul!(usize, DataBlocks);

impl fmt::Display for DataBlocks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} data blocks", self.0)
    }
}

custom_derive! {
    #[derive(NewtypeAdd, NewtypeAddAssign,
             NewtypeDeref,
             NewtypeFrom,
             NewtypeSub,
             Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
    /// A type for Meta Data blocks as used by the thin pool.
    /// MetaBlocks have a kernel defined constant size of META_BLOCK_SIZE
    pub struct MetaBlocks(pub u64);
}

impl MetaBlocks {
    /// Return the number of Sectors in the MetaBlocks.
    pub fn sectors(self) -> Sectors {
        self.0 * META_BLOCK_SIZE
    }
}

self_div!(MetaBlocks);
serde!(MetaBlocks);

unsigned_mul!(u64, MetaBlocks);
unsigned_mul!(u32, MetaBlocks);
unsigned_mul!(u16, MetaBlocks);
unsigned_mul!(u8, MetaBlocks);
unsigned_mul!(usize, MetaBlocks);

impl fmt::Display for MetaBlocks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} meta blocks", self.0)
    }
}

custom_derive! {
    #[derive(NewtypeAdd, NewtypeAddAssign,
             NewtypeDeref,
             NewtypeFrom,
             NewtypeSub, NewtypeSubAssign,
             Debug, Default, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
    /// Structure to represent bytes
    pub struct Bytes(pub u64);
}

impl Bytes {
    /// Return the number of Sectors fully contained in these bytes.
    pub fn sectors(self) -> Sectors {
        Sectors(self.0 / SECTOR_SIZE as u64)
    }

    checked_add!(Bytes);
}

self_div!(Bytes);
serde!(Bytes);
sum!(Bytes);

unsigned_mul!(u64, Bytes);
unsigned_mul!(u32, Bytes);
unsigned_mul!(u16, Bytes);
unsigned_mul!(u8, Bytes);
unsigned_mul!(usize, Bytes);

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} bytes", self.0)
    }
}

custom_derive! {
    #[derive(NewtypeAdd, NewtypeAddAssign,
             NewtypeDeref,
             NewtypeFrom,
             NewtypeSub, NewtypeSubAssign,
             Debug, Default, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
    /// A separate type to store counts and offsets expressed in
    /// 512-byte sectors.
    pub struct Sectors(pub u64);
}

impl Sectors {
    /// The number of bytes in these sectors.
    pub fn bytes(&self) -> Bytes {
        Bytes(self.0 * SECTOR_SIZE as u64)
    }

    /// The number of whole metablocks contained in these sectors.
    pub fn metablocks(&self) -> MetaBlocks {
        MetaBlocks(*self / META_BLOCK_SIZE)
    }

    checked_add!(Sectors);
}


self_div!(Sectors);
serde!(Sectors);
sum!(Sectors);

unsigned_div!(u64, Sectors);
unsigned_div!(u32, Sectors);
unsigned_div!(u16, Sectors);
unsigned_div!(u8, Sectors);
unsigned_div!(usize, Sectors);

unsigned_mul!(u64, Sectors);
unsigned_mul!(u32, Sectors);
unsigned_mul!(u16, Sectors);
unsigned_mul!(u8, Sectors);
unsigned_mul!(usize, Sectors);

unsigned_rem!(u64, Sectors);
unsigned_rem!(u32, Sectors);
unsigned_rem!(u16, Sectors);
unsigned_rem!(u8, Sectors);
unsigned_rem!(usize, Sectors);

impl fmt::Display for Sectors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} sectors", self.0)
    }
}

/// Returns an error if value is unsuitable.
fn str_check(value: &str, max_allowed_chars: usize) -> DmResult<()> {
    if !value.is_ascii() {
        let err_msg = format!("value {} has some non-ascii characters", value);
        return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
    }
    let num_chars = value.len();
    if num_chars == 0 {
        return Err(DmError::Dm(ErrorEnum::Invalid, "value has zero characters".into()));
    }
    if num_chars > max_allowed_chars {
        let err_msg = format!("value {} has {} chars which is greater than maximum allowed {}",
                              value,
                              num_chars,
                              max_allowed_chars);
        return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
    }
    Ok(())
}

/// Define borrowed and owned versions of string types that guarantee
/// conformance to DM restrictions, such as maximum length.
// This implementation follows the example of Path/PathBuf as closely as
// possible.
macro_rules! str_id {
    ($B: ident, $O: ident, $MAX: ident, $check: ident) => {
        /// The borrowed version of the DM identifier.
        #[derive(Debug, PartialEq, Eq, Hash)]
        pub struct $B {
            inner: str,
        }

        /// The owned version of the DM identifier.
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $O {
            inner: String,
        }

        impl $B {
            /// Create a new borrowed identifier from a `&str`.
            pub fn new(value: &str) -> DmResult<&$B> {
                $check(value, $MAX - 1)?;
                Ok(unsafe { transmute(value) })
            }
        }

        impl ToOwned for $B {
            type Owned = $O;
            fn to_owned(&self) -> $O {
                $O { inner: self.inner.to_owned() }
            }
        }

        impl AsRef<str> for $B {
            fn as_ref(&self) -> &str {
                &self.inner
            }
        }

        impl Deref for $B {
            type Target = str;
            fn deref(&self) -> &str {
                &self.inner
            }
        }

        impl fmt::Display for $B {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", &self.inner)
            }
        }

        impl $O {
            /// Construct a new owned identifier.
            pub fn new(value: String) -> DmResult<$O> {
                $check(&value, $MAX - 1)?;
                Ok($O { inner: value })
            }
        }

        impl AsRef<$B> for $O {
            fn as_ref(&self) -> &$B {
                self
            }
        }

        impl Borrow<$B> for $O {
            fn borrow(&self) -> &$B {
                self.deref()
            }
        }

        impl Deref for $O {
            type Target = $B;
            fn deref(&self) -> &$B {
                $B::new(&self.inner).expect("inner satisfies all correctness criteria for $B::new")
            }
        }
    }
}

/// A devicemapper name. Really just a string, but also the argument type of
/// DevId::Name. Used in function arguments to indicate that the function
/// takes only a name, not a devicemapper uuid.
str_id!(DmName, DmNameBuf, DM_NAME_LEN, str_check);

/// A devicemapper uuid. A devicemapper uuid has a devicemapper-specific
/// format.
str_id!(DmUuid, DmUuidBuf, DM_UUID_LEN, str_check);

/// Used as a parameter for functions that take either a Device name
/// or a Device UUID.
#[derive(Debug, PartialEq, Eq)]
pub enum DevId<'a> {
    /// The parameter is the device's name
    Name(&'a DmName),
    /// The parameter is the device's devicemapper uuid
    Uuid(&'a DmUuid),
}

impl<'a> fmt::Display for DevId<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DevId::Name(name) => write!(f, "{}", name),
            DevId::Uuid(uuid) => write!(f, "{}", uuid),
        }
    }
}


/// Number of bytes in Struct_dm_target_spec::target_type field.
const DM_TARGET_TYPE_LEN: usize = 16;

str_id!(TargetType, TargetTypeBuf, DM_TARGET_TYPE_LEN, str_check);

/// This 4-tuple consists of starting offset (sectors), length
/// (sectors), target type (string, e.g. "linear"), and
/// params(string). See target documentation for the format of each
/// target type's params field.
pub type TargetLine = (Sectors, Sectors, TargetTypeBuf, String);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// Verify that Sectors can be multiplied by a usize.
    /// The real test is that this tests compiles at all.
    fn test_usize() {
        assert_eq!(Sectors(0) * 32usize, Sectors(0));
    }
}
