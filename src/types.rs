// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use consts::SECTOR_SIZE;

use std::fmt;
use std::fmt::Display;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Deref, Div, Mul, Rem, Sub, SubAssign};

use serde;

/// a kernel defined block size constant for a DM meta device
/// defined in drivers/md/persistent-data/dm-space-map-metadata.h line 12
const META_BLOCK_SIZE: Sectors = Sectors(8);

trait Block: Sized + Add + AddAssign + Deref + Sub + SubAssign {}


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
                Ok($T(try!(serde::Deserialize::deserialize(deserializer))))
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

serde!(DataBlocks);

unsigned_mul!(u64, DataBlocks);
unsigned_mul!(u32, DataBlocks);
unsigned_mul!(u16, DataBlocks);
unsigned_mul!(u8, DataBlocks);
unsigned_mul!(usize, DataBlocks);

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

serde!(MetaBlocks);

unsigned_mul!(u64, MetaBlocks);
unsigned_mul!(u32, MetaBlocks);
unsigned_mul!(u16, MetaBlocks);
unsigned_mul!(u8, MetaBlocks);
unsigned_mul!(usize, MetaBlocks);

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

serde!(Bytes);
sum!(Bytes);

unsigned_mul!(u64, Bytes);
unsigned_mul!(u32, Bytes);
unsigned_mul!(u16, Bytes);
unsigned_mul!(u8, Bytes);
unsigned_mul!(usize, Bytes);

impl Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
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

    checked_add!(Sectors);
}


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

impl Display for Sectors {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} sectors", self.0)
    }
}


/// This 4-tuple consists of starting offset (sectors), length
/// (sectors), target type (string, e.g. "linear"), and
/// params(string). See target documentation for the format of each
/// target type's params field.
pub type TargetLine = (Sectors, Sectors, String, String);

/// The same as TargetLine, except generalized for argument rather than
/// return type.
pub type TargetLineArg<T1, T2> = (Sectors, Sectors, T1, T2);
