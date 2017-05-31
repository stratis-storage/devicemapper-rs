// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use consts::SECTOR_SIZE;

use std::fmt;
use std::fmt::Display;
use std::iter::Sum;
use std::ops::{Div, Mul, Rem, Add};

use serde;

use step::Step;

// macro for implementing Step
macro_rules! impl_step {
    ($t: ident) => {
        impl Step for $t {
            fn next(&self) -> Option<$t> {
                self.0.next().map($t)
            }
            fn next_by(&self, by: &$t) -> Option<$t> {
                self.0.next_by(&**by).map($t)
            }
            fn prev(&self) -> Option<$t> {
                self.0.prev().map($t)
            }
            fn prev_by(&self, by: &$t) -> Option<$t> {
                self.0.prev_by(&**by).map($t)
            }
            fn steps_to(&self, value: &$t) -> $t {
                $t(self.0.steps_to(&**value))
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
             NewtypeSub,
             Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
    /// A type for Data Blocks as used by the thin pool.
    pub struct DataBlocks(pub u64);
}

impl serde::Serialize for DataBlocks {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer
    {
        serializer.serialize_u64(**self)
    }
}

impl<'de> serde::Deserialize<'de> for DataBlocks {
    fn deserialize<D>(deserializer: D) -> Result<DataBlocks, D::Error>
        where D: serde::de::Deserializer<'de>
    {
        let val = try!(serde::Deserialize::deserialize(deserializer));
        Ok(DataBlocks(val))
    }
}

custom_derive! {
    #[derive(NewtypeAdd, NewtypeAddAssign,
             NewtypeDeref,
             NewtypeFrom,
             NewtypeSub,
             Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
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

impl Sum for Bytes {
    fn sum<I: Iterator<Item = Bytes>>(iter: I) -> Bytes {
        iter.fold(Bytes(0), Add::add)
    }
}

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

impl_step!(Bytes);

custom_derive! {
    #[derive(NewtypeAdd, NewtypeAddAssign,
             NewtypeDeref,
             NewtypeFrom,
             NewtypeSub,
             Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
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

impl serde::Serialize for Sectors {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer
    {
        serializer.serialize_u64(**self)
    }
}

impl<'de> serde::Deserialize<'de> for Sectors {
    fn deserialize<D>(deserializer: D) -> Result<Sectors, D::Error>
        where D: serde::de::Deserializer<'de>
    {
        let val = try!(serde::Deserialize::deserialize(deserializer));
        Ok(Sectors(val))
    }
}

impl Sum for Sectors {
    fn sum<I: Iterator<Item = Sectors>>(iter: I) -> Sectors {
        iter.fold(Sectors(0), Add::add)
    }
}

impl_step!(Sectors);

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
pub type TargetLine = (u64, u64, String, String);
