// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::borrow::Borrow;
use std::fmt;
use std::iter::Sum;
use std::ops::{Add, Deref, Div, Mul, Rem};

use serde;

use crate::consts::SECTOR_SIZE;
use crate::deviceinfo::{DM_NAME_LEN, DM_UUID_LEN};
use crate::errors::ErrorKind;
use crate::result::{DmError, DmResult};

/// a kernel defined block size constant for any DM meta device
/// a DM meta device may store cache device or thinpool device metadata
/// defined in drivers/md/persistent-data/dm-space-map-metadata.h as
/// DM_SM_METADATA_BLOCK_SIZE.
const META_BLOCK_SIZE: Sectors = Sectors(8);

/// The maximum size of a metadata device.
/// defined in drivers/md/persistent-data/dm-space-map-metadata.h as
/// DM_SM_METADATA_MAX_BLOCKS.
/// As far as I can tell, this is not a limit on the size of a designated
/// metadata device, but instead on the possible usage of that device.
#[allow(dead_code)]
const MAX_META_DEV_SIZE: MetaBlocks = MetaBlocks(255 * ((1 << 14) - 64));

macro_attr! {
    #[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
    /// A type for Data Blocks as used by the thin pool.
    pub struct DataBlocks(pub u64);
}

impl DataBlocks {
    checked_add!(DataBlocks);
}

NewtypeAdd! { () pub struct DataBlocks(u64); }
NewtypeAddAssign! { () pub struct DataBlocks(u64); }
NewtypeDeref! { () pub struct DataBlocks(u64); }
NewtypeFrom! { () pub struct DataBlocks(u64); }
NewtypeSub! { () pub struct DataBlocks(u64); }
NewtypeSubAssign! { () pub struct DataBlocks(u64); }

self_div!(DataBlocks);
serde!(DataBlocks);
debug!(DataBlocks);

unsigned_div!(u64, DataBlocks);
unsigned_div!(u32, DataBlocks);
unsigned_div!(u16, DataBlocks);
unsigned_div!(u8, DataBlocks);
usize_div!(DataBlocks);

unsigned_mul!(u64, DataBlocks);
unsigned_mul!(u32, DataBlocks);
unsigned_mul!(u16, DataBlocks);
unsigned_mul!(u8, DataBlocks);
usize_mul!(DataBlocks);

unsigned_rem!(u64, DataBlocks);
unsigned_rem!(u32, DataBlocks);
unsigned_rem!(u16, DataBlocks);
unsigned_rem!(u8, DataBlocks);
usize_rem!(DataBlocks);
rem!(DataBlocks);

impl fmt::Display for DataBlocks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} data blocks", self.0)
    }
}

macro_attr! {
    #[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
    /// A type for Meta Data blocks as used by the thin pool.
    /// MetaBlocks have a kernel defined constant size of META_BLOCK_SIZE
    pub struct MetaBlocks(pub u64);
}

impl MetaBlocks {
    /// Return the number of Sectors in the MetaBlocks.
    pub fn sectors(self) -> Sectors {
        self.0 * META_BLOCK_SIZE
    }

    checked_add!(MetaBlocks);
}

NewtypeAdd! { () pub struct MetaBlocks(u64); }
NewtypeAddAssign! { () pub struct MetaBlocks(u64); }
NewtypeDeref! { () pub struct MetaBlocks(u64); }
NewtypeFrom! { () pub struct MetaBlocks(u64); }
NewtypeSub! { () pub struct MetaBlocks(u64); }
NewtypeSubAssign! { () pub struct MetaBlocks(u64); }

self_div!(MetaBlocks);
serde!(MetaBlocks);
debug!(MetaBlocks);

unsigned_div!(u64, MetaBlocks);
unsigned_div!(u32, MetaBlocks);
unsigned_div!(u16, MetaBlocks);
unsigned_div!(u8, MetaBlocks);
usize_div!(MetaBlocks);

unsigned_mul!(u64, MetaBlocks);
unsigned_mul!(u32, MetaBlocks);
unsigned_mul!(u16, MetaBlocks);
unsigned_mul!(u8, MetaBlocks);
usize_mul!(MetaBlocks);

unsigned_rem!(u64, MetaBlocks);
unsigned_rem!(u32, MetaBlocks);
unsigned_rem!(u16, MetaBlocks);
unsigned_rem!(u8, MetaBlocks);
usize_rem!(MetaBlocks);
rem!(MetaBlocks);

impl fmt::Display for MetaBlocks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} meta blocks", self.0)
    }
}

macro_attr! {
    #[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
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

NewtypeAdd! { () pub struct Bytes(u64); }
NewtypeAddAssign! { () pub struct Bytes(u64); }
NewtypeDeref! { () pub struct Bytes(u64); }
NewtypeFrom! { () pub struct Bytes(u64); }
NewtypeSub! { () pub struct Bytes(u64); }
NewtypeSubAssign! { () pub struct Bytes(u64); }

self_div!(Bytes);
serde!(Bytes);
debug!(Bytes);
sum!(Bytes);

unsigned_div!(u64, Bytes);
unsigned_div!(u32, Bytes);
unsigned_div!(u16, Bytes);
unsigned_div!(u8, Bytes);
usize_div!(Bytes);

unsigned_mul!(u64, Bytes);
unsigned_mul!(u32, Bytes);
unsigned_mul!(u16, Bytes);
unsigned_mul!(u8, Bytes);
usize_mul!(Bytes);

unsigned_rem!(u64, Bytes);
unsigned_rem!(u32, Bytes);
unsigned_rem!(u16, Bytes);
unsigned_rem!(u8, Bytes);
usize_rem!(Bytes);
rem!(Bytes);

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} bytes", self.0)
    }
}

macro_attr! {
    #[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
    /// A separate type to store counts and offsets expressed in
    /// 512-byte sectors.
    pub struct Sectors(pub u64);
}

impl Sectors {
    /// The number of bytes in these sectors.
    pub fn bytes(self) -> Bytes {
        Bytes(self.0 * SECTOR_SIZE as u64)
    }

    /// The number of whole metablocks contained in these sectors.
    pub fn metablocks(self) -> MetaBlocks {
        MetaBlocks(self / META_BLOCK_SIZE)
    }

    checked_add!(Sectors);
}

NewtypeAdd! { () pub struct Sectors(u64); }
NewtypeAddAssign! { () pub struct Sectors(u64); }
NewtypeDeref! { () pub struct Sectors(u64); }
NewtypeFrom! { () pub struct Sectors(u64); }
NewtypeSub! { () pub struct Sectors(u64); }
NewtypeSubAssign! { () pub struct Sectors(u64); }

self_div!(Sectors);
serde!(Sectors);
debug!(Sectors);
sum!(Sectors);

unsigned_div!(u64, Sectors);
unsigned_div!(u32, Sectors);
unsigned_div!(u16, Sectors);
unsigned_div!(u8, Sectors);
usize_div!(Sectors);

unsigned_mul!(u64, Sectors);
unsigned_mul!(u32, Sectors);
unsigned_mul!(u16, Sectors);
unsigned_mul!(u8, Sectors);
usize_mul!(Sectors);

unsigned_rem!(u64, Sectors);
unsigned_rem!(u32, Sectors);
unsigned_rem!(u16, Sectors);
unsigned_rem!(u8, Sectors);
usize_rem!(Sectors);
rem!(Sectors);

impl fmt::Display for Sectors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} sectors", self.0)
    }
}

/// Returns an error if value is unsuitable.
fn str_check(value: &str, max_allowed_chars: usize) -> DmResult<()> {
    if !value.is_ascii() {
        let err_msg = format!("value {} has some non-ascii characters", value);
        return Err(DmError::Core(ErrorKind::InvalidArgument(err_msg).into()));
    }
    let num_chars = value.len();
    if num_chars == 0 {
        return Err(DmError::Core(
            ErrorKind::InvalidArgument("value has zero characters".into()).into(),
        ));
    }
    if num_chars > max_allowed_chars {
        let err_msg = format!(
            "value {} has {} chars which is greater than maximum allowed {}",
            value, num_chars, max_allowed_chars
        );
        return Err(DmError::Core(ErrorKind::InvalidArgument(err_msg).into()));
    }
    Ok(())
}

/// Define borrowed and owned versions of string types that guarantee
/// conformance to DM restrictions, such as maximum length.
// This implementation follows the example of Path/PathBuf as closely as
// possible.
macro_rules! str_id {
    ($B:ident, $O:ident, $MAX:ident, $check:ident) => {
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
            #[allow(clippy::new_ret_no_self)]
            pub fn new(value: &str) -> DmResult<&$B> {
                $check(value, $MAX - 1)?;
                Ok(unsafe { &*(value as *const str as *const $B) })
            }

            /// Get the inner value as bytes
            pub fn as_bytes(&self) -> &[u8] {
                self.inner.as_bytes()
            }
        }

        impl ToOwned for $B {
            type Owned = $O;
            fn to_owned(&self) -> $O {
                $O {
                    inner: self.inner.to_owned(),
                }
            }
        }

        impl fmt::Display for $B {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", &self.inner)
            }
        }

        impl $O {
            /// Construct a new owned identifier.
            #[allow(clippy::new_ret_no_self)]
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
    };
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

#[cfg(test)]
mod tests {
    use crate::errors::Error;

    use super::*;

    #[test]
    /// Verify that Sectors can be multiplied by a usize.
    /// The real test is that this tests compiles at all.
    fn test_usize() {
        assert_eq!(Sectors(0) * 32usize, Sectors(0));
    }

    #[test]
    /// Verify that creating an empty DmName is an error.
    pub fn test_empty_name() {
        assert!(match DmName::new("") {
            Err(DmError::Core(Error(ErrorKind::InvalidArgument(_), _))) => true,
            _ => false,
        })
    }

    #[test]
    /// Test the return types of various % operations
    pub fn test_sectors_remainder() {
        assert_eq!(Sectors(13) % Sectors(11), Sectors(2));
        assert_eq!(Sectors(13) % 11usize, Sectors(2));
    }
}
