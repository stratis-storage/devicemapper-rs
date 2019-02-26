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

range!(DataBlocks, "data blocks");

range!(MetaBlocks, "meta blocks");

impl MetaBlocks {
    /// Return the number of Sectors in the MetaBlocks.
    pub fn sectors(self) -> Sectors {
        self.0 * META_BLOCK_SIZE
    }
}

range!(Bytes, "bytes");

impl Bytes {
    /// Return the number of Sectors fully contained in these bytes.
    pub fn sectors(self) -> Sectors {
        Sectors(self.0 / SECTOR_SIZE as u64)
    }
}

range!(Sectors, "sectors");

impl Sectors {
    /// The number of bytes in these sectors.
    pub fn bytes(self) -> Bytes {
        Bytes(self.0 * SECTOR_SIZE as u64)
    }

    /// The number of whole metablocks contained in these sectors.
    pub fn metablocks(self) -> MetaBlocks {
        MetaBlocks(self / META_BLOCK_SIZE)
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
    /// Verify that creating an empty DmName is an error.
    pub fn test_empty_name() {
        assert!(match DmName::new("") {
            Err(DmError::Core(Error(ErrorKind::InvalidArgument(_), _))) => true,
            _ => false,
        })
    }
}
