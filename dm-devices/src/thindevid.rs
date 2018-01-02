// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::str::FromStr;

use serde;

use super::result::{DmError, DmResult, ErrorEnum};

const THIN_DEV_ID_LIMIT: u64 = 0x1_000_000; // 2 ^ 24

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
/// A thindev id is a 24 bit number, i.e., its bit width is not a power of 2.
pub struct ThinDevId {
    value: u32,
}

impl ThinDevId {
    /// Make a new ThinDevId.
    /// Return an error if value is too large to represent in 24 bits.
    pub fn new_u64(value: u64) -> DmResult<ThinDevId> {
        if value < THIN_DEV_ID_LIMIT {
            Ok(ThinDevId { value: value as u32 })
        } else {
            Err(DmError::Dm(ErrorEnum::Invalid,
                            format!("argument {} unrepresentable in 24 bits", value)))
        }
    }
}

impl From<ThinDevId> for u32 {
    fn from(id: ThinDevId) -> u32 {
        id.value
    }
}

impl fmt::Display for ThinDevId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}

impl FromStr for ThinDevId {
    type Err = DmError;

    fn from_str(s: &str) -> Result<ThinDevId, DmError> {
        s.parse::<u64>()
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for thindev id \"{}\"", s))
                     })
            .map(ThinDevId::new_u64)?
    }
}

impl serde::Serialize for ThinDevId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer
    {
        serializer.serialize_u32(self.value)
    }
}

impl<'de> serde::Deserialize<'de> for ThinDevId {
    fn deserialize<D>(deserializer: D) -> Result<ThinDevId, D::Error>
        where D: serde::de::Deserializer<'de>
    {
        Ok(ThinDevId { value: serde::Deserialize::deserialize(deserializer)? })
    }
}


#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    /// Verify that new_checked_u64 discriminates.
    fn test_new_checked_u64() {
        assert!(ThinDevId::new_u64(2u64.pow(32)).is_err());
        assert!(ThinDevId::new_u64(THIN_DEV_ID_LIMIT - 1).is_ok());
    }
}
