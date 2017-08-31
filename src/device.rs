// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;

/// A struct containing the device's major and minor numbers
///
/// Also allows conversion to/from a single 64bit value.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Device {
    /// Device major number
    pub major: u32,
    /// Device minor number
    pub minor: u8,
}

/// Display format is the device number in "<major>:<minor>" format
impl fmt::Display for Device {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.major, self.minor)
    }
}

impl From<u64> for Device {
    fn from(val: u64) -> Device {
        Device {
            major: (val >> 8) as u32,
            minor: (val & 0xff) as u8,
        }
    }
}

impl From<Device> for u64 {
    fn from(dev: Device) -> u64 {
        u64::from((dev.major << 8) ^ (u32::from(dev.minor) & 0xff))
    }
}
