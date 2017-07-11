// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
use std::io;
use std::io::Error;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::io::ErrorKind::InvalidInput;
use std::os::unix::fs::MetadataExt;

/// A generic device-mapper device.

/// A struct containing the device's major and minor numbers
///
/// Also allows conversion to/from a single 64bit value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Device {
    /// Device major number
    pub major: u32,
    /// Device minor number
    pub minor: u8,
}

impl Device {
    /// The device node for this device.
    pub fn devnode(&self) -> PathBuf {
        vec!["/dev", &format!("dm-{}", self.minor)]
            .iter()
            .collect()
    }

    /// Get a string with the Device's major and minor numbers in
    /// "<major>:<minor>" format.
    pub fn dstr(&self) -> String {
        format!("{}:{}", self.major, self.minor)
    }
}

impl FromStr for Device {
    type Err = Error;
    fn from_str(s: &str) -> io::Result<Device> {
        match s.parse::<i64>() {
            Ok(x) => Ok(Device::from(x as u64)),
            Err(_) => {
                match Path::new(s).metadata() {
                    Ok(x) => {
                        if x.mode() & 0x6000 == 0x6000 {
                            // S_IFBLK
                            Ok(Device::from(x.rdev()))
                        } else {
                            Err(Error::new(InvalidInput, format!("{} not block device", s)))
                        }
                    }
                    Err(x) => Err(x),
                }
            }
        }
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
        ((dev.major << 8) ^ (dev.minor as u32 & 0xff)) as u64
    }
}
