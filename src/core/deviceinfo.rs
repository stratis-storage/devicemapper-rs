// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::convert::TryFrom;

use semver::Version;

#[cfg(target_os = "android")]
use crate::core::util::str_from_byte_slice;
#[cfg(not(target_os = "android"))]
use crate::core::util::str_from_c_str;
use crate::{
    core::{
        device::Device,
        dm_flags::DmFlags,
        dm_ioctl as dmi,
        types::{DmName, DmNameBuf, DmUuid, DmUuidBuf},
    },
    result::{DmError, DmResult, ErrorEnum},
};

/// Name max length
pub const DM_NAME_LEN: usize = 128;
/// UUID max length
pub const DM_UUID_LEN: usize = 129;

/// Contains information about the device.
#[derive(Clone, Debug)]
pub struct DeviceInfo {
    version: Version,
    data_size: u32,
    data_start: u32,
    target_count: u32,
    open_count: i32,
    flags: DmFlags,
    event_nr: u32,
    dev: Device,
    name: DmNameBuf,
    uuid: Option<DmUuidBuf>,
}

impl TryFrom<dmi::Struct_dm_ioctl> for DeviceInfo {
    type Error = DmError;

    fn try_from(ioctl: dmi::Struct_dm_ioctl) -> DmResult<Self> {
        #[cfg(not(target_os = "android"))]
        let uuid = str_from_c_str(&ioctl.uuid as &[i8]);
        #[cfg(target_os = "android")]
        let uuid = str_from_byte_slice(&ioctl.uuid as &[u8]);
        let uuid = uuid.ok_or_else(|| {
            DmError::Dm(
                ErrorEnum::Invalid,
                "Devicemapper UUID is not null terminated".to_string(),
            )
        })?;
        let uuid = if uuid.is_empty() {
            None
        } else {
            Some(DmUuidBuf::new(uuid.to_string())?)
        };

        #[cfg(not(target_os = "android"))]
        let name = str_from_c_str(&ioctl.name as &[i8]);
        #[cfg(target_os = "android")]
        let name = str_from_byte_slice(&ioctl.name as &[u8]);
        let name = DmNameBuf::new(
            name.ok_or_else(|| {
                DmError::Dm(
                    ErrorEnum::Invalid,
                    "Devicemapper name is not null terminated".to_string(),
                )
            })?
            .to_string(),
        )?;
        Ok(DeviceInfo {
            version: Version::new(
                u64::from(ioctl.version[0]),
                u64::from(ioctl.version[1]),
                u64::from(ioctl.version[2]),
            ),
            data_size: ioctl.data_size,
            data_start: ioctl.data_start,
            target_count: ioctl.target_count,
            open_count: ioctl.open_count,
            flags: DmFlags::from_bits_truncate(ioctl.flags),
            event_nr: ioctl.event_nr,
            // dm_ioctl struct reserves 64 bits for device but kernel "huge"
            // encoding is only 32 bits.
            dev: Device::from_kdev_t(ioctl.dev as u32),
            name,
            uuid,
        })
    }
}

impl DeviceInfo {
    pub fn new(hdr: dmi::Struct_dm_ioctl) -> DmResult<Self> {
        DeviceInfo::try_from(hdr)
    }

    /// The major, minor, and patchlevel versions of devicemapper.
    pub fn version(&self) -> &Version {
        &self.version
    }

    /// The number of times the device is currently open.
    pub fn open_count(&self) -> i32 {
        self.open_count
    }

    /// The last event number for the device.
    pub fn event_nr(&self) -> u32 {
        self.event_nr
    }

    /// The device's major and minor device numbers, as a Device.
    pub fn device(&self) -> Device {
        self.dev
    }

    /// The device's name.
    pub fn name(&self) -> &DmName {
        self.name.as_ref()
    }

    /// The device's devicemapper uuid.
    pub fn uuid(&self) -> Option<&DmUuid> {
        self.uuid.as_ref().map(|uuid| uuid.as_ref())
    }

    /// The flags returned from the device.
    pub fn flags(&self) -> DmFlags {
        self.flags
    }
}
