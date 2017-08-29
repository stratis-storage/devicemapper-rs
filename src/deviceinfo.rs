// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
use std::mem::transmute;
use std::str::from_utf8;

use dm_ioctl as dmi;

use consts::{DM_NAME_LEN, DmFlags, DM_UUID_LEN};
use device::Device;
use dm::{DmName, DmUuid};
use util::slice_to_null;

/// Contains information about the device.
#[derive(Clone, Copy)]
pub struct DeviceInfo {
    /// ioclt arugument consits of a single chunk of memory, with this
    /// structure at the start.
    hdr: dmi::Struct_dm_ioctl,
}

impl DeviceInfo {
    pub fn new(hdr: dmi::Struct_dm_ioctl) -> DeviceInfo {
        DeviceInfo { hdr }
    }

    /// The major, minor, and patchlevel versions of devicemapper.
    pub fn version(&self) -> (u32, u32, u32) {
        (self.hdr.version[0], self.hdr.version[1], self.hdr.version[2])
    }

    /// The number of times the device is currently open.
    pub fn open_count(&self) -> i32 {
        self.hdr.open_count
    }

    /// The last event number for the device.
    pub fn event_nr(&self) -> u32 {
        self.hdr.event_nr
    }

    /// The device's major and minor device numbers, as a Device.
    pub fn device(&self) -> Device {
        self.hdr.dev.into()
    }

    /// The device's name.
    pub fn name(&self) -> &DmName {
        let name: &[u8; DM_NAME_LEN] = unsafe { transmute(&self.hdr.name) };
        let slc = slice_to_null(name).expect("kernel ensures null-terminated");
        let name = from_utf8(slc).expect("kernel ensures ASCII characters");
        DmName::new(name).expect(".len() < DM_NAME_LEN")
    }

    /// The device's devicemapper uuid.
    pub fn uuid(&self) -> &DmUuid {
        let uuid: &[u8; DM_UUID_LEN] = unsafe { transmute(&self.hdr.uuid) };
        let slc = slice_to_null(uuid).expect("kernel ensures null-terminated");
        let uuid = from_utf8(slc).expect("kernel ensures ASCII characters");
        DmUuid::new(uuid).expect(".len() < DM_UUID_LEN")
    }

    /// The flags returned from the device.
    pub fn flags(&self) -> DmFlags {
        DmFlags::from_bits_truncate(self.hdr.flags)
    }
}
