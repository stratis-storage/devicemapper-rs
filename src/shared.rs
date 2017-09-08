// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A module to contain functionality shared among the various types of
/// devices.

use std::path::PathBuf;

use super::consts::{DmFlags, DM_SUSPEND};
use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DevId, DM, DmName};
use super::result::DmResult;
use super::types::{Sectors, TargetLineArg};

/// A trait capturing some shared properties of DM devices.
pub trait DmDevice {
    /// The device's device node.
    fn devnode(&self) -> PathBuf;

    /// The device.
    fn device(&self) -> Device;

    /// The device's name.
    fn name(&self) -> &DmName;

    /// The number of sectors available for user data.
    fn size(&self) -> Sectors;

    /// Erase the kernel's memory of this device.
    fn teardown(self, dm: &DM) -> DmResult<()>;
}

/// Create a device, load a table, and resume it.
pub fn device_create<T1, T2>(dm: &DM,
                             name: &DmName,
                             table: &[TargetLineArg<T1, T2>])
                             -> DmResult<DeviceInfo>
    where T1: AsRef<str>,
          T2: AsRef<str>
{
    dm.device_create(name, None, DmFlags::empty())?;

    let id = DevId::Name(name);
    let dev_info = match dm.table_load(&id, table) {
        Err(e) => {
            dm.device_remove(&id, DmFlags::empty())?;
            return Err(e);
        }
        Ok(dev_info) => dev_info,
    };
    dm.device_suspend(&id, DmFlags::empty())?;

    Ok(dev_info)
}

/// Reload the table for a device
pub fn table_reload<T1, T2>(dm: &DM,
                            id: &DevId,
                            table: &[TargetLineArg<T1, T2>])
                            -> DmResult<DeviceInfo>
    where T1: AsRef<str>,
          T2: AsRef<str>
{
    let dev_info = dm.table_load(id, table)?;
    dm.device_suspend(id, DM_SUSPEND)?;
    dm.device_suspend(id, DmFlags::empty())?;
    Ok(dev_info)
}

/// Check if a device of the given name exists.
pub fn device_exists(dm: &DM, name: &DmName) -> DmResult<bool> {
    // TODO: Why do we have to call .as_ref() here instead of relying on deref
    // coercion?
    Ok(dm.list_devices()
           .map(|l| l.iter().any(|&(ref n, _)| n.as_ref() == name))?)
}
