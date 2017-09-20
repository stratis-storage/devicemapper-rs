// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A module to contain functionality shared among the various types of
/// devices.

use std::path::PathBuf;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DevId, DM, DM_STATUS_TABLE, DM_SUSPEND, DmFlags, DmName};
use super::errors::Result;
use super::types::{Sectors, TargetLineArg};
use super::util::chain_error;


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
    fn teardown(self, dm: &DM) -> Result<()>;
}

/// Create a device, load a table, and resume it.
/// Returns a DeviceCreation error on failure.
pub fn device_create<T1, T2>(dm: &DM,
                             name: &DmName,
                             table: &[TargetLineArg<T1, T2>])
                             -> Result<DeviceInfo>
    where T1: AsRef<str>,
          T2: AsRef<str>
{
    chain_error(|| {
        dm.device_create(name, None, DmFlags::empty())?;

        let id = DevId::Name(name);
        let dev_info = match dm.table_load(&id, table) {
            Err(err) => {
                // TODO: record the cleanup failure appropriately
                let _ = dm.device_remove(&id, DmFlags::empty());
                return Err(err);
            }
            Ok(dev_info) => dev_info,
        };
        dm.device_suspend(&id, DmFlags::empty())?;
        Ok(dev_info)
    },
                || format!("failed to create device {}", name.as_ref()).into())
}

/// Setup a device that is already known to the kernel.
/// Verify that kernel data matches arguments passed.
/// Return the status of the device.
pub fn device_setup(dm: &DM,
                    id: &DevId,
                    table: &[TargetLineArg<String, String>])
                    -> Result<DeviceInfo> {
    chain_error(|| {
                    if dm.table_status(id, DM_STATUS_TABLE)?.1 != table {
                        return Err("Specified data does not match kernel data".into());
                    }
                    Ok(dm.device_status(id)?)
                },
                || format!("Failed to set up device {}", id).into())
}

/// Reload the table for a device
pub fn table_reload<T1, T2>(dm: &DM,
                            id: &DevId,
                            table: &[TargetLineArg<T1, T2>])
                            -> Result<DeviceInfo>
    where T1: AsRef<str>,
          T2: AsRef<str>
{
    chain_error(|| {
                    let dev_info = dm.table_load(id, table)?;
                    dm.device_suspend(id, DM_SUSPEND)?;
                    dm.device_suspend(id, DmFlags::empty())?;
                    Ok(dev_info)
                },
                || "Failed to reload table.".into())
}

/// Check if a device of the given name exists.
pub fn device_exists(dm: &DM, name: &DmName) -> Result<bool> {
    // TODO: Why do we have to call .as_ref() here instead of relying on deref
    // coercion?
    chain_error(|| {
                    Ok(dm.list_devices()
                           .map(|l| l.iter().any(|&(ref n, _)| n.as_ref() == name))?)
                },
                || format!("Can not determine whether device {} exists", name).into())
}
