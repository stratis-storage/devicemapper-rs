// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A module to contain functionality shared among the various types of
/// devices.

use std::path::PathBuf;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DevId, DM, DM_STATUS_TABLE, DM_SUSPEND, DmFlags, DmName};
use super::errors::{ErrorKind, Result, ResultExt};
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
    fn teardown(self, dm: &DM) -> Result<()>;
}

/// Create a device, load a table, and resume it.
pub fn device_create<T1, T2>(dm: &DM,
                             name: &DmName,
                             table: &[TargetLineArg<T1, T2>])
                             -> Result<DeviceInfo>
    where T1: AsRef<str>,
          T2: AsRef<str>
{
    dm.device_create(name, None, DmFlags::empty())
        .chain_err(|| ErrorKind::DeviceCreationError(name.to_owned()))?;

    let id = DevId::Name(name);
    let dev_info = match dm.table_load(&id, table) {
        Err(err) => {
            // TODO: record the cleanup failure appropriately
            let _ = dm.device_remove(&id, DmFlags::empty());
            return Err(err.chain_err(|| ErrorKind::DeviceCreationError(name.to_owned())));
        }
        Ok(dev_info) => dev_info,
    };
    dm.device_suspend(&id, DmFlags::empty())?;

    Ok(dev_info)
}

/// Setup a device that is already known to the kernel.
/// Verify that kernel data matches arguments passed.
/// Return the status of the device.
pub fn device_setup(dm: &DM,
                    id: &DevId,
                    table: &[TargetLineArg<String, String>])
                    -> Result<DeviceInfo> {
    if dm.table_status(id, DM_STATUS_TABLE)?.1 != table {
        let err_msg = "Specified data does not match kernel data";
        return Err(ErrorKind::InvalidArgument(err_msg.into()).into());
    }
    Ok(dm.device_status(id)?)
}

/// Reload the table for a device
pub fn table_reload<T1, T2>(dm: &DM,
                            id: &DevId,
                            table: &[TargetLineArg<T1, T2>])
                            -> Result<DeviceInfo>
    where T1: AsRef<str>,
          T2: AsRef<str>
{
    let dev_info = dm.table_load(id, table)?;
    dm.device_suspend(id, DM_SUSPEND)?;
    dm.device_suspend(id, DmFlags::empty())?;
    Ok(dev_info)
}

/// Check if a device of the given name exists.
pub fn device_exists(dm: &DM, name: &DmName) -> Result<bool> {
    // TODO: Why do we have to call .as_ref() here instead of relying on deref
    // coercion?
    Ok(dm.list_devices()
           .map_err(|e| {
                        e.chain_err(|| format!("can not determine whether device {} exists", name))
                    })
           .map(|l| l.iter().any(|&(ref n, _)| n.as_ref() == name))?)
}
