// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A module to contain functionality shared among the various types of
/// devices.

use std::fmt;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use super::device::{Device, devnode_to_devno};
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::result::{DmError, DmResult, ErrorEnum};
use super::types::{DevId, DmName, DmUuid, Sectors, TargetTypeBuf};


/// The trait for properties of the params string of TargetType
pub trait TargetParams: fmt::Debug + fmt::Display + Eq + FromStr + PartialEq {}

impl TargetParams for String {}

/// One line of a device mapper table.
#[derive(Debug, Eq, PartialEq)]
pub struct TargetLine<T: TargetParams> {
    /// The start of the segment
    pub start: Sectors,
    /// The length of the segment
    pub length: Sectors,
    /// The target type
    pub target_type: TargetTypeBuf,
    /// The target specific parameters
    pub params: T,
}


/// A trait capturing some shared properties of DM devices.
pub trait DmDevice<T: TargetParams> {
    /// The device's device node.
    fn devnode(&self) -> PathBuf;

    /// The device.
    fn device(&self) -> Device;

    /// Check if tables indicate an equivalent device.
    fn equivalent_tables(left: &[TargetLine<T>], right: &[TargetLine<T>]) -> DmResult<bool>;

    /// The device's name.
    fn name(&self) -> &DmName;

    /// The number of sectors available for user data.
    fn size(&self) -> Sectors;

    /// The devicemapper table
    fn table(&self, dm: &DM) -> DmResult<Vec<TargetLine<T>>>
        where DmError: From<<T as FromStr>::Err>
    {
        let (_, table) = dm.table_status(&DevId::Name(self.name()), DmFlags::DM_STATUS_TABLE)?;
        table
            .into_iter()
            .map(|x| -> DmResult<TargetLine<T>> {
                     Ok(TargetLine {
                            start: x.0,
                            length: x.1,
                            target_type: x.2,
                            params: x.3.parse::<T>()?,
                        })
                 })
            .collect()
    }

    /// Erase the kernel's memory of this device.
    fn teardown(self, dm: &DM) -> DmResult<()>;

    /// The device's UUID, if available.
    /// Note that the UUID is not any standard UUID format.
    fn uuid(&self) -> Option<&DmUuid>;
}

/// Send a message that expects no reply to target device.
pub fn message<T: TargetParams, D: DmDevice<T>>(dm: &DM, target: &D, msg: &str) -> DmResult<()> {
    dm.target_msg(&DevId::Name(target.name()), None, msg)?;
    Ok(())
}

/// Create a device, load a table, and resume it.
pub fn device_create<T: TargetParams>(dm: &DM,
                                      name: &DmName,
                                      uuid: Option<&DmUuid>,
                                      table: &[TargetLine<T>])
                                      -> DmResult<DeviceInfo> {
    dm.device_create(name, uuid, DmFlags::empty())?;

    let id = DevId::Name(name);
    let table = table
        .iter()
        .map(|x| (x.start, x.length, x.target_type.clone(), x.params.to_string()))
        .collect::<Vec<_>>();
    let dev_info = match dm.table_load(&id, &table) {
        Err(e) => {
            dm.device_remove(&id, DmFlags::empty())?;
            return Err(e);
        }
        Ok(dev_info) => dev_info,
    };
    dm.device_suspend(&id, DmFlags::empty())?;

    Ok(dev_info)
}

/// Verify that kernel data matches arguments passed.
pub fn device_match<T: TargetParams, D: DmDevice<T>>(dm: &DM,
                                                     dev: &D,
                                                     uuid: Option<&DmUuid>,
                                                     table: &[TargetLine<T>])
                                                     -> DmResult<()>
    where DmError: From<<T as FromStr>::Err>
{
    let kernel_table = dev.table(dm)?;
    if !D::equivalent_tables(&kernel_table, table)? {
        let err_msg = format!("Specified new table \"{:?}\" does not match kernel table \"{:?}\"",
                              table,
                              kernel_table);

        return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
    }

    if dev.uuid() != uuid {
        let err_msg = format!("Specified uuid \"{:?}\" does not match kernel uuuid \"{:?}\"",
                              uuid,
                              dev.uuid());

        return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
    }
    Ok(())
}

/// Reload the table for a device
pub fn table_reload<T: TargetParams>(dm: &DM,
                                     id: &DevId,
                                     table: &[TargetLine<T>])
                                     -> DmResult<DeviceInfo> {
    let table = table
        .iter()
        .map(|x| (x.start, x.length, x.target_type.clone(), x.params.to_string()))
        .collect::<Vec<_>>();
    let dev_info = dm.table_load(id, &table)?;
    dm.device_suspend(id, DmFlags::DM_SUSPEND)?;
    dm.device_suspend(id, DmFlags::empty())?;
    Ok(dev_info)
}

/// Check if a device of the given name exists.
pub fn device_exists(dm: &DM, name: &DmName) -> DmResult<bool> {
    // TODO: Why do we have to call .as_ref() here instead of relying on deref
    // coercion?
    Ok(dm.list_devices()
           .map(|l| l.iter().any(|&(ref n, _, _)| n.as_ref() == name))?)
}

/// Parse a device from either of a path or a maj:min pair
pub fn parse_device(val: &str) -> DmResult<Device> {
    let device = if val.starts_with('/') {
        devnode_to_devno(Path::new(val))?
            .ok_or_else(|| {
                            DmError::Dm(ErrorEnum::Invalid,
                                        format!("failed to parse device number from \"{}\"", val))
                        })?
            .into()
    } else {
        val.parse::<Device>()?
    };
    Ok(device)
}
