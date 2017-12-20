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


/// Trait for a TargetTable, a struct representing a table to be loaded or
/// read from DM.
pub trait TargetTable: fmt::Debug + PartialEq + Sized {
    /// Returns true if the devices represented by the two tables are not
    /// different in any identity changing way.
    fn equivalent_devices(left: &Self, right: &Self) -> bool;

    /// Constructs a table from a rough table returned by DM::table_status()
    fn read(table: &[(Sectors, Sectors, TargetTypeBuf, String)]) -> DmResult<Self>;

    /// Generates a table that can be loaded by DM::table_load()
    fn as_raw_table(&self) -> Vec<(Sectors, Sectors, TargetTypeBuf, String)>;
}


/// A trait capturing some shared properties of DM devices.
pub trait DmDevice<T: TargetTable> {
    /// The device's device node.
    fn devnode(&self) -> PathBuf;

    /// The device.
    fn device(&self) -> Device;

    /// The devicemapper table
    fn load_table(dm: &DM, id: &DevId) -> DmResult<T> {
        let (_, table) = dm.table_status(id, DmFlags::DM_STATUS_TABLE)?;
        T::read(&table)
    }

    /// The device's name.
    fn name(&self) -> &DmName;

    /// The number of sectors available for user data.
    fn size(&self) -> Sectors;

    /// Erase the kernel's memory of this device.
    fn teardown(self, dm: &DM) -> DmResult<()>;

    /// The device's UUID, if available.
    /// Note that the UUID is not any standard UUID format.
    fn uuid(&self) -> Option<&DmUuid>;
}

/// Send a message that expects no reply to target device.
pub fn message<T: TargetTable, D: DmDevice<T>>(dm: &DM, target: &D, msg: &str) -> DmResult<()> {
    dm.target_msg(&DevId::Name(target.name()), None, msg)?;
    Ok(())
}

/// Create a device, load a table, and resume it.
pub fn device_create<T: TargetTable>(dm: &DM,
                                     name: &DmName,
                                     uuid: Option<&DmUuid>,
                                     table: &T)
                                     -> DmResult<DeviceInfo> {
    dm.device_create(name, uuid, DmFlags::empty())?;

    let id = DevId::Name(name);
    let dev_info = match dm.table_load(&id, &table.as_raw_table()) {
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
pub fn device_match<T: TargetTable, D: DmDevice<T>>(dm: &DM,
                                                    dev: &D,
                                                    uuid: Option<&DmUuid>,
                                                    table: &T)
                                                    -> DmResult<()> {
    let kernel_table = D::load_table(dm, &DevId::Name(dev.name()))?;
    if !T::equivalent_devices(&kernel_table, table) {
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
pub fn table_reload<T: TargetTable>(dm: &DM, id: &DevId, table: &T) -> DmResult<DeviceInfo> {
    let dev_info = dm.table_load(id, &table.as_raw_table())?;
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
