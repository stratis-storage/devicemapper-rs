// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;

use serde;

use consts::{DM_STATUS_TABLE, DmFlags};
use device::Device;
use deviceinfo::DeviceInfo;
use dm::{DM, DevId, DmName};
use result::{DmError, DmResult, ErrorEnum};
use shared::{DmDevice, device_create, device_exists, table_reload};
use thinpooldev::ThinPoolDev;
use types::TargetLine;

use types::Sectors;

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

/// DM construct for a thin block device
pub struct ThinDev {
    dev_info: Box<DeviceInfo>,
    thin_id: ThinDevId,
    size: Sectors,
    thinpool: Device,
}

impl fmt::Debug for ThinDev {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

impl DmDevice for ThinDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.size
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
/// Thin device status.
pub enum ThinStatus {
    /// Thin device is good. Includes number of mapped sectors, and
    /// highest mapped sector.
    Good((Sectors, Option<Sectors>)),
    /// Thin device is failed.
    Fail,
}

/// support use of DM for thin provisioned devices over pools
impl ThinDev {
    /// Use the given ThinPoolDev as backing space for a newly constructed
    /// thin provisioned ThinDev returned by new().
    pub fn new(name: &DmName,
               dm: &DM,
               thin_pool: &ThinPoolDev,
               thin_id: ThinDevId,
               length: Sectors)
               -> DmResult<ThinDev> {

        thin_pool
            .message(dm, &format!("create_thin {}", thin_id))?;
        ThinDev::setup(name, dm, thin_pool, thin_id, length)
    }

    /// Set up an existing thindev.
    /// By "existing" is here meant that metadata for this thin device exists
    /// on the metadata device for its thin pool.
    pub fn setup(name: &DmName,
                 dm: &DM,
                 thin_pool: &ThinPoolDev,
                 thin_id: ThinDevId,
                 length: Sectors)
                 -> DmResult<ThinDev> {

        let id = DevId::Name(name);
        let thin_pool_device = thin_pool.device();
        let table = ThinDev::dm_table(thin_pool_device, thin_id, length);

        let dev_info = if device_exists(dm, name)? {
            if dm.table_status(&id, DM_STATUS_TABLE)?.1 != table {
                return Err(DmError::Dm(ErrorEnum::Invalid,
                                       "specified data does not match kernel data".into()));
            }
            Box::new(dm.device_status(&id)?)
        } else {
            Box::new(device_create(dm, name, &id, &table)?)
        };

        DM::wait_for_dm();
        Ok(ThinDev {
               dev_info: dev_info,
               thin_id: thin_id,
               size: length,
               thinpool: thin_pool_device,
           })
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start> <length> "thin" <thin device specific string>
    /// where the thin device specific string has the format:
    /// <thinpool maj:min> <thin_id>
    /// There is exactly one entry in the table.
    fn dm_table(thin_pool: Device, thin_id: ThinDevId, length: Sectors) -> Vec<TargetLine> {
        let params = format!("{} {}", thin_pool, thin_id);
        vec![(Sectors::default(), length, "thin".to_owned(), params)]
    }

    /// return the thin id of the linear device
    pub fn id(&self) -> ThinDevId {
        self.thin_id
    }

    /// Get the current status of the thin device.
    pub fn status(&self, dm: &DM) -> DmResult<ThinStatus> {
        let (_, mut status) = dm.table_status(&DevId::Name(self.name()), DmFlags::empty())?;

        assert_eq!(status.len(),
                   1,
                   "Kernel must return 1 line from thin status");

        let status_line = status.pop().expect("assertion above holds").3;
        if status_line.starts_with("Fail") {
            return Ok(ThinStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(status_vals.len() >= 2,
                "Kernel must return at least 2 values from thin pool status");

        let count = status_vals[0]
            .parse::<u64>()
            .expect("Kernel always returns a parseable u64 for sector count");

        let highest = if count == 0 {
            None
        } else {
            Some(Sectors(status_vals[1]
                             .parse::<u64>()
                             .expect("Kernel always returns a parseable u64 when count > 0")))
        };

        Ok(ThinStatus::Good((Sectors(count), highest)))
    }

    /// Extend the thin device's (virtual) size by the number of
    /// sectors given.
    pub fn extend(&mut self, dm: &DM, sectors: Sectors) -> DmResult<()> {
        self.size += sectors;

        table_reload(dm,
                     &DevId::Name(self.name()),
                     &ThinDev::dm_table(self.thinpool, self.thin_id, self.size))?;

        Ok(())
    }

    /// Tear down the DM device, and also delete resources associated
    /// with its thin id from the thinpool.
    pub fn destroy(self, dm: &DM, thin_pool: &ThinPoolDev) -> DmResult<()> {
        let thin_id = self.thin_id;
        self.teardown(dm)?;
        thin_pool.message(dm, &format!("delete {}", thin_id))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use std::fs::OpenOptions;
    use std::path::Path;

    use super::super::loopbacked::{blkdev_size, test_with_spec};
    use super::super::thinpooldev::minimal_thinpool;

    use super::*;

    const MIN_THIN_DEV_SIZE: Sectors = Sectors(1);

    #[test]
    /// Verify that new_checked_u64 discriminates.
    fn test_new_checked_u64() {
        assert!(ThinDevId::new_u64(2u64.pow(32)).is_err());
        assert!(ThinDevId::new_u64(THIN_DEV_ID_LIMIT - 1).is_ok());
    }

    /// Verify that specifying a size of 0 Sectors will cause a failure.
    fn test_zero_size(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        assert!(ThinDev::new(&DmName::new("name").expect("is valid DM name"),
                             &dm,
                             &tp,
                             ThinDevId::new_u64(0).expect("is below limit"),
                             Sectors(0))
                        .is_err());
        tp.teardown(&dm).unwrap();
    }

    /// Verify success when constructing a new ThinDev. Check that the
    /// status of the device is as expected.
    fn test_basic(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        let td_size = MIN_THIN_DEV_SIZE;
        let td = ThinDev::new(&DmName::new("name").expect("is valid DM name"),
                              &dm,
                              &tp,
                              ThinDevId::new_u64(0).expect("is below limit"),
                              td_size)
                .unwrap();

        assert!(match td.status(&dm).unwrap() {
                    ThinStatus::Fail => false,
                    _ => true,
                });

        assert_eq!(blkdev_size(&OpenOptions::new()
                                    .read(true)
                                    .open(td.devnode())
                                    .unwrap()),
                   td_size.bytes());

        td.teardown(&dm).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify that constructing a thin device with an already in use id
    /// fails.
    fn test_twice(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);
        let id = ThinDevId::new_u64(0).expect("is below limit");

        let td = ThinDev::new(&DmName::new("name1").expect("is valid DM name"),
                              &dm,
                              &tp,
                              id,
                              MIN_THIN_DEV_SIZE)
                .unwrap();

        assert!(ThinDev::new(&DmName::new("name2").expect("is valid DM name"),
                             &dm,
                             &tp,
                             id,
                             MIN_THIN_DEV_SIZE)
                        .is_err());

        td.teardown(&dm).unwrap();
        tp.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_basic() {
        test_with_spec(1, test_basic);
    }

    #[test]
    fn loop_test_twice() {
        test_with_spec(1, test_twice);
    }

    #[test]
    fn loop_test_zero_size() {
        test_with_spec(1, test_zero_size);
    }
}
