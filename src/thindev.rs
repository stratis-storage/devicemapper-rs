// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;

use serde;

use consts::{DmFlags, DM_SUSPEND};
use deviceinfo::DeviceInfo;
use dm::{DM, DevId};
use result::{DmResult, DmError, ErrorEnum};
use thinpooldev::ThinPoolDev;
use types::TargetLine;

use types::Sectors;

const THIN_DEV_ID_LIMIT: u64 = 0x1000000; // 2 ^ 24

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
        Ok(ThinDevId { value: try!(serde::Deserialize::deserialize(deserializer)) })
    }
}

/// DM construct for a thin block device
pub struct ThinDev {
    dev_info: DeviceInfo,
    thin_id: ThinDevId,
    size: Sectors,
    thinpool_dstr: String,
}

impl fmt::Debug for ThinDev {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, Copy)]
/// Thin device status.
pub enum ThinStatus {
    /// Thin device is good. Includes number of mapped sectors, and
    /// highest mapped sector.
    Good((Sectors, Sectors)),
    /// Thin device is failed.
    Fail,
}

/// support use of DM for thin provisioned devices over pools
impl ThinDev {
    /// Use the given ThinPoolDev as backing space for a newly constructed
    /// thin provisioned ThinDev returned by new().
    pub fn new(name: &str,
               dm: &DM,
               thin_pool: &ThinPoolDev,
               thin_id: ThinDevId,
               length: Sectors)
               -> DmResult<ThinDev> {

        try!(thin_pool.message(dm, &format!("create_thin {}", thin_id)));
        ThinDev::setup(name, dm, thin_pool, thin_id, length)
    }

    /// Set up an existing thindev.
    /// By "existing" is here meant that metadata for this thin device exists
    /// on the metadata device for its thin pool.
    pub fn setup(name: &str,
                 dm: &DM,
                 thin_pool: &ThinPoolDev,
                 thin_id: ThinDevId,
                 length: Sectors)
                 -> DmResult<ThinDev> {

        try!(dm.device_create(name, None, DmFlags::empty()));
        let id = &DevId::Name(name);
        let thin_pool_dstr = thin_pool.dstr();
        let di = try!(dm.table_load(id, &ThinDev::dm_table(&thin_pool_dstr, thin_id, length)));
        try!(dm.device_suspend(id, DmFlags::empty()));
        DM::wait_for_dm();
        Ok(ThinDev {
               dev_info: di,
               thin_id: thin_id,
               size: length,
               thinpool_dstr: thin_pool_dstr,
           })
    }

    /// Generate a Vec<> to be passed to DM. The format of the Vec
    /// entries are: "<start> <length> thin <thinpool maj:min>
    /// <thin_id>"
    fn dm_table(thin_pool_dstr: &str, thin_id: ThinDevId, length: Sectors) -> Vec<TargetLine> {
        let params = format!("{} {}", thin_pool_dstr, thin_id);
        vec![(Sectors::default(), length, "thin".to_owned(), params)]
    }

    /// name of the thin device
    pub fn name(&self) -> &str {
        self.dev_info.name()
    }

    /// Get the "x:y" device string for this LinearDev
    pub fn dstr(&self) -> String {
        self.dev_info.device().dstr()
    }

    /// return the total size of the linear device
    pub fn size(&self) -> Sectors {
        self.size
    }

    /// return the thin id of the linear device
    pub fn id(&self) -> ThinDevId {
        self.thin_id
    }

    /// path of the device node
    pub fn devnode(&self) -> DmResult<PathBuf> {
        self.dev_info
            .device()
            .devnode()
            .ok_or(DmError::Dm(ErrorEnum::NotFound,
                               "No path associated with dev_info".into()))

    }

    /// Get the current status of the thin device.
    pub fn status(&self, dm: &DM) -> DmResult<ThinStatus> {
        let (_, mut status) = try!(dm.table_status(&DevId::Name(self.dev_info.name()),
                                                   DmFlags::empty()));

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

        Ok(ThinStatus::Good((Sectors(status_vals[0]
                                         .parse::<u64>()
                                         .expect("mapped sector count value must be valid")),
                             Sectors(status_vals[1]
                                         .parse::<u64>()
                                         .expect("highest mapped sector value must be valid")))))
    }

    /// Extend the thin device's (virtual) size by the number of
    /// sectors given.
    pub fn extend(&mut self, dm: &DM, sectors: Sectors) -> DmResult<()> {

        self.size += sectors;

        let id = &DevId::Name(self.dev_info.name());

        try!(dm.table_load(id,
                           &ThinDev::dm_table(&self.thinpool_dstr, self.thin_id, self.size)));
        try!(dm.device_suspend(id, DM_SUSPEND));
        try!(dm.device_suspend(id, DmFlags::empty()));

        Ok(())
    }

    /// Tear down the DM device, and also delete resources associated
    /// with its thin id from the thinpool.
    pub fn destroy(self, dm: &DM, thin_pool: &ThinPoolDev) -> DmResult<()> {
        let thin_id = self.thin_id;
        try!(self.teardown(dm));
        try!(thin_pool.message(dm, &format!("delete {}", thin_id)));

        Ok(())
    }

    /// Tear down the DM device.
    pub fn teardown(self, dm: &DM) -> DmResult<()> {
        try!(dm.device_remove(&DevId::Name(self.name()), DmFlags::empty()));
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use super::{THIN_DEV_ID_LIMIT, ThinDevId};

    #[test]
    /// Verify that new_checked_u64 discriminates.
    fn test_new_checked_u64() {
        assert!(ThinDevId::new_u64(2u64.pow(32)).is_err());
        assert!(ThinDevId::new_u64(THIN_DEV_ID_LIMIT - 1).is_ok());
    }
}
