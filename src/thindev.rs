// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;

use {DM, DevId, DeviceInfo, DmFlags};
use result::{DmResult, DmError, InternalError};
use thinpooldev::ThinPoolDev;
use TargetLine;

use types::Sectors;

/// DM construct for a thin block device
#[derive(Clone)]
pub struct ThinDev {
    dev_info: DeviceInfo,
    thin_id: u32,
    size: Sectors,
}

impl fmt::Debug for ThinDev {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

/// support use of DM for thin provisioned devices over pools
impl ThinDev {
    /// Use the given ThinPoolDev as backing space for a newly constructed
    /// thin provisioned ThinDev returned by new().
    pub fn new(name: &str,
               dm: &DM,
               thin_pool: ThinPoolDev,
               thin_id: u32,
               length: Sectors)
               -> DmResult<ThinDev> {

        try!(thin_pool.message(dm, &format!("create_thin {}", thin_id)));
        try!(dm.device_create(name, None, DmFlags::empty()));
        let id = &DevId::Name(name);
        let di = try!(dm.table_load(&id, &ThinDev::dm_table(&thin_pool, thin_id, length)));
        try!(dm.device_suspend(id, DmFlags::empty()));
        DM::wait_for_dm();
        Ok(ThinDev {
               dev_info: di,
               thin_id: thin_id,
               size: length,
           })
    }

    /// Generate a Vec<> to be passed to DM. The format of the Vec
    /// entries are: "<start> <length> thin <thinpool maj:min>
    /// <thin_id>"
    fn dm_table(thin_pool: &ThinPoolDev, thin_id: u32, length: Sectors) -> Vec<TargetLine> {
        let params = format!("{} {}", thin_pool.dstr(), thin_id);
        vec![(0u64, *length, "thin".to_owned(), params)]
    }

    /// name of the thin device
    pub fn name(&self) -> &str {
        self.dev_info.name()
    }

    /// Get the "x:y" device string for this LinearDev
    pub fn dstr(&self) -> String {
        self.dev_info.device().dstr()
    }

    /// path of the device node
    pub fn devnode(&self) -> DmResult<PathBuf> {
        self.dev_info
            .device()
            .devnode()
            .ok_or(DmError::Dm(InternalError("No path associated with dev_info".into())))

    }

    /// Remove the device from DM
    pub fn teardown(self, dm: &DM) -> DmResult<()> {
        try!(dm.device_remove(&DevId::Name(self.name()), DmFlags::empty()));
        Ok(())
    }
}
