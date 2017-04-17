// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;

use {DM, DevId, DeviceInfo, DmFlags};
use lineardev::LinearDev;
use result::{DmResult, DmError, InternalError};
use types::DataBlocks;
use types::Sectors;
use TargetLine;

/// DM construct to contain thin provisioned devices
pub struct ThinPoolDev {
    dev_info: DeviceInfo,
    meta_dev: LinearDev,
    data_dev: LinearDev,
}

impl fmt::Debug for ThinPoolDev {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}


/// Use DM to create a "thin-pool".  A "thin-pool" is shared space for
/// other thin provisioned devices to use.
///
/// See section "Setting up a fresh pool device":
/// https://www.kernel.org/doc/Documentation/device-mapper/thin-provisioning.txt
impl ThinPoolDev {
    /// Construct a new ThinPoolDev with the given data and meta devs.  The
    /// ThinPoolDev is used as backing for by ThinDev.
    pub fn new(name: &str,
               dm: &DM,
               length: Sectors,
               data_block_size: Sectors,
               low_water_mark: DataBlocks,
               meta: LinearDev,
               data: LinearDev)
               -> DmResult<ThinPoolDev> {
        try!(dm.device_create(name, None, DmFlags::empty()));

        let id = &DevId::Name(name);
        let di = try!(dm.table_load(id,
                                    &ThinPoolDev::dm_table(length,
                                                           data_block_size,
                                                           low_water_mark,
                                                           &meta,
                                                           &data)));
        try!(dm.device_suspend(id, DmFlags::empty()));

        DM::wait_for_dm();
        Ok(ThinPoolDev {
               dev_info: di,
               meta_dev: meta,
               data_dev: data,
           })
    }

    /// Generate a Vec<> to be passed to DM. The format of the Vec
    /// entries is: <start sec> <length> "thin-pool" <meta maj:min>
    /// <data maj:min> <block size> <low water mark>
    fn dm_table(length: Sectors,
                data_block_size: Sectors,
                low_water_mark: DataBlocks,
                meta: &LinearDev,
                data: &LinearDev)
                -> Vec<TargetLine> {
        let params = format!("{} {} {} {} 1 skip_block_zeroing",
                             meta.dstr(),
                             data.dstr(),
                             *data_block_size,
                             *low_water_mark);
        vec![(0u64, *length, "thin-pool".to_owned(), params)]
    }

    /// send a message to DM thin pool
    pub fn message(&self, dm: &DM, message: &str) -> DmResult<()> {
        try!(dm.target_msg(&DevId::Name(self.name()), 0, message));
        Ok(())
    }

    /// name of the thin pool device
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
        try!(self.data_dev.teardown(dm));
        try!(self.meta_dev.teardown(dm));
        Ok(())
    }
}
