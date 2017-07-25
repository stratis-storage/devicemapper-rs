// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;
use std::process::Command;

use consts::DmFlags;
use deviceinfo::DeviceInfo;
use dm::{DM, DevId};
use lineardev::LinearDev;
use result::{DmResult, DmError, ErrorEnum};
use segment::Segment;
use types::{DataBlocks, MetaBlocks, Sectors, TargetLine};

/// DM construct to contain thin provisioned devices
pub struct ThinPoolDev {
    dev_info: DeviceInfo,
    meta_dev: LinearDev,
    data_dev: LinearDev,
    data_block_size: Sectors,
    low_water_mark: DataBlocks,
}

impl fmt::Debug for ThinPoolDev {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, Copy)]
/// Contains values indicating the thinpool's used vs total
/// allocations for metadata and data blocks.
pub struct ThinPoolBlockUsage {
    /// The number of metadata blocks that are in use.
    pub used_meta: MetaBlocks,
    /// The total number of metadata blocks available to the thinpool.
    pub total_meta: MetaBlocks,
    /// The number of data blocks that are in use.
    pub used_data: DataBlocks,
    /// The total number of data blocks available to the thinpool.
    pub total_data: DataBlocks,
}

#[derive(Debug, Clone, Copy)]
/// Top-level thinpool status that indicates if it is working or failed.
pub enum ThinPoolStatus {
    /// The thinpool is working.
    Good(ThinPoolWorkingStatus, ThinPoolBlockUsage),
    /// The thinpool is in a failed condition.
    Fail,
}

#[derive(Debug, Clone, Copy)]
/// Indicates if a working thinpool is working optimally, or is
/// experiencing a non-fatal error condition.
pub enum ThinPoolWorkingStatus {
    /// The pool is working normally.
    Good,
    /// The pool has been forced to transition to read-only mode.
    ReadOnly,
    /// The pool is out of space.
    OutOfSpace,
    /// The pool needs checking.
    NeedsCheck,
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
               data_block_size: data_block_size,
               low_water_mark: low_water_mark,
           })
    }

    /// Obtain the meta device that backs this thin pool device.
    pub fn meta_dev(&self) -> &LinearDev {
        &self.meta_dev
    }

    /// Obtain the data device that backs this thin pool device.
    pub fn data_dev(&self) -> &LinearDev {
        &self.data_dev
    }

    /// Obtain the data block size for this thin pool device.
    pub fn data_block_size(&self) -> Sectors {
        self.data_block_size
    }

    /// Set up an existing ThinPoolDev.
    pub fn setup(name: &str,
                 dm: &DM,
                 length: Sectors,
                 data_block_size: Sectors,
                 low_water_mark: DataBlocks,
                 meta: LinearDev,
                 data: LinearDev)
                 -> DmResult<ThinPoolDev> {
        if !try!(Command::new("thin_check")
                     .arg("-q")
                     .arg(&try!(meta.devnode()))
                     .status())
                    .success() {
            return Err(DmError::Dm(ErrorEnum::CheckFailed(meta, data),
                                   "thin_check failed, run thin_repair".into()));
        }

        ThinPoolDev::new(name,
                         dm,
                         length,
                         data_block_size,
                         low_water_mark,
                         meta,
                         data)
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
        vec![(Sectors::default(), length, "thin-pool".to_owned(), params)]
    }

    /// send a message to DM thin pool
    pub fn message(dm: &DM, name: &str, message: &str) -> DmResult<()> {
        try!(dm.target_msg(&DevId::Name(name), 0, message));
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
            .ok_or(DmError::Dm(ErrorEnum::NotFound,
                               "No path associated with dev_info".into()))

    }

    /// Get the current status of the thinpool.
    pub fn status(&self, dm: &DM) -> DmResult<ThinPoolStatus> {
        let (_, mut status) = try!(dm.table_status(&DevId::Name(self.dev_info.name()),
                                                   DmFlags::empty()));

        assert_eq!(status.len(),
                   1,
                   "Kernel must return 1 line from thin pool status");

        let status_line = status.pop().expect("assertion above holds").3;
        if status_line.starts_with("Fail") {
            return Ok(ThinPoolStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(status_vals.len() >= 8,
                "Kernel must return at least 8 values from thin pool status");

        let usage = {
            let meta_vals = status_vals[1].split('/').collect::<Vec<_>>();
            let data_vals = status_vals[2].split('/').collect::<Vec<_>>();
            ThinPoolBlockUsage {
                used_meta: MetaBlocks(meta_vals[0]
                                          .parse::<u64>()
                                          .expect("used_meta value must be valid")),
                total_meta: MetaBlocks(meta_vals[1]
                                           .parse::<u64>()
                                           .expect("total_meta value must be valid")),
                used_data: DataBlocks(data_vals[0]
                                          .parse::<u64>()
                                          .expect("used_data value must be valid")),
                total_data: DataBlocks(data_vals[1]
                                           .parse::<u64>()
                                           .expect("total_data value must be valid")),
            }
        };

        match status_vals[7] {
            "-" => {}
            "needs_check" => {
                return Ok(ThinPoolStatus::Good(ThinPoolWorkingStatus::NeedsCheck, usage))
            }
            _ => panic!("Kernel returned unexpected 8th value in thin pool status"),
        }

        match status_vals[4] {
            "rw" => Ok(ThinPoolStatus::Good(ThinPoolWorkingStatus::Good, usage)),
            "ro" => Ok(ThinPoolStatus::Good(ThinPoolWorkingStatus::ReadOnly, usage)),
            "out_of_data_space" => {
                Ok(ThinPoolStatus::Good(ThinPoolWorkingStatus::OutOfSpace, usage))
            }
            _ => panic!("Kernel returned unexpected 5th value in thin pool status"),
        }
    }

    /// Reload the device mapper table.
    fn table_reload(&self, dm: &DM) -> DmResult<()> {
        try!(dm.table_reload(dm,
                             &DevId::Name(self.name()),
                             &ThinPoolDev::dm_table(try!(self.data_dev.size()),
                                                    self.data_block_size,
                                                    self.low_water_mark,
                                                    &self.meta_dev,
                                                    &self.data_dev)));
        Ok(())
    }

    /// Extend an existing meta device with additional new segments.
    pub fn extend_meta(&mut self, dm: &DM, new_segs: Vec<Segment>) -> DmResult<()> {
        try!(self.meta_dev.extend(new_segs));
        self.table_reload(dm)
    }

    /// Extend an existing data device with additional new segments.
    pub fn extend_data(&mut self, dm: &DM, new_segs: Vec<Segment>) -> DmResult<()> {
        try!(self.data_dev.extend(new_segs));
        self.table_reload(dm)
    }

    /// Remove the device from DM
    pub fn teardown(self, dm: &DM) -> DmResult<()> {
        try!(dm.device_remove(&DevId::Name(self.name()), DmFlags::empty()));
        try!(self.data_dev.teardown(dm));
        try!(self.meta_dev.teardown(dm));
        Ok(())
    }
}
