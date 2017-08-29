// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;
use std::process::Command;

use consts::{DmFlags, IEC};
use deviceinfo::DeviceInfo;
use dm::{DM, DevId, DmName};
use lineardev::LinearDev;
use result::{DmResult, DmError, ErrorEnum};
use segment::Segment;
use shared::{device_exists, table_load, table_reload};
use types::{DataBlocks, MetaBlocks, Sectors, TargetLine};

/// Values are explicitly stated in the device-mapper kernel documentation.
#[allow(dead_code)]
const MIN_DATA_BLOCK_SIZE: Sectors = Sectors(128); // 64 KiB
#[allow(dead_code)]
const MAX_DATA_BLOCK_SIZE: Sectors = Sectors(2 * IEC::Mi); // 1 GiB
#[allow(dead_code)]
const MIN_RECOMMENDED_METADATA_SIZE: Sectors = Sectors(4 * IEC::Ki); // 2 MiB
#[allow(dead_code)]
const MAX_RECOMMENDED_METADATA_SIZE: Sectors = Sectors(32 * IEC::Mi); // 16 GiB

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
    /// Construct a new ThinPoolDev with the given data and meta devs.
    /// TODO: If the device already exists, verify that kernel's model
    /// matches arguments.
    pub fn new(name: DmName,
               dm: &DM,
               data_block_size: Sectors,
               low_water_mark: DataBlocks,
               meta: LinearDev,
               data: LinearDev)
               -> DmResult<ThinPoolDev> {
        let id = DevId::Name(name);
        let dev_info = if device_exists(dm, name)? {
            // TODO: Verify that kernel table matches our table.
            dm.device_status(id)?
        } else {
            dm.device_create(name, None, DmFlags::empty())?;
            let table =
                ThinPoolDev::dm_table(data.size()?, data_block_size, low_water_mark, &meta, &data);
            table_load(dm, id, &table)?
        };

        DM::wait_for_dm();
        Ok(ThinPoolDev {
               dev_info: dev_info,
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
    /// By "existing" is here meant that metadata for the thinpool already
    /// exists on the thinpool's metadata device.
    pub fn setup(name: DmName,
                 dm: &DM,
                 data_block_size: Sectors,
                 low_water_mark: DataBlocks,
                 meta: LinearDev,
                 data: LinearDev)
                 -> DmResult<ThinPoolDev> {
        if !Command::new("thin_check")
                .arg("-q")
                .arg(&meta.devnode()?)
                .status()?
                .success() {
            return Err(DmError::Dm(ErrorEnum::CheckFailed(meta, data),
                                   "thin_check failed, run thin_repair".into()));
        }

        ThinPoolDev::new(name, dm, data_block_size, low_water_mark, meta, data)
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start sec> <length> "thin-pool" <thin-pool-specific string>
    /// where the thin-pool-specific string has the format:
    /// <meta maj:min> <data maj:min> <block size> <low water mark>
    /// There is exactly one entry in the table.
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
    pub fn message(&self, dm: &DM, message: &str) -> DmResult<()> {
        dm.target_msg(DevId::Name(self.name()), Sectors(0), message)?;
        Ok(())
    }

    /// name of the thin pool device
    pub fn name(&self) -> DmName {
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
            .ok_or_else(|| {
                            DmError::Dm(ErrorEnum::NotFound,
                                        "No path associated with dev_info".into())
                        })

    }

    /// Get the current status of the thinpool.
    /// Returns an error if there was an error getting the status value.
    /// Panics if there is an error parsing the status value.
    // Justification: see comment above DM::parse_table_status.
    pub fn status(&self, dm: &DM) -> DmResult<ThinPoolStatus> {
        let (_, mut status) = dm.table_status(DevId::Name(self.name()), DmFlags::empty())?;

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

    /// Extend an existing meta device with additional new segments.
    pub fn extend_meta(&mut self, dm: &DM, new_segs: Vec<Segment>) -> DmResult<()> {
        self.meta_dev.extend(new_segs)?;
        table_reload(dm,
                     DevId::Name(self.name()),
                     &ThinPoolDev::dm_table(self.data_dev.size()?,
                                            self.data_block_size,
                                            self.low_water_mark,
                                            &self.meta_dev,
                                            &self.data_dev))?;
        Ok(())
    }

    /// Extend an existing data device with additional new segments.
    pub fn extend_data(&mut self, dm: &DM, new_segs: Vec<Segment>) -> DmResult<()> {
        self.data_dev.extend(new_segs)?;
        table_reload(dm,
                     DevId::Name(self.name()),
                     &ThinPoolDev::dm_table(self.data_dev.size()?,
                                            self.data_block_size,
                                            self.low_water_mark,
                                            &self.meta_dev,
                                            &self.data_dev))?;
        Ok(())
    }

    /// Remove the device from DM
    pub fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(DevId::Name(self.name()), DmFlags::empty())?;
        self.data_dev.teardown(dm)?;
        self.meta_dev.teardown(dm)?;
        Ok(())
    }
}
