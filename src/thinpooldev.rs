// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::path::PathBuf;

use super::consts::IEC;
use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DevId, DmFlags, DmName, DmUuid};
use super::lineardev::LinearDev;
use super::result::{DmResult, DmError, ErrorEnum};
use super::segment::Segment;
use super::shared::{DmDevice, device_create, device_exists, device_setup, table_reload};
use super::types::{DataBlocks, MetaBlocks, Sectors, TargetLine};

#[cfg(test)]
use std::path::Path;
#[cfg(test)]
use super::loopbacked::devnode_to_devno;

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
#[derive(Debug)]
pub struct ThinPoolDev {
    dev_info: Box<DeviceInfo>,
    meta_dev: LinearDev,
    data_dev: LinearDev,
    data_block_size: Sectors,
    low_water_mark: DataBlocks,
}

impl DmDevice for ThinPoolDev {
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
        self.data_dev.size()
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        self.data_dev.teardown(dm)?;
        self.meta_dev.teardown(dm)?;
        Ok(())
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
    /// Returns an error if the device is already known to the kernel.
    /// Precondition: the metadata device does not contain any pool metadata.
    pub fn new(dm: &DM,
               name: &DmName,
               uuid: Option<&DmUuid>,
               data_block_size: Sectors,
               low_water_mark: DataBlocks,
               meta: LinearDev,
               data: LinearDev)
               -> DmResult<ThinPoolDev> {
        if device_exists(dm, name)? {
            let err_msg = format!("thinpooldev {} already exists", name);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg.into()));
        }

        let table =
            ThinPoolDev::dm_table(data.size(), data_block_size, low_water_mark, &meta, &data);
        let dev_info = device_create(dm, name, uuid, &table)?;

        DM::wait_for_dm();
        Ok(ThinPoolDev {
               dev_info: Box::new(dev_info),
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

    /// Set up a thin pool from the given metadata and data device.
    /// Precondition: There is existing metadata for this thinpool device
    /// on the metadata device. If the metadata is corrupted, subsequent
    /// errors will result, so it is expected that the metadata is
    /// well-formed and consistent with the data on the data device.
    pub fn setup(dm: &DM,
                 name: &DmName,
                 uuid: Option<&DmUuid>,
                 data_block_size: Sectors,
                 low_water_mark: DataBlocks,
                 meta: LinearDev,
                 data: LinearDev)
                 -> DmResult<ThinPoolDev> {
        let table =
            ThinPoolDev::dm_table(data.size(), data_block_size, low_water_mark, &meta, &data);
        let dev_info = device_setup(dm, name, uuid, &table)?;

        DM::wait_for_dm();
        Ok(ThinPoolDev {
               dev_info: Box::new(dev_info),
               meta_dev: meta,
               data_dev: data,
               data_block_size: data_block_size,
               low_water_mark: low_water_mark,
           })
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
                             meta.device(),
                             data.device(),
                             *data_block_size,
                             *low_water_mark);
        vec![(Sectors::default(), length, "thin-pool".to_owned(), params)]
    }

    /// send a message to DM thin pool
    pub fn message(&self, dm: &DM, message: &str) -> DmResult<()> {
        dm.target_msg(&DevId::Name(self.name()), Sectors(0), message)?;
        Ok(())
    }

    /// Get the current status of the thinpool.
    /// Returns an error if there was an error getting the status value.
    /// Panics if there is an error parsing the status value.
    // Justification: see comment above DM::parse_table_status.
    pub fn status(&self, dm: &DM) -> DmResult<ThinPoolStatus> {
        let (_, mut status) = dm.table_status(&DevId::Name(self.name()), DmFlags::empty())?;

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
    pub fn extend_meta(&mut self, dm: &DM, new_segs: &[Segment]) -> DmResult<()> {
        self.meta_dev.extend(dm, new_segs)?;
        table_reload(dm,
                     &DevId::Name(self.name()),
                     &ThinPoolDev::dm_table(self.data_dev.size(),
                                            self.data_block_size,
                                            self.low_water_mark,
                                            &self.meta_dev,
                                            &self.data_dev))?;
        Ok(())
    }

    /// Extend an existing data device with additional new segments.
    pub fn extend_data(&mut self, dm: &DM, new_segs: &[Segment]) -> DmResult<()> {
        self.data_dev.extend(dm, new_segs)?;
        table_reload(dm,
                     &DevId::Name(self.name()),
                     &ThinPoolDev::dm_table(self.data_dev.size(),
                                            self.data_block_size,
                                            self.low_water_mark,
                                            &self.meta_dev,
                                            &self.data_dev))?;
        Ok(())
    }
}

#[cfg(test)]
pub fn minimal_thinpool(dm: &DM, path: &Path) -> ThinPoolDev {
    let dev = Device::from(devnode_to_devno(path).unwrap());
    let meta = LinearDev::setup(dm,
                                DmName::new("meta").expect("valid format"),
                                None,
                                &[Segment::new(dev, Sectors(0), MIN_RECOMMENDED_METADATA_SIZE)])
            .unwrap();

    let data =
        LinearDev::setup(dm,
                         DmName::new("data").expect("valid format"),
                         None,
                         &[Segment::new(dev, MIN_RECOMMENDED_METADATA_SIZE, MIN_DATA_BLOCK_SIZE)])
                .unwrap();

    ThinPoolDev::new(dm,
                     DmName::new("pool").expect("valid format"),
                     None,
                     MIN_DATA_BLOCK_SIZE,
                     DataBlocks(1),
                     meta,
                     data)
            .unwrap()
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::super::loopbacked::test_with_spec;

    use super::*;

    /// Verify success when constructing a new ThinPoolDev with minimum values
    /// for data block size and metadata device. Check that the status of the
    /// device is as expected.
    fn test_minimum_values(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);
        match tp.status(&dm).unwrap() {
            ThinPoolStatus::Good(ThinPoolWorkingStatus::Good, tpbu) => {
                // Even an empty thinpool requires some metadata.
                assert!(tpbu.used_meta > MetaBlocks(0));
                assert_eq!(tpbu.total_meta, tp.meta_dev().size().metablocks());
                assert_eq!(tpbu.used_data, DataBlocks(0));
                assert_eq!(tpbu.total_data,
                           DataBlocks(tp.data_dev().size() / tp.data_block_size()));
            }
            _ => assert!(false),
        }

        tp.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_basic() {
        test_with_spec(1, test_minimum_values);
    }
}
