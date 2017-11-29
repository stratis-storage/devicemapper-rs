// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::path::PathBuf;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::lineardev::LinearDev;
use super::result::{DmResult, DmError, ErrorEnum};
use super::segment::Segment;
use super::shared::{DmDevice, device_create, device_exists, device_setup, table_reload};
use super::types::{DataBlocks, DevId, DmName, DmUuid, MetaBlocks, Sectors, TargetLine,
                   TargetTypeBuf};

#[cfg(test)]
use std::path::Path;
#[cfg(test)]
use super::loopbacked::devnode_to_devno;


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

#[derive(Debug, Clone)]
/// Contains values indicating the thinpool's used vs total
/// allocations for metadata and data blocks.
pub struct ThinPoolUsage {
    /// The number of metadata blocks that are in use.
    pub used_meta: MetaBlocks,
    /// The total number of metadata blocks available to the thinpool.
    pub total_meta: MetaBlocks,
    /// The number of data blocks that are in use.
    pub used_data: DataBlocks,
    /// The total number of data blocks available to the thinpool.
    pub total_data: DataBlocks,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Indicates if a working thinpool is working optimally, or is
/// experiencing a non-fatal error condition.
pub enum ThinPoolStatusSummary {
    /// The pool is working normally.
    Good,
    /// The pool has been forced to transition to read-only mode.
    ReadOnly,
    /// The pool is out of space.
    OutOfSpace,
}

/// Policy if no space on device
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ThinPoolNoSpacePolicy {
    /// error the IO if no space on device
    Error,
    /// queue the IO if no space on device
    Queue,
}

/// Status of a working thin pool, i.e, one that does not have status Fail
/// Note that this struct is incomplete. It does not contain every value
/// that can be parsed from a data line, as some of those values are of
/// unknown format.
#[derive(Debug, Clone)]
pub struct ThinPoolWorkingStatus {
    /// The transaction id.
    pub transaction_id: u64,
    /// A struct recording block usage for meta and data devices.
    pub usage: ThinPoolUsage,
    /// discard_passdown/no_discard_passdown
    pub discard_passdown: bool,
    /// no space policy
    pub no_space_policy: ThinPoolNoSpacePolicy,
    /// A summary of some other status information.
    pub summary: ThinPoolStatusSummary,
    /// needs_check flag has been set in metadata superblock
    pub needs_check: bool,
}

impl ThinPoolWorkingStatus {
    /// Make a new ThinPoolWorkingStatus struct
    pub fn new(transaction_id: u64,
               usage: ThinPoolUsage,
               discard_passdown: bool,
               no_space_policy: ThinPoolNoSpacePolicy,
               summary: ThinPoolStatusSummary,
               needs_check: bool)
               -> ThinPoolWorkingStatus {
        ThinPoolWorkingStatus {
            transaction_id: transaction_id,
            usage: usage,
            discard_passdown: discard_passdown,
            no_space_policy: no_space_policy,
            summary: summary,
            needs_check: needs_check,
        }

    }
}

#[derive(Debug, Clone)]
/// Top-level thinpool status that indicates if it is working or failed.
pub enum ThinPoolStatus {
    /// The thinpool has not failed utterly.
    Working(Box<ThinPoolWorkingStatus>),
    /// The thinpool is in a failed condition.
    Fail,
}


/// Use DM to create a "thin-pool".  A "thin-pool" is shared space for
/// other thin provisioned devices to use.
///
/// See section "Setting up a fresh pool device":
/// https://www.kernel.org/doc/Documentation/device-mapper/thin-provisioning.txt
impl ThinPoolDev {
    /// Construct a new ThinPoolDev with the given data and meta devs.
    /// Returns an error if the device is already known to the kernel.
    /// Returns an error if data_block_size is not within required range.
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
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let table = ThinPoolDev::dm_table(&meta, &data, data_block_size, low_water_mark);
        let dev_info = device_create(dm, name, uuid, &table)?;

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
    /// Returns an error if data_block_size is not within required range.
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
        let table = ThinPoolDev::dm_table(&meta, &data, data_block_size, low_water_mark);
        let dev_info = device_setup(dm, name, uuid, &table)?;

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
    fn dm_table(meta: &LinearDev,
                data: &LinearDev,
                data_block_size: Sectors,
                low_water_mark: DataBlocks)
                -> Vec<TargetLine> {
        let params = format!("{} {} {} {} 1 skip_block_zeroing",
                             meta.device(),
                             data.device(),
                             *data_block_size,
                             *low_water_mark);
        vec![TargetLine {
                 start: Sectors::default(),
                 length: data.size(),
                 target_type: TargetTypeBuf::new("thin-pool".into()).expect("< length limit"),
                 params: params,
             }]
    }

    /// Get the current status of the thinpool.
    /// Returns an error if there was an error getting the status value.
    /// Panics if there is an error parsing the status value.
    /// Note: Kernel docs show the ordering of the discard_passdown and the
    /// summary field opposite to the code below. But this code couldn't
    /// pass tests unless it were correct and the kernel docs wrong.
    // Justification: see comment above DM::parse_table_status.
    pub fn status(&self, dm: &DM) -> DmResult<ThinPoolStatus> {
        let (_, status) = dm.table_status(&DevId::Name(self.name()), DmFlags::empty())?;

        assert_eq!(status.len(),
                   1,
                   "Kernel must return 1 line from thin pool status");

        let status_line = &status.get(0).expect("assertion above holds").params;
        if status_line.starts_with("Fail") {
            return Ok(ThinPoolStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(status_vals.len() >= 8,
                "Kernel must return at least 8 values from thin pool status");

        let transaction_id = status_vals[0].parse::<u64>().expect("see justification");

        let usage = {
            let meta_vals = status_vals[1].split('/').collect::<Vec<_>>();
            let data_vals = status_vals[2].split('/').collect::<Vec<_>>();
            ThinPoolUsage {
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

        let summary = match status_vals[4] {
            "rw" => ThinPoolStatusSummary::Good,
            "ro" => ThinPoolStatusSummary::ReadOnly,
            "out_of_data_space" => ThinPoolStatusSummary::OutOfSpace,
            val => {
                panic!(format!("Kernel returned unexpected 5th value \"{}\" in thin pool status",
                               val))
            }

        };

        let discard_passdown = match status_vals[5] {
            "discard_passdown" => true,
            "no_discard_passdown" => false,
            val => {
                panic!(format!("Kernel returned unexpected 6th value \"{}\" in thin pool status",
                               val))
            }
        };

        let no_space_policy = match status_vals[6] {
            "error_if_no_space" => ThinPoolNoSpacePolicy::Error,
            "queue_if_no_space" => ThinPoolNoSpacePolicy::Queue,
            val => {
                panic!(format!("Kernel returned unexpected 7th value \"{}\" in thin pool status",
                               val))
            }
        };

        let needs_check = match status_vals[7] {
            "-" => false,
            "needs_check" => true,
            val => {
                panic!(format!("Kernel returned unexpected 8th value \"{}\" in thin pool status",
                               val))
            }
        };

        Ok(ThinPoolStatus::Working(Box::new(ThinPoolWorkingStatus::new(transaction_id,
                                                                       usage,
                                                                       discard_passdown,
                                                                       no_space_policy,
                                                                       summary,
                                                                       needs_check))))
    }

    /// Set the segments for the existing metadata device.
    /// Warning: It is the client's responsibility to make sure the designated
    /// segments are compatible with the device's existing segments.
    /// If they are not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_meta_segments(&mut self, dm: &DM, segments: &[Segment]) -> DmResult<()> {
        self.meta_dev.set_segments(dm, segments)?;
        table_reload(dm,
                     &DevId::Name(self.name()),
                     &ThinPoolDev::dm_table(&self.meta_dev,
                                            &self.data_dev,
                                            self.data_block_size,
                                            self.low_water_mark))?;
        Ok(())
    }

    /// Set the data device's existing segments.
    /// Warning: It is the client's responsibility to make sure the designated
    /// segments are compatible with the device's existing segments.
    /// If they are not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_data_segments(&mut self, dm: &DM, segments: &[Segment]) -> DmResult<()> {
        self.data_dev.set_segments(dm, segments)?;
        table_reload(dm,
                     &DevId::Name(self.name()),
                     &ThinPoolDev::dm_table(&self.meta_dev,
                                            &self.data_dev,
                                            self.data_block_size,
                                            self.low_water_mark))?;
        Ok(())
    }
}

#[cfg(test)]
use super::consts::IEC;

/// Values are explicitly stated in the device-mapper kernel documentation.
#[cfg(test)]
const MIN_DATA_BLOCK_SIZE: Sectors = Sectors(128); // 64 KiB
#[cfg(test)]
#[allow(dead_code)]
const MAX_DATA_BLOCK_SIZE: Sectors = Sectors(2 * IEC::Mi); // 1 GiB
#[cfg(test)]
const MIN_RECOMMENDED_METADATA_SIZE: Sectors = Sectors(4 * IEC::Ki); // 2 MiB
#[cfg(test)]
#[allow(dead_code)]
const MAX_RECOMMENDED_METADATA_SIZE: Sectors = Sectors(32 * IEC::Mi); // 16 GiB

#[cfg(test)]
pub fn minimal_thinpool(dm: &DM, path: &Path) -> ThinPoolDev {
    let dev = Device::from(devnode_to_devno(path).unwrap());
    let meta = LinearDev::setup(dm,
                                DmName::new("meta").expect("valid format"),
                                None,
                                &[Segment::new(dev, Sectors(0), MIN_RECOMMENDED_METADATA_SIZE)])
            .unwrap();

    // 512 * MIN_DATA_BLOCK_SIZE (32 MiB) should be enough for an xfs filesystem
    let data = LinearDev::setup(dm,
                                DmName::new("data").expect("valid format"),
                                None,
                                &[Segment::new(dev,
                                               MIN_RECOMMENDED_METADATA_SIZE,
                                               512u64 * MIN_DATA_BLOCK_SIZE)])
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

    use super::super::errors::{Error, ErrorKind};
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
            ThinPoolStatus::Working(ref status) if status.summary ==
                                                   ThinPoolStatusSummary::Good => {
                let usage = &status.usage;
                // Even an empty thinpool requires some metadata.
                assert!(usage.used_meta > MetaBlocks(0));
                assert_eq!(usage.total_meta, tp.meta_dev().size().metablocks());
                assert_eq!(usage.used_data, DataBlocks(0));
                assert_eq!(usage.total_data,
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

    /// Verify that data block size less than minimum results in a failure.
    fn test_low_data_block_size(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);
        let dev = Device::from(devnode_to_devno(paths[0]).unwrap());

        let dm = DM::new().unwrap();

        let meta_name = DmName::new("meta").expect("valid format");
        let meta =
            LinearDev::setup(&dm,
                             meta_name,
                             None,
                             &[Segment::new(dev, Sectors(0), MIN_RECOMMENDED_METADATA_SIZE)])
                    .unwrap();

        let data_name = DmName::new("data").expect("valid format");
        let data = LinearDev::setup(&dm,
                                    data_name,
                                    None,
                                    &[Segment::new(dev,
                                                   MIN_RECOMMENDED_METADATA_SIZE,
                                                   512u64 * MIN_DATA_BLOCK_SIZE)])
                .unwrap();

        assert!(match ThinPoolDev::new(&dm,
                                       DmName::new("pool").expect("valid format"),
                                       None,
                                       MIN_DATA_BLOCK_SIZE / 2u64,
                                       DataBlocks(1),
                                       meta,
                                       data) {
                    Err(DmError::Core(Error(ErrorKind::IoctlError(_), _))) => true,
                    _ => false,
                });

        dm.device_remove(&DevId::Name(meta_name), DmFlags::empty())
            .unwrap();
        dm.device_remove(&DevId::Name(data_name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    fn loop_test_low_data_block_size() {
        test_with_spec(1, test_low_data_block_size);
    }
}
