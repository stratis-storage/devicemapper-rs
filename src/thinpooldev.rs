// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::lineardev::LinearDev;
use super::result::{DmResult, DmError, ErrorEnum};
use super::segment::Segment;
use super::shared::{DmDevice, device_create, device_exists, device_setup, table_reload};
use super::types::{DataBlocks, DevId, DmName, DmUuid, MetaBlocks, Sectors, TargetLine,
                   TargetParams, TargetTypeBuf};

#[cfg(test)]
use std::path::Path;
#[cfg(test)]
use super::loopbacked::devnode_to_devno;


struct ThinPoolDevStatusParams {
    pub transaction_id: u64,
    pub meta_usage: (MetaBlocks, MetaBlocks),
    pub data_usage: (DataBlocks, DataBlocks),
    pub summary: ThinPoolStatusSummary,
    pub discard_passdown: bool,
    pub no_space_policy: ThinPoolNoSpacePolicy,
    pub needs_check: bool,
}

impl ThinPoolDevStatusParams {
    pub fn new(transaction_id: u64,
               meta_usage: (MetaBlocks, MetaBlocks),
               data_usage: (DataBlocks, DataBlocks),
               summary: ThinPoolStatusSummary,
               discard_passdown: bool,
               no_space_policy: ThinPoolNoSpacePolicy,
               needs_check: bool)
               -> ThinPoolDevStatusParams {
        ThinPoolDevStatusParams {
            transaction_id: transaction_id,
            meta_usage: meta_usage,
            data_usage: data_usage,
            summary: summary,
            discard_passdown: discard_passdown,
            no_space_policy: no_space_policy,
            needs_check: needs_check,
        }
    }
}

impl FromStr for ThinPoolDevStatusParams {
    type Err = DmError;

    /// Note: Kernel docs show the ordering of the discard_passdown and the
    /// summary field opposite to the code below. But this code couldn't
    /// pass tests unless it were correct and the kernel docs wrong.
    fn from_str(s: &str) -> Result<ThinPoolDevStatusParams, DmError> {
        let vals = s.split(' ').collect::<Vec<_>>();
        if vals.len() < 8 {
            return Err(DmError::Dm(ErrorEnum::ParseError,
                                   format!("expected at least 8 values in \"{}\", found {}",
                                           s,
                                           vals.len())));
        }

        let transaction_id = vals[0]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse transaction id \"{}\": {}",
                                             vals[0],
                                             e))
                     })?;

        let meta_vals = vals[1].split('/').collect::<Vec<_>>();
        if meta_vals.len() != 2 {
            return Err(DmError::Dm(ErrorEnum::ParseError,
                                   format!("expected exactly 2 values in \"{}\", found {}",
                                           vals[1],
                                           meta_vals.len())));
        }

        let meta_vals = (MetaBlocks(meta_vals[0].parse::<u64>().map_err(|e| {
            DmError::Dm(ErrorEnum::ParseError,
                        format!("could not parse used metadata blocks \"{}\": {}",
                                meta_vals[0],
                                e))})?),
            MetaBlocks(meta_vals[1].parse::<u64>().map_err(|e| {
            DmError::Dm(ErrorEnum::ParseError,
                        format!("could not parse total metadata blocks \"{}\": {}",
                            meta_vals[1],
                        e))})?));

        let data_vals = vals[2].split('/').collect::<Vec<_>>();
        if data_vals.len() != 2 {
            return Err(DmError::Dm(ErrorEnum::ParseError,
                                   format!("expected exactly 2 values in \"{}\", found {}",
                                           vals[2],
                                           data_vals.len())));
        }

        let data_vals = (DataBlocks(data_vals[0].parse::<u64>().map_err(|e| {
            DmError::Dm(ErrorEnum::ParseError,
                        format!("could not parse used metadata blocks \"{}\": {}",
                                data_vals[0],
                                e))})?),
            DataBlocks(data_vals[1].parse::<u64>().map_err(|e| {
            DmError::Dm(ErrorEnum::ParseError,
                        format!("could not parse total metadata blocks \"{}\": {}",
                            data_vals[1],
                        e))})?));

        let summary = match vals[4] {
            "rw" => ThinPoolStatusSummary::Good,
            "ro" => ThinPoolStatusSummary::ReadOnly,
            "out_of_data_space" => ThinPoolStatusSummary::OutOfSpace,
            val => {
                return Err(DmError::Dm(ErrorEnum::ParseError,
                                       format!("unexpected value \"{}\" for summary", val)));
            }

        };

        let discard_passdown = match vals[5] {
            "discard_passdown" => true,
            "no_discard_passdown" => false,
            val => {
                return Err(DmError::Dm(ErrorEnum::ParseError,
                                       format!("unexpected value \"{}\" for discard_passdown",
                                               val)));
            }
        };

        let no_space_policy = match vals[6] {
            "error_if_no_space" => ThinPoolNoSpacePolicy::Error,
            "queue_if_no_space" => ThinPoolNoSpacePolicy::Queue,
            val => {
                return Err(DmError::Dm(ErrorEnum::ParseError,
                                       format!("unexpected value \"{}\" for no space policy",
                                               val)));
            }
        };

        let needs_check = match vals[7] {
            "-" => false,
            "needs_check" => true,
            val => {
                return Err(DmError::Dm(ErrorEnum::ParseError,
                                       format!("unexpected value \"{}\" for needs check", val)));
            }
        };

        Ok(ThinPoolDevStatusParams::new(transaction_id,
                                        meta_vals,
                                        data_vals,
                                        summary,
                                        discard_passdown,
                                        no_space_policy,
                                        needs_check))
    }
}

impl From<ThinPoolDevStatusParams> for ThinPoolWorkingStatus {
    fn from(params: ThinPoolDevStatusParams) -> ThinPoolWorkingStatus {
        let (used_meta, total_meta) = params.meta_usage;
        let (used_data, total_data) = params.data_usage;
        ThinPoolWorkingStatus::new(params.transaction_id,
                                   ThinPoolUsage {
                                       used_meta: used_meta,
                                       total_meta: total_meta,
                                       used_data: used_data,
                                       total_data: total_data,
                                   },
                                   params.discard_passdown,
                                   params.no_space_policy,
                                   params.summary,
                                   params.needs_check)
    }
}


/// ThinPoolDev target params
#[derive(Debug, PartialEq)]
pub struct ThinPoolDevTargetParams {
    /// the metadata device
    pub metadata_dev: Device,
    /// the data device
    pub data_dev: Device,
    /// data block size
    pub data_block_size: Sectors,
    /// data low water mark
    pub low_water_mark: DataBlocks,
    /// feature args
    pub feature_args: Vec<String>,
}

impl ThinPoolDevTargetParams {
    /// Make a new ThinPoolDevTargetParams struct
    pub fn new(metadata_dev: Device,
               data_dev: Device,
               data_block_size: Sectors,
               low_water_mark: DataBlocks)
               -> ThinPoolDevTargetParams {
        ThinPoolDevTargetParams {
            metadata_dev: metadata_dev,
            data_dev: data_dev,
            data_block_size: data_block_size,
            low_water_mark: low_water_mark,
            feature_args: vec!["skip_block_zeroing".to_owned()],
        }
    }
}

impl fmt::Display for ThinPoolDevTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let feature_args = if self.feature_args.is_empty() {
            "0".to_owned()
        } else {
            format!("{} {}",
                    self.feature_args.len(),
                    self.feature_args.join(" "))
        };

        write!(f,
               "{} {} {} {} {}",
               self.metadata_dev,
               self.data_dev,
               *self.data_block_size,
               *self.low_water_mark,
               feature_args)
    }
}

impl TargetParams for ThinPoolDevTargetParams {}


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
               meta: LinearDev,
               data: LinearDev,
               data_block_size: Sectors,
               low_water_mark: DataBlocks)
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
                 meta: LinearDev,
                 data: LinearDev,
                 data_block_size: Sectors,
                 low_water_mark: DataBlocks)
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
                -> Vec<TargetLine<ThinPoolDevTargetParams>> {
        vec![TargetLine {
                 start: Sectors::default(),
                 length: data.size(),
                 target_type: TargetTypeBuf::new("thin-pool".into()).expect("< length limit"),
                 params: ThinPoolDevTargetParams::new(meta.device(),
                                                      data.device(),
                                                      data_block_size,
                                                      low_water_mark),
             }]
    }

    /// Get the current status of the thinpool.
    pub fn status(&self, dm: &DM) -> DmResult<ThinPoolStatus> {
        let (_, table) = dm.table_status(&DevId::Name(self.name()), DmFlags::empty())?;

        if table.len() != 1 {
            return Err(DmError::Dm(ErrorEnum::ParseError,
                                   format!("expected exactly 1 line in table, found {}",
                                           table.len())));
        }

        let status_line = &table.first().expect("table.len() == 1").params;
        if status_line.starts_with("Fail") {
            return Ok(ThinPoolStatus::Fail);
        }

        let params = status_line.parse::<ThinPoolDevStatusParams>()?;
        Ok(ThinPoolStatus::Working(Box::new(params.into())))
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
                     meta,
                     data,
                     MIN_DATA_BLOCK_SIZE,
                     DataBlocks(1))
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
                                       meta,
                                       data,
                                       MIN_DATA_BLOCK_SIZE / 2u64,
                                       DataBlocks(1)) {
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
