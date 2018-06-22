// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::hash_set::HashSet;
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::DM;
use super::dm_options::DmOptions;
use super::lineardev::{LinearDev, LinearDevTargetParams};
use super::result::{DmError, DmResult, ErrorEnum};
use super::shared::{device_create, device_exists, device_match, parse_device, DmDevice,
                    TargetLine, TargetParams, TargetTable};
use super::types::{DataBlocks, DevId, DmName, DmUuid, MetaBlocks, Sectors, TargetTypeBuf};
use super::util::feature_args_to_string;

#[cfg(test)]
use super::device::devnode_to_devno;
#[cfg(test)]
use std::path::Path;

const THINPOOL_TARGET_NAME: &str = "thin-pool";

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ThinPoolTargetParams {
    pub metadata_dev: Device,
    pub data_dev: Device,
    pub data_block_size: Sectors,
    pub low_water_mark: DataBlocks,
    pub feature_args: HashSet<String>,
}

impl ThinPoolTargetParams {
    pub fn new(
        metadata_dev: Device,
        data_dev: Device,
        data_block_size: Sectors,
        low_water_mark: DataBlocks,
        feature_args: Vec<String>,
    ) -> ThinPoolTargetParams {
        ThinPoolTargetParams {
            metadata_dev,
            data_dev,
            data_block_size,
            low_water_mark,
            feature_args: feature_args.into_iter().collect::<HashSet<_>>(),
        }
    }
}

impl fmt::Display for ThinPoolTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", THINPOOL_TARGET_NAME, self.param_str())
    }
}

impl FromStr for ThinPoolTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<ThinPoolTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();

        if vals.len() < 6 {
            let err_msg = format!(
                "expected at least 6 values in params string \"{}\", found {}",
                s,
                vals.len()
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        if vals[0] != THINPOOL_TARGET_NAME {
            let err_msg = format!(
                "Expected a thin-pool target entry but found target type {}",
                vals[0]
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let metadata_dev = parse_device(vals[1])?;
        let data_dev = parse_device(vals[2])?;

        let data_block_size = vals[3].parse::<u64>().map(Sectors).map_err(|_| {
            DmError::Dm(
                ErrorEnum::Invalid,
                format!(
                    "failed to parse value for data block size from \"{}\"",
                    vals[3]
                ),
            )
        })?;

        let low_water_mark = vals[4].parse::<u64>().map(DataBlocks).map_err(|_| {
            DmError::Dm(
                ErrorEnum::Invalid,
                format!(
                    "failed to parse value for low water mark from \"{}\"",
                    vals[4]
                ),
            )
        })?;
        let num_feature_args = vals[5].parse::<usize>().map_err(|_| {
            DmError::Dm(
                ErrorEnum::Invalid,
                format!(
                    "failed to parse value for number of feature args from \"{}\"",
                    vals[5]
                ),
            )
        })?;

        let feature_args: Vec<String> = vals[6..6 + num_feature_args]
            .iter()
            .map(|x| x.to_string())
            .collect();

        Ok(ThinPoolTargetParams::new(
            metadata_dev,
            data_dev,
            data_block_size,
            low_water_mark,
            feature_args,
        ))
    }
}

impl TargetParams for ThinPoolTargetParams {
    fn param_str(&self) -> String {
        format!(
            "{} {} {} {} {}",
            self.metadata_dev,
            self.data_dev,
            *self.data_block_size,
            *self.low_water_mark,
            feature_args_to_string(&self.feature_args)
        )
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(THINPOOL_TARGET_NAME.into()).expect("THINPOOL_TARGET_NAME is valid")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ThinPoolDevTargetTable {
    pub table: TargetLine<ThinPoolTargetParams>,
}

impl ThinPoolDevTargetTable {
    pub fn new(
        start: Sectors,
        length: Sectors,
        params: ThinPoolTargetParams,
    ) -> ThinPoolDevTargetTable {
        ThinPoolDevTargetTable {
            table: TargetLine::new(start, length, params),
        }
    }
}

impl fmt::Display for ThinPoolDevTargetTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let table = &self.table;
        writeln!(f, "{} {} {}", *table.start, *table.length, table.params)
    }
}

impl TargetTable for ThinPoolDevTargetTable {
    fn from_raw_table(
        table: &[(Sectors, Sectors, TargetTypeBuf, String)],
    ) -> DmResult<ThinPoolDevTargetTable> {
        if table.len() != 1 {
            let err_msg = format!(
                "ThinPoolDev table should have exactly one line, has {} lines",
                table.len()
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }
        let line = table.first().expect("table.len() == 1");
        Ok(ThinPoolDevTargetTable::new(
            line.0,
            line.1,
            format!("{} {}", line.2.to_string(), line.3).parse::<ThinPoolTargetParams>()?,
        ))
    }

    fn to_raw_table(&self) -> Vec<(Sectors, Sectors, TargetTypeBuf, String)> {
        to_raw_table_unique!(self)
    }
}

/// DM construct to contain thin provisioned devices
#[derive(Debug)]
pub struct ThinPoolDev {
    dev_info: Box<DeviceInfo>,
    meta_dev: LinearDev,
    data_dev: LinearDev,
    table: ThinPoolDevTargetTable,
}

impl DmDevice<ThinPoolDevTargetTable> for ThinPoolDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    // This method is incomplete. It is expected that it will be refined so
    // that it will return true in more cases, i.e., to be less stringent.
    fn equivalent_tables(
        left: &ThinPoolDevTargetTable,
        right: &ThinPoolDevTargetTable,
    ) -> DmResult<bool> {
        Ok(left == right)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.data_dev.size()
    }

    fn table(&self) -> &ThinPoolDevTargetTable {
        table!(self)
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), &DmOptions::new())?;
        self.data_dev.teardown(dm)?;
        self.meta_dev.teardown(dm)?;
        Ok(())
    }

    fn uuid(&self) -> Option<&DmUuid> {
        uuid!(self)
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
    pub fn new(
        transaction_id: u64,
        usage: ThinPoolUsage,
        discard_passdown: bool,
        no_space_policy: ThinPoolNoSpacePolicy,
        summary: ThinPoolStatusSummary,
        needs_check: bool,
    ) -> ThinPoolWorkingStatus {
        ThinPoolWorkingStatus {
            transaction_id,
            usage,
            discard_passdown,
            no_space_policy,
            summary,
            needs_check,
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
    pub fn new(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        meta: LinearDev,
        data: LinearDev,
        data_block_size: Sectors,
        low_water_mark: DataBlocks,
    ) -> DmResult<ThinPoolDev> {
        if device_exists(dm, name)? {
            let err_msg = format!("thinpooldev {} already exists", name);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let table = ThinPoolDev::gen_default_table(&meta, &data, data_block_size, low_water_mark);
        let dev_info = device_create(dm, name, uuid, &table, &DmOptions::new())?;

        Ok(ThinPoolDev {
            dev_info: Box::new(dev_info),
            meta_dev: meta,
            data_dev: data,
            table,
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
        self.table.table.params.data_block_size
    }

    /// Set up a thin pool from the given metadata and data device.
    /// Returns an error if data_block_size is not within required range.
    /// Precondition: There is existing metadata for this thinpool device
    /// on the metadata device. If the metadata is corrupted, subsequent
    /// errors will result, so it is expected that the metadata is
    /// well-formed and consistent with the data on the data device.
    pub fn setup(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        meta: LinearDev,
        data: LinearDev,
        data_block_size: Sectors,
        low_water_mark: DataBlocks,
    ) -> DmResult<ThinPoolDev> {
        let table = ThinPoolDev::gen_default_table(&meta, &data, data_block_size, low_water_mark);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = ThinPoolDev {
                dev_info: Box::new(dev_info),
                meta_dev: meta,
                data_dev: data,
                table,
            };
            device_match(dm, &dev, uuid)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table, &DmOptions::new())?;
            ThinPoolDev {
                dev_info: Box::new(dev_info),
                meta_dev: meta,
                data_dev: data,
                table,
            }
        };
        Ok(dev)
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start sec (0)> <length> "thin-pool" <thin-pool-specific string>
    /// where the thin-pool-specific string has the format:
    /// <meta maj:min> <data maj:min> <block size> <low water mark>
    /// There is exactly one entry in the table.
    /// Various defaults are hard coded in the method.
    fn gen_default_table(
        meta: &LinearDev,
        data: &LinearDev,
        data_block_size: Sectors,
        low_water_mark: DataBlocks,
    ) -> ThinPoolDevTargetTable {
        ThinPoolDevTargetTable::new(
            Sectors::default(),
            data.size(),
            ThinPoolTargetParams::new(
                meta.device(),
                data.device(),
                data_block_size,
                low_water_mark,
                vec![
                    "skip_block_zeroing".to_owned(),
                    "error_if_no_space".to_owned(),
                ],
            ),
        )
    }

    /// Get the current status of the thinpool.
    /// Returns an error if there was an error getting the status value.
    /// Panics if there is an error parsing the status value.
    /// Note: Kernel docs show the ordering of the discard_passdown and the
    /// summary field opposite to the code below. But this code couldn't
    /// pass tests unless it were correct and the kernel docs wrong.
    // Justification: see comment above DM::parse_table_status.
    pub fn status(&self, dm: &DM) -> DmResult<ThinPoolStatus> {
        let (_, status) = dm.table_status(&DevId::Name(self.name()), &DmOptions::new())?;

        assert_eq!(
            status.len(),
            1,
            "Kernel must return 1 line from thin pool status"
        );

        let status_line = &status.first().expect("assertion above holds").3;
        if status_line.starts_with("Fail") {
            return Ok(ThinPoolStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(
            status_vals.len() >= 8,
            "Kernel must return at least 8 values from thin pool status"
        );

        let transaction_id = status_vals[0].parse::<u64>().expect("see justification");

        let usage = {
            let meta_vals = status_vals[1].split('/').collect::<Vec<_>>();
            let data_vals = status_vals[2].split('/').collect::<Vec<_>>();
            ThinPoolUsage {
                used_meta: MetaBlocks(
                    meta_vals[0]
                        .parse::<u64>()
                        .expect("used_meta value must be valid"),
                ),
                total_meta: MetaBlocks(
                    meta_vals[1]
                        .parse::<u64>()
                        .expect("total_meta value must be valid"),
                ),
                used_data: DataBlocks(
                    data_vals[0]
                        .parse::<u64>()
                        .expect("used_data value must be valid"),
                ),
                total_data: DataBlocks(
                    data_vals[1]
                        .parse::<u64>()
                        .expect("total_data value must be valid"),
                ),
            }
        };

        let summary = match status_vals[4] {
            "rw" => ThinPoolStatusSummary::Good,
            "ro" => ThinPoolStatusSummary::ReadOnly,
            "out_of_data_space" => ThinPoolStatusSummary::OutOfSpace,
            val => panic!(format!(
                "Kernel returned unexpected 5th value \"{}\" in thin pool status",
                val
            )),
        };

        let discard_passdown = match status_vals[5] {
            "discard_passdown" => true,
            "no_discard_passdown" => false,
            val => panic!(format!(
                "Kernel returned unexpected 6th value \"{}\" in thin pool status",
                val
            )),
        };

        let no_space_policy = match status_vals[6] {
            "error_if_no_space" => ThinPoolNoSpacePolicy::Error,
            "queue_if_no_space" => ThinPoolNoSpacePolicy::Queue,
            val => panic!(format!(
                "Kernel returned unexpected 7th value \"{}\" in thin pool status",
                val
            )),
        };

        let needs_check = match status_vals[7] {
            "-" => false,
            "needs_check" => true,
            val => panic!(format!(
                "Kernel returned unexpected 8th value \"{}\" in thin pool status",
                val
            )),
        };

        Ok(ThinPoolStatus::Working(Box::new(
            ThinPoolWorkingStatus::new(
                transaction_id,
                usage,
                discard_passdown,
                no_space_policy,
                summary,
                needs_check,
            ),
        )))
    }

    /// Set the table for the existing metadata device.
    /// This action puts the device in a state where it is ready to be resumed.
    /// Warning: It is the client's responsibility to make sure the designated
    /// table is compatible with the device's existing table.
    /// If are not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_meta_table(
        &mut self,
        dm: &DM,
        table: Vec<TargetLine<LinearDevTargetParams>>,
    ) -> DmResult<()> {
        self.suspend(dm, false)?;
        self.meta_dev.set_table(dm, table)?;
        self.meta_dev.resume(dm)?;

        // Reload the table even though it is unchanged.
        // See comment on CacheDev::set_cache_table for reason.
        self.table_load(dm, self.table())?;

        Ok(())
    }

    /// Set the data device's existing table.
    /// This action puts the device in a state where it is ready to be resumed.
    /// Warning: It is the client's responsibility to make sure the designated
    /// table is compatible with the device's existing table.
    /// If not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_data_table(
        &mut self,
        dm: &DM,
        table: Vec<TargetLine<LinearDevTargetParams>>,
    ) -> DmResult<()> {
        self.suspend(dm, false)?;

        self.data_dev.set_table(dm, table)?;
        self.data_dev.resume(dm)?;

        let mut table = self.table.clone();
        table.table.length = self.data_dev.size();
        self.table_load(dm, &table)?;

        self.table = table;

        Ok(())
    }
}

#[cfg(test)]
use super::consts::IEC;
#[cfg(test)]
use super::lineardev::LinearTargetParams;
#[cfg(test)]
use super::loopbacked::blkdev_size;
#[cfg(test)]
use super::test_lib::test_name;
#[cfg(test)]
use std::fs::OpenOptions;

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
/// Generate a minimal thinpool dev. Use all the space available not consumed
/// by the metadata device for the data device.
pub fn minimal_thinpool(dm: &DM, path: &Path) -> ThinPoolDev {
    let dev_size = blkdev_size(&OpenOptions::new().read(true).open(path).unwrap()).sectors();
    let dev = Device::from(devnode_to_devno(path).unwrap().unwrap());
    let meta_params = LinearTargetParams::new(dev, Sectors(0));
    let meta_table = vec![
        TargetLine::new(
            Sectors(0),
            MIN_RECOMMENDED_METADATA_SIZE,
            LinearDevTargetParams::Linear(meta_params),
        ),
    ];
    let meta = LinearDev::setup(
        dm,
        &test_name("meta").expect("valid format"),
        None,
        meta_table,
    ).unwrap();

    let data_params = LinearTargetParams::new(dev, MIN_RECOMMENDED_METADATA_SIZE);
    let data_table = vec![
        TargetLine::new(
            Sectors(0),
            dev_size - MIN_RECOMMENDED_METADATA_SIZE,
            LinearDevTargetParams::Linear(data_params),
        ),
    ];
    let data = LinearDev::setup(
        dm,
        &test_name("data").expect("valid format"),
        None,
        data_table,
    ).unwrap();

    ThinPoolDev::new(
        dm,
        &test_name("pool").expect("valid format"),
        None,
        meta,
        data,
        MIN_DATA_BLOCK_SIZE,
        DataBlocks(1),
    ).unwrap()
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
            ThinPoolStatus::Working(ref status)
                if status.summary == ThinPoolStatusSummary::Good =>
            {
                let usage = &status.usage;
                // Even an empty thinpool requires some metadata.
                assert!(usage.used_meta > MetaBlocks(0));
                assert_eq!(usage.total_meta, tp.meta_dev().size().metablocks());
                assert_eq!(usage.used_data, DataBlocks(0));
                assert_eq!(
                    usage.total_data,
                    DataBlocks(tp.data_dev().size() / tp.data_block_size())
                );
            }
            _ => assert!(false),
        }

        let table = ThinPoolDev::read_kernel_table(&dm, &DevId::Name(tp.name()))
            .unwrap()
            .table;
        let params = &table.params;
        assert_eq!(params.metadata_dev, tp.meta_dev().device());
        assert_eq!(params.data_dev, tp.data_dev().device());

        tp.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_basic() {
        test_with_spec(1, test_minimum_values);
    }

    /// Verify that data block size less than minimum results in a failure.
    fn test_low_data_block_size(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);
        let dev = Device::from(devnode_to_devno(paths[0]).unwrap().unwrap());

        let dm = DM::new().unwrap();

        let meta_name = test_name("meta").expect("valid format");
        let meta_params = LinearTargetParams::new(dev, Sectors(0));
        let meta_table = vec![
            TargetLine::new(
                Sectors(0),
                MIN_RECOMMENDED_METADATA_SIZE,
                LinearDevTargetParams::Linear(meta_params),
            ),
        ];
        let meta = LinearDev::setup(&dm, &meta_name, None, meta_table).unwrap();

        let data_name = test_name("data").expect("valid format");
        let data_params = LinearTargetParams::new(dev, MIN_RECOMMENDED_METADATA_SIZE);
        let data_table = vec![
            TargetLine::new(
                Sectors(0),
                512u64 * MIN_DATA_BLOCK_SIZE,
                LinearDevTargetParams::Linear(data_params),
            ),
        ];
        let data = LinearDev::setup(&dm, &data_name, None, data_table).unwrap();

        assert!(match ThinPoolDev::new(
            &dm,
            &test_name("pool").expect("valid format"),
            None,
            meta,
            data,
            MIN_DATA_BLOCK_SIZE / 2u64,
            DataBlocks(1)
        ) {
            Err(DmError::Core(Error(ErrorKind::IoctlError(_), _))) => true,
            _ => false,
        });
        dm.device_remove(&DevId::Name(&meta_name), &DmOptions::new())
            .unwrap();
        dm.device_remove(&DevId::Name(&data_name), &DmOptions::new())
            .unwrap();
    }

    #[test]
    fn loop_test_low_data_block_size() {
        test_with_spec(1, test_low_data_block_size);
    }

    /// Verify that setting the data table does not fail and results in
    /// the correct size data device.
    fn test_set_data(paths: &[&Path]) -> () {
        assert!(paths.len() > 1);

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);

        let mut data_table = tp.data_dev.table().table.clone();
        let data_size = tp.data_dev.size();

        let dev2 = Device::from(devnode_to_devno(paths[1]).unwrap().unwrap());
        let data_params = LinearTargetParams::new(dev2, Sectors(0));
        data_table.push(TargetLine::new(
            data_size,
            data_size,
            LinearDevTargetParams::Linear(data_params),
        ));
        tp.set_data_table(&dm, data_table).unwrap();
        tp.resume(&dm).unwrap();

        match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => {
                let usage = &status.usage;
                assert_eq!(
                    *usage.total_data * tp.table().table.params.data_block_size,
                    2u8 * data_size
                );
            }
            ThinPoolStatus::Fail => panic!("thin pool should not have failed"),
        }

        tp.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_set_data() {
        test_with_spec(2, test_set_data);
    }

    /// Verify that setting the meta table does not fail and results in
    /// the correct size meta device.
    fn test_set_meta(paths: &[&Path]) -> () {
        assert!(paths.len() > 1);

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);

        let mut meta_table = tp.meta_dev.table().table.clone();
        let meta_size = tp.meta_dev.size();

        let dev2 = Device::from(devnode_to_devno(paths[1]).unwrap().unwrap());
        let meta_params = LinearTargetParams::new(dev2, Sectors(0));
        meta_table.push(TargetLine::new(
            meta_size,
            meta_size,
            LinearDevTargetParams::Linear(meta_params),
        ));
        tp.set_meta_table(&dm, meta_table).unwrap();
        tp.resume(&dm).unwrap();

        match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => {
                let usage = &status.usage;
                assert_eq!(usage.total_meta.sectors(), 2u8 * meta_size);
            }
            ThinPoolStatus::Fail => panic!("thin pool should not have failed"),
        }

        tp.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_set_meta() {
        test_with_spec(2, test_set_meta);
    }

    /// Just test that suspending and resuming a thinpool has no errors.
    fn test_suspend(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);
        tp.suspend(&dm, false).unwrap();
        tp.suspend(&dm, false).unwrap();
        tp.resume(&dm).unwrap();
        tp.resume(&dm).unwrap();
        tp.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_suspend() {
        test_with_spec(1, test_suspend);
    }
}
