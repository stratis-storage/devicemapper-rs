// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::lineardev::{LinearDev, LinearDevTargetParams};
use super::result::{DmError, DmResult, ErrorEnum};
use super::shared::{DmDevice, TargetLine, TargetParams, TargetTable, device_create, device_exists,
                    device_match, parse_device, table_reload};
use super::types::{DataBlocks, DevId, DmName, DmUuid, MetaBlocks, Sectors, TargetTypeBuf};


const CACHE_TARGET_NAME: &str = "cache";

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CacheTargetParams {
    pub meta: Device,
    pub cache: Device,
    pub origin: Device,
    pub cache_block_size: Sectors,
    pub feature_args: HashSet<String>,
    pub policy: String,
    pub policy_args: HashMap<String, String>,
}

impl CacheTargetParams {
    pub fn new(meta: Device,
               cache: Device,
               origin: Device,
               cache_block_size: Sectors,
               feature_args: Vec<String>,
               policy: String,
               policy_args: Vec<(String, String)>)
               -> CacheTargetParams {
        CacheTargetParams {
            meta,
            cache,
            origin,
            cache_block_size,
            feature_args: feature_args.into_iter().collect::<HashSet<_>>(),
            policy,
            policy_args: policy_args.into_iter().collect::<HashMap<_, _>>(),
        }
    }
}

impl fmt::Display for CacheTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", CACHE_TARGET_NAME, self.param_str())
    }
}

impl FromStr for CacheTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<CacheTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();

        if vals.len() < 8 {
            let err_msg = format!("expected at least 8 values in params string \"{}\", found {}",
                                  s,
                                  vals.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        if vals[0] != CACHE_TARGET_NAME {
            let err_msg = format!("Expected a cache target entry but found target type {}",
                                  vals[0]);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let metadata_dev = parse_device(vals[1])?;
        let cache_dev = parse_device(vals[2])?;
        let origin_dev = parse_device(vals[3])?;

        let block_size = vals[4]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for data block size from \"{}\"",
                                    vals[4]))})?;

        let num_feature_args = vals[5]
            .parse::<usize>()
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for number of feature args from \"{}\"",
                                    vals[5]))})?;

        let end_feature_args_index = 6 + num_feature_args;
        let feature_args: Vec<String> = vals[6..end_feature_args_index]
            .iter()
            .map(|x| x.to_string())
            .collect();

        let policy = vals[end_feature_args_index].to_owned();

        let num_policy_args = vals[end_feature_args_index + 1]
            .parse::<usize>()
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for number of policy args from \"{}\"",
                                    vals[end_feature_args_index + 1]))})?;

        let start_policy_args_index = end_feature_args_index + 2;
        let end_policy_args_index = start_policy_args_index + num_policy_args;
        let policy_args: Vec<(String, String)> = vals[start_policy_args_index..
        end_policy_args_index]
                .chunks(2)
                .map(|x| (x[0].to_string(), x[1].to_string()))
                .collect();

        Ok(CacheTargetParams::new(metadata_dev,
                                  cache_dev,
                                  origin_dev,
                                  block_size,
                                  feature_args,
                                  policy,
                                  policy_args))

    }
}

impl TargetParams for CacheTargetParams {
    fn param_str(&self) -> String {
        let feature_args = if self.feature_args.is_empty() {
            "0".to_owned()
        } else {
            format!("{} {}",
                    self.feature_args.len(),
                    self.feature_args
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>()
                        .join(" "))
        };

        let policy_args = if self.policy_args.is_empty() {
            "0".to_owned()
        } else {
            format!("{} {}",
                    self.policy_args.len(),
                    self.policy_args
                        .iter()
                        .map(|(k, v)| format!("{} {}", k, v))
                        .collect::<Vec<String>>()
                        .join(" "))
        };

        format!("{} {} {} {} {} {} {}",
                self.meta,
                self.cache,
                self.origin,
                *self.cache_block_size,
                feature_args,
                self.policy,
                policy_args)
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(CACHE_TARGET_NAME.into()).expect("CACHE_TARGET_NAME is valid")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CacheDevTargetTable {
    pub table: TargetLine<CacheTargetParams>,
}

impl CacheDevTargetTable {
    pub fn new(start: Sectors, length: Sectors, params: CacheTargetParams) -> CacheDevTargetTable {
        CacheDevTargetTable { table: TargetLine::new(start, length, params) }
    }
}

impl TargetTable for CacheDevTargetTable {
    fn from_raw_table(table: &[(Sectors, Sectors, TargetTypeBuf, String)])
                      -> DmResult<CacheDevTargetTable> {
        if table.len() != 1 {
            let err_msg = format!("CacheDev table should have exactly one line, has {} lines",
                                  table.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }
        let line = table.first().expect("table.len() == 1");
        Ok(CacheDevTargetTable::new(line.0,
                                    line.1,
                                    format!("{} {}", line.2.to_string(), line.3)
                                        .parse::<CacheTargetParams>()?))
    }

    fn to_raw_table(&self) -> Vec<(Sectors, Sectors, TargetTypeBuf, String)> {
        to_raw_table_unique!(self)
    }
}


/// Cache usage
#[derive(Debug)]
pub struct CacheDevUsage {
    /// The metadata block size, should always be equal to META_BLOCK_SIZE.
    /// At time of writing, all metadata blocks have the same size.
    pub meta_block_size: Sectors,
    /// The number of metadata blocks in use
    pub used_meta: MetaBlocks,
    /// The number of metadata blocks available
    pub total_meta: MetaBlocks,
    /// The cache block size
    pub cache_block_size: Sectors,
    /// Used cache blocks
    pub used_cache: DataBlocks,
    /// Total cache blocks
    pub total_cache: DataBlocks,
}

impl CacheDevUsage {
    /// Make a new CacheDevUsage struct
    pub fn new(meta_block_size: Sectors,
               used_meta: MetaBlocks,
               total_meta: MetaBlocks,
               cache_block_size: Sectors,
               used_cache: DataBlocks,
               total_cache: DataBlocks)
               -> CacheDevUsage {
        // This is defined at the kernel level and should not change.
        assert_eq!(meta_block_size, Sectors(8));
        CacheDevUsage {
            meta_block_size,
            used_meta,
            total_meta,
            cache_block_size,
            used_cache,
            total_cache,
        }
    }
}

/// Cache dev performance data
#[derive(Debug)]
pub struct CacheDevPerformance {
    /// Number of read hits
    pub read_hits: u64,
    /// Number of read misses
    pub read_misses: u64,
    /// Number of write hits
    pub write_hits: u64,
    /// Number of write misses
    pub write_misses: u64,
    /// Number of demotions
    pub demotions: u64,
    /// Number of promotions
    pub promotions: u64,
    /// Number of dirty blocks
    pub dirty: u64,
}

impl CacheDevPerformance {
    /// Construct a new CacheDevPerformance struct
    pub fn new(read_hits: u64,
               read_misses: u64,
               write_hits: u64,
               write_misses: u64,
               demotions: u64,
               promotions: u64,
               dirty: u64)
               -> CacheDevPerformance {
        CacheDevPerformance {
            read_hits,
            read_misses,
            write_hits,
            write_misses,
            demotions,
            promotions,
            dirty,
        }
    }
}

/// The cache metadata mode
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CacheDevMetadataMode {
    /// The cache is working normally.
    Good,
    /// The cache has been forced to transition to read-only mode.
    ReadOnly,
}

/// Status values of a cache device when it is working
#[derive(Debug)]
pub struct CacheDevWorkingStatus {
    /// A struct recording block usage for all devices
    pub usage: CacheDevUsage,
    /// A struct recording cache dev performance
    pub performance: CacheDevPerformance,
    /// The feature args
    pub feature_args: Vec<String>,
    /// The core args
    pub core_args: Vec<(String, String)>,
    /// The name of the replacement policy to use
    /// User-defined policies are permitted.
    pub policy: String,
    /// Arguments for the designated policy
    pub policy_args: Vec<(String, String)>,
    /// cache metadata mode
    pub metadata_mode: CacheDevMetadataMode,
    /// needs_check flag has been set in metadata superblock
    pub needs_check: bool,
}

impl CacheDevWorkingStatus {
    /// Make a new CacheDevWorkingStatus struct
    #[allow(too_many_arguments)]
    pub fn new(usage: CacheDevUsage,
               performance: CacheDevPerformance,
               feature_args: Vec<String>,
               core_args: Vec<(String, String)>,
               policy: String,
               policy_args: Vec<(String, String)>,
               metadata_mode: CacheDevMetadataMode,
               needs_check: bool)
               -> CacheDevWorkingStatus {
        CacheDevWorkingStatus {
            usage,
            performance,
            feature_args,
            core_args,
            policy,
            policy_args,
            metadata_mode,
            needs_check,
        }
    }
}

/// Return type of CacheDev::status()
#[derive(Debug)]
pub enum CacheDevStatus {
    /// The cache has not failed utterly
    Working(Box<CacheDevWorkingStatus>),
    /// The cache is in a failed condition
    Fail,
}


/// DM Cache device
#[derive(Debug)]
pub struct CacheDev {
    dev_info: Box<DeviceInfo>,
    meta_dev: LinearDev,
    cache_dev: LinearDev,
    origin_dev: LinearDev,
    table: CacheDevTargetTable,
}

impl DmDevice<CacheDevTargetTable> for CacheDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    // Omit replacement policy field from equality test when checking that
    // two devices are the same. Equality of replacement policies is not a
    // necessary requirement for equality of devices as the replacement
    // policy can be changed dynamically by a reload of of the device's table.
    // It is convenient that this is the case, because checking equality of
    // replacement policies is somewhat hard. "default", which is a valid
    // policy string, is not a particular policy, but an alias for the default
    // policy for this version of devicemapper. Therefore, using string
    // equality to check equivalence can result in false negatives, as
    // "default" != "smq", the current default policy in the recent kernel.
    // Note: There is the possibility of implementing the following somewhat
    // complicated check. Without loss of generality, let
    // left[0].params.policy = "default" and
    // right[0].params.policy = X, where X != "default". Then, if X is the
    // default policy, return true, otherwise return false. Unfortunately,
    // there is no straightforward programmatic way of determining the default
    // policy for a given kernel, and we are assured that the default policy
    // can vary between kernels, and may of course, change in future.
    fn equivalent_tables(left: &CacheDevTargetTable,
                         right: &CacheDevTargetTable)
                         -> DmResult<bool> {
        let left = &left.table;
        let right = &right.table;

        Ok(left.start == right.start && left.length == right.length &&
           left.params.meta == right.params.meta &&
           left.params.origin == right.params.origin &&
           left.params.cache_block_size == right.params.cache_block_size &&
           left.params.feature_args == right.params.feature_args &&
           left.params.policy_args == right.params.policy_args)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.origin_dev.size()
    }

    fn table(&self) -> &CacheDevTargetTable {
        table!(self)
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        self.cache_dev.teardown(dm)?;
        self.origin_dev.teardown(dm)?;
        self.meta_dev.teardown(dm)?;
        Ok(())
    }

    fn uuid(&self) -> Option<&DmUuid> {
        uuid!(self)
    }
}


/// Cache device implementation.
impl CacheDev {
    /// Construct a new CacheDev with the given data and meta devs.
    /// Returns an error if the device is already known to the kernel.
    pub fn new(dm: &DM,
               name: &DmName,
               uuid: Option<&DmUuid>,
               meta: LinearDev,
               cache: LinearDev,
               origin: LinearDev,
               cache_block_size: Sectors)
               -> DmResult<CacheDev> {
        if device_exists(dm, name)? {
            let err_msg = format!("cachedev {} already exists", name);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let table = CacheDev::gen_default_table(&meta, &cache, &origin, cache_block_size);
        let dev_info = device_create(dm, name, uuid, &table)?;

        Ok(CacheDev {
               dev_info: Box::new(dev_info),
               meta_dev: meta,
               cache_dev: cache,
               origin_dev: origin,
               table,
           })
    }

    /// Set up a cache device from the given metadata and data devices.
    pub fn setup(dm: &DM,
                 name: &DmName,
                 uuid: Option<&DmUuid>,
                 meta: LinearDev,
                 cache: LinearDev,
                 origin: LinearDev,
                 cache_block_size: Sectors)
                 -> DmResult<CacheDev> {
        let table = CacheDev::gen_default_table(&meta, &cache, &origin, cache_block_size);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = CacheDev {
                dev_info: Box::new(dev_info),
                meta_dev: meta,
                cache_dev: cache,
                origin_dev: origin,
                table,
            };
            device_match(dm, &dev, uuid)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table)?;
            CacheDev {
                dev_info: Box::new(dev_info),
                meta_dev: meta,
                cache_dev: cache,
                origin_dev: origin,
                table,
            }
        };

        Ok(dev)
    }

    /// Set the table for the existing origin device.
    /// Warning: It is the client's responsibility to make sure the designated
    /// table is compatible with the device's existing table.
    /// If not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_origin_table(&mut self,
                            dm: &DM,
                            table: Vec<TargetLine<LinearDevTargetParams>>)
                            -> DmResult<()> {
        // Follow resize_origin example in device-mapper-test-suite file
        // cache_stack.rb. This looks funky, but it does seem to be exactly
        // what the test is doing.
        self.suspend(dm)?;

        self.origin_dev.suspend(dm)?;
        self.origin_dev.set_table(dm, table)?;
        self.origin_dev.resume(dm)?;

        let mut table = self.table.clone();
        table.table.length = self.origin_dev.size();
        table_reload(dm, &DevId::Name(self.name()), &table)?;

        self.resume(dm)?;

        self.table = table;

        Ok(())
    }

    /// Set the table for the existing cache sub-device.
    /// Warning: It is the client's responsibility to make sure the designated
    /// table is compatible with the device's existing table.
    /// If not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_cache_table(&mut self,
                           dm: &DM,
                           table: Vec<TargetLine<LinearDevTargetParams>>)
                           -> DmResult<()> {
        // Follow resize_ssd example in device-mapper-test-suite file
        // cache_stack.rb. This looks funky, but it does seem to be exactly
        // what the test is doing.
        self.suspend(dm)?;

        self.cache_dev.suspend(dm)?;
        self.cache_dev.set_table(dm, table)?;
        self.cache_dev.resume(dm)?;

        self.resume(dm)?;

        Ok(())
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start sec (0)> <length> "cache" <cache-specific string>
    /// where the cache-specific string has the format:
    /// <meta maj:min> <cache maj:min> <origin maj:min> <block size>
    /// <#num feature args (1)> writethrough <replacement policy (default)>
    /// <#num policy args (0)>
    /// There is exactly one entry in the table.
    /// Various defaults are hard coded in the method.
    fn gen_default_table(meta: &LinearDev,
                         cache: &LinearDev,
                         origin: &LinearDev,
                         cache_block_size: Sectors)
                         -> CacheDevTargetTable {
        CacheDevTargetTable::new(Sectors::default(),
                                 origin.size(),
                                 CacheTargetParams::new(meta.device(),
                                                        cache.device(),
                                                        origin.device(),
                                                        cache_block_size,
                                                        vec!["writethrough".into()],
                                                        "default".to_owned(),
                                                        vec![]))
    }

    /// Parse pairs of arguments from a slice
    /// Use the same policy as status() method in asserting
    fn parse_pairs(start_index: usize, vals: &[&str]) -> (usize, Vec<(String, String)>) {
        let num_pairs = vals[start_index]
            .parse::<usize>()
            .expect("number value must be valid format");
        if num_pairs % 2 != 0 {
            panic!(format!("Number of args \"{}\" is not even", num_pairs));
        }
        let next_start_index = start_index + num_pairs + 1;
        (next_start_index,
         vals[start_index + 1..next_start_index]
             .chunks(2)
             .map(|p| (p[0].to_string(), p[1].to_string()))
             .collect())
    }

    /// Get the current status of the cache device.
    // Note: This method is not entirely complete. In particular, *_args values
    // may require more or better checking or processing.
    pub fn status(&self, dm: &DM) -> DmResult<CacheDevStatus> {
        let (_, status) = dm.table_status(&DevId::Name(self.name()), DmFlags::empty())?;

        assert_eq!(status.len(),
                   1,
                   "Kernel must return 1 line from cache dev status");

        let status_line = &status.first().expect("assertion above holds").3;
        if status_line.starts_with("Fail") {
            return Ok(CacheDevStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(status_vals.len() >= 17,
                "Kernel must return at least 17 values from cache dev status");


        let usage = {
            let meta_block_size = status_vals[0];
            let meta_usage = status_vals[1].split('/').collect::<Vec<_>>();
            let cache_block_size = status_vals[2];
            let cache_usage = status_vals[3].split('/').collect::<Vec<_>>();
            CacheDevUsage::new(Sectors(meta_block_size
                                           .parse::<u64>()
                                           .expect("meta_block_size value must be valid")),
                               MetaBlocks(meta_usage[0]
                                              .parse::<u64>()
                                              .expect("used_meta value must be valid")),
                               MetaBlocks(meta_usage[1]
                                              .parse::<u64>()
                                              .expect("total_meta value must be valid")),
                               Sectors(cache_block_size
                                           .parse::<u64>()
                                           .expect("cache_block_size value must be valid")),
                               DataBlocks(cache_usage[0]
                                              .parse::<u64>()
                                              .expect("used_cache value must be valid")),
                               DataBlocks(cache_usage[1]
                                              .parse::<u64>()
                                              .expect("total_cache value must be valid")))
        };

        let performance =
            CacheDevPerformance::new(status_vals[4]
                                         .parse::<u64>()
                                         .expect("read hits value must be valid format"),
                                     status_vals[5]
                                         .parse::<u64>()
                                         .expect("read misses value must be valid format"),
                                     status_vals[6]
                                         .parse::<u64>()
                                         .expect("write hits value must be valid format"),
                                     status_vals[7]
                                         .parse::<u64>()
                                         .expect("write misses value must be valid format"),
                                     status_vals[8]
                                         .parse::<u64>()
                                         .expect("demotions value must be valid format"),
                                     status_vals[9]
                                         .parse::<u64>()
                                         .expect("promotions value must be valid format"),
                                     status_vals[10]
                                         .parse::<u64>()
                                         .expect("dirty value must be valid format"));

        let num_feature_args = status_vals[11]
            .parse::<usize>()
            .expect("number value must be valid format");
        let core_args_start_index = 12usize + num_feature_args;
        let feature_args: Vec<String> = status_vals[12..core_args_start_index]
            .iter()
            .map(|x| x.to_string())
            .collect();

        let (policy_start_index, core_args) = CacheDev::parse_pairs(core_args_start_index,
                                                                    &status_vals);

        let policy = status_vals[policy_start_index].to_string();
        let (rest_start_index, policy_args) = CacheDev::parse_pairs(policy_start_index + 1,
                                                                    &status_vals);

        let cache_metadata_mode = match status_vals[rest_start_index] {
            "rw" => CacheDevMetadataMode::Good,
            "ro" => CacheDevMetadataMode::ReadOnly,
            val => {
                panic!(format!("Kernel returned unexpected {}th value \"{}\" in thin pool status",
                               rest_start_index + 1,
                               val))
            }
        };

        let needs_check = match status_vals[rest_start_index + 1] {
            "-" => false,
            "needs_check" => true,
            val => {
                panic!(format!("Kernel returned unexpected {}th value \"{}\" in thin pool status",
                               rest_start_index + 2,
                               val))
            }
        };

        Ok(CacheDevStatus::Working(Box::new(CacheDevWorkingStatus::new(usage,
                                                                       performance,
                                                                       feature_args,
                                                                       core_args,
                                                                       policy,
                                                                       policy_args,
                                                                       cache_metadata_mode,
                                                                       needs_check))))
    }
}


#[cfg(test)]
use std::fs::OpenOptions;
#[cfg(test)]
use std::path::Path;

#[cfg(test)]
use super::consts::IEC;
#[cfg(test)]
use super::device::devnode_to_devno;
#[cfg(test)]
use super::lineardev::LinearTargetParams;
#[cfg(test)]
use super::loopbacked::blkdev_size;

// Specified in kernel docs
/// The minimum size recommended in the docs for a cache block.
pub const MIN_CACHE_BLOCK_SIZE: Sectors = Sectors(64); // 32 KiB
/// The maximum size recommended in the docs for a cache block.
#[allow(decimal_literal_representation)]
pub const MAX_CACHE_BLOCK_SIZE: Sectors = Sectors(2_097_152); // 1 GiB

#[cfg(test)]
// Make a minimal cachedev. Put the meta and cache on one device, and put
// the origin on a separate device. paths.len() must be at least 2 or the
// method will fail.
pub fn minimal_cachedev(dm: &DM, paths: &[&Path]) -> CacheDev {
    assert!(paths.len() >= 2);
    let dev1 = Device::from(devnode_to_devno(paths[0]).unwrap().unwrap());

    let meta_name = DmName::new("cache-meta").expect("valid format");

    // Minimum recommended metadata size for thinpool
    let meta_length = Sectors(4 * IEC::Ki);
    let meta_params = LinearTargetParams::new(dev1, Sectors(0));
    let meta_table = vec![TargetLine::new(Sectors(0),
                                          meta_length,
                                          LinearDevTargetParams::Linear(meta_params))];
    let meta = LinearDev::setup(&dm, meta_name, None, meta_table).unwrap();

    let cache_name = DmName::new("cache-cache").expect("valid format");
    let cache_offset = meta_length;
    let cache_length = MIN_CACHE_BLOCK_SIZE;
    let cache_params = LinearTargetParams::new(dev1, cache_offset);
    let cache_table = vec![TargetLine::new(Sectors(0),
                                           cache_length,
                                           LinearDevTargetParams::Linear(cache_params))];
    let cache = LinearDev::setup(&dm, cache_name, None, cache_table).unwrap();

    let dev2_size = blkdev_size(&OpenOptions::new().read(true).open(paths[1]).unwrap()).sectors();
    let dev2 = Device::from(devnode_to_devno(paths[1]).unwrap().unwrap());

    let origin_name = DmName::new("cache-origin").expect("valid format");
    let origin_params = LinearTargetParams::new(dev2, Sectors(0));
    let origin_table = vec![TargetLine::new(Sectors(0),
                                            dev2_size,
                                            LinearDevTargetParams::Linear(origin_params))];
    let origin = LinearDev::setup(&dm, origin_name, None, origin_table).unwrap();

    CacheDev::new(&dm,
                  DmName::new("cache").expect("valid format"),
                  None,
                  meta,
                  cache,
                  origin,
                  MIN_CACHE_BLOCK_SIZE)
            .unwrap()
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::super::loopbacked::test_with_spec;

    use super::*;


    // Test creating a minimal cache dev.
    // Verify that status method executes and gives reasonable values.
    fn test_minimal_cache_dev(paths: &[&Path]) -> () {
        assert!(paths.len() >= 2);
        let dm = DM::new().unwrap();
        let cache = minimal_cachedev(&dm, paths);

        match cache.status(&dm).unwrap() {
            CacheDevStatus::Working(ref status) => {
                let usage = &status.usage;

                assert_eq!(usage.meta_block_size, Sectors(8));

                // Even an empty cache dev takes up some metadata space.
                assert!(usage.used_meta > MetaBlocks(0));

                assert_eq!(usage.cache_block_size, MIN_CACHE_BLOCK_SIZE);
                assert_eq!(usage.cache_block_size,
                           cache.table.table.params.cache_block_size);

                // No data means no cache blocks used
                assert_eq!(usage.used_cache, DataBlocks(0));

                let performance = &status.performance;

                // No activity should mean all performance data is 0
                assert_eq!(performance.read_hits, 0);
                assert_eq!(performance.read_misses, 0);
                assert_eq!(performance.write_hits, 0);
                assert_eq!(performance.write_misses, 0);
                assert_eq!(performance.demotions, 0);
                assert_eq!(performance.promotions, 0);
                assert_eq!(performance.dirty, 0);

                // The current defaults for configuration values
                assert_eq!(status.feature_args, vec!["writethrough"]);
                assert_eq!(status.core_args,
                           vec![("migration_threshold".to_string(), "2048".to_string())]);
                assert_eq!(status.policy, "smq");

                assert_eq!(status.policy_args, vec![] as Vec<(String, String)>);

                assert_eq!(status.metadata_mode, CacheDevMetadataMode::Good);

                assert_eq!(status.needs_check, false);


            }
            _ => assert!(false),
        }

        let table = CacheDev::load_table(&dm, &DevId::Name(cache.name()))
            .unwrap()
            .table;

        let params = &table.params;
        assert_eq!(params.cache_block_size, MIN_CACHE_BLOCK_SIZE);
        assert_eq!(params.feature_args,
                   vec!["writethrough".into()]
                       .into_iter()
                       .collect::<HashSet<_>>());
        assert_eq!(params.policy, "default");

        cache.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_minimal_cache_dev() {
        test_with_spec(2, test_minimal_cache_dev);
    }

    /// Basic test of cache size change
    /// This executes the code paths, but is not enough to ensure correctness.
    /// * Construct a minimal cache
    /// * Expand the cache by one more block
    /// * Decrease the cache to its original size
    fn test_cache_size_change(paths: &[&Path]) {
        assert!(paths.len() >= 3);

        let dm = DM::new().unwrap();
        let mut cache = minimal_cachedev(&dm, paths);

        let mut cache_table = cache.cache_dev.table().table.clone();
        let dev3 = Device::from(devnode_to_devno(paths[2]).unwrap().unwrap());

        let extra_length = MIN_CACHE_BLOCK_SIZE;
        let cache_params = LinearTargetParams::new(dev3, Sectors(0));
        let current_length = cache.cache_dev.size();
        cache_table.push(TargetLine::new(current_length,
                                         extra_length,
                                         LinearDevTargetParams::Linear(cache_params)));
        assert!(cache.set_cache_table(&dm, cache_table.clone()).is_ok());

        cache_table.pop();

        assert!(cache.set_cache_table(&dm, cache_table).is_ok());

        cache.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_cache_size_change() {
        test_with_spec(3, test_cache_size_change);
    }

    /// Verify that suspending and resuming the cache doesn't fail.
    fn test_suspend(paths: &[&Path]) {
        assert!(paths.len() >= 2);

        let dm = DM::new().unwrap();
        let mut cache = minimal_cachedev(&dm, paths);
        cache.suspend(&dm).unwrap();
        cache.resume(&dm).unwrap();
        cache.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_suspend() {
        test_with_spec(2, test_suspend);
    }
}
