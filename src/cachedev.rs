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
use super::lineardev::LinearDev;
use super::result::{DmResult, DmError, ErrorEnum};
use super::shared::{DmDevice, TargetLine, TargetParams, device_create, device_exists,
                    device_match, parse_device};
use super::types::{DataBlocks, DevId, DmName, DmUuid, MetaBlocks, Sectors, TargetTypeBuf};

#[derive(Debug, Eq, PartialEq)]
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
            meta: meta,
            cache: cache,
            origin: origin,
            cache_block_size: cache_block_size,
            feature_args: feature_args.into_iter().collect::<HashSet<_>>(),
            policy: policy,
            policy_args: policy_args.into_iter().collect::<HashMap<_, _>>(),
        }
    }
}

impl fmt::Display for CacheTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

        write!(f,
               "{} {} {} {} {} {} {}",
               self.meta,
               self.cache,
               self.origin,
               *self.cache_block_size,
               feature_args,
               self.policy,
               policy_args)
    }
}

impl FromStr for CacheTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<CacheTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();

        if vals.len() < 7 {
            let err_msg = format!("expected at least 7 values in params string \"{}\", found {}",
                                  s,
                                  vals.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let metadata_dev = parse_device(vals[0])?;
        let cache_dev = parse_device(vals[1])?;
        let origin_dev = parse_device(vals[2])?;

        let block_size = vals[3]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for data block size from \"{}\"",
                                    vals[3]))})?;

        let num_feature_args = vals[4]
            .parse::<usize>()
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for number of feature args from \"{}\"",
                                    vals[4]))})?;

        let end_feature_args_index = 5 + num_feature_args;
        let feature_args: Vec<String> = vals[5..end_feature_args_index]
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

impl TargetParams for CacheTargetParams {}


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
            meta_block_size: meta_block_size,
            used_meta: used_meta,
            total_meta: total_meta,
            cache_block_size: cache_block_size,
            used_cache: used_cache,
            total_cache: total_cache,
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
            read_hits: read_hits,
            read_misses: read_misses,
            write_hits: write_hits,
            write_misses: write_misses,
            demotions: demotions,
            promotions: promotions,
            dirty: dirty,
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
            usage: usage,
            performance: performance,
            feature_args: feature_args,
            core_args: core_args,
            policy: policy,
            policy_args: policy_args,
            metadata_mode: metadata_mode,
            needs_check: needs_check,
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
    block_size: Sectors,
}

impl DmDevice<CacheTargetParams> for CacheDev {
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
    fn equivalent_tables(left: &[TargetLine<CacheTargetParams>],
                         right: &[TargetLine<CacheTargetParams>])
                         -> DmResult<bool> {
        if left.len() != 1 {
            let err_msg = format!("cache dev tables have exactly one line, found {} lines in table",
                                  left.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }
        if right.len() != 1 {
            let err_msg = format!("cache dev tables have exactly one line, found {} lines in table",
                                  right.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let left = left.first().expect("left.len() == 1");
        let right = right.first().expect("right.len() == 1");

        Ok(left.start == right.start && left.length == right.length &&
           left.target_type == right.target_type &&
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
               block_size: cache_block_size,
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
        let table = CacheDev::gen_default_table(&meta, &origin, &cache, cache_block_size);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = CacheDev {
                dev_info: Box::new(dev_info),
                meta_dev: meta,
                cache_dev: cache,
                origin_dev: origin,
                block_size: cache_block_size,
            };
            device_match(dm, &dev, uuid, &table)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table)?;
            CacheDev {
                dev_info: Box::new(dev_info),
                meta_dev: meta,
                cache_dev: cache,
                origin_dev: origin,
                block_size: cache_block_size,
            }
        };

        Ok(dev)
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start sec> <length> "cache" <cache-specific string>
    /// where the cache-specific string has the format:
    /// <meta maj:min> <cache maj:min> <origin maj:min> <block size>
    /// <#num feature args (0)> <replacement policy (default)>
    /// <#num policy args (0)>
    /// There is exactly one entry in the table.
    /// Various defaults are hard coded in the method.
    fn gen_default_table(meta: &LinearDev,
                         cache: &LinearDev,
                         origin: &LinearDev,
                         cache_block_size: Sectors)
                         -> Vec<TargetLine<CacheTargetParams>> {
        vec![TargetLine {
                 start: Sectors::default(),
                 length: origin.size(),
                 target_type: TargetTypeBuf::new("cache".into()).expect("< length limit"),
                 params: CacheTargetParams::new(meta.device(),
                                                cache.device(),
                                                origin.device(),
                                                cache_block_size,
                                                vec![],
                                                "default".to_owned(),
                                                vec![]),
             }]
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
mod tests {
    use std::path::Path;

    use super::super::consts::IEC;
    use super::super::device::devnode_to_devno;
    use super::super::loopbacked::test_with_spec;
    use super::super::segment::Segment;

    use super::*;

    // Specified in kernel docs
    const MIN_CACHE_BLOCK_SIZE: Sectors = Sectors(64); // 32 KiB
    #[allow(dead_code)]
    const MAX_CACHE_BLOCK_SIZE: Sectors = Sectors(2_097_152); // 1 GiB

    // Test creating a minimal cache dev.
    // Verify that status method executes and gives reasonable values.
    fn test_minimal_cache_dev(paths: &[&Path]) -> () {
        assert!(paths.len() >= 2);
        let dev1 = Device::from(devnode_to_devno(paths[0]).unwrap().unwrap());

        let dm = DM::new().unwrap();

        let meta_name = DmName::new("cache-meta").expect("valid format");

        // Minimum recommended metadata size for thinpool
        let meta_length = Sectors(4 * IEC::Ki);
        let meta = LinearDev::setup(&dm,
                                    meta_name,
                                    None,
                                    &[Segment::new(dev1, Sectors(0), meta_length)])
                .unwrap();

        let cache_name = DmName::new("cache-cache").expect("valid format");
        let cache_offset = meta_length;
        let cache_length = MIN_CACHE_BLOCK_SIZE;
        let cache = LinearDev::setup(&dm,
                                     cache_name,
                                     None,
                                     &[Segment::new(dev1, cache_offset, cache_length)])
                .unwrap();

        let dev2 = Device::from(devnode_to_devno(paths[1]).unwrap().unwrap());

        let origin_name = DmName::new("cache-origin").expect("valid format");
        let origin_length = 512u64 * MIN_CACHE_BLOCK_SIZE;
        let origin = LinearDev::setup(&dm,
                                      origin_name,
                                      None,
                                      &[Segment::new(dev2, Sectors(0), origin_length)])
                .unwrap();

        let cache = CacheDev::new(&dm,
                                  DmName::new("cache").expect("valid format"),
                                  None,
                                  meta,
                                  cache,
                                  origin,
                                  MIN_CACHE_BLOCK_SIZE)
                .unwrap();

        match cache.status(&dm).unwrap() {
            CacheDevStatus::Working(ref status) => {
                let usage = &status.usage;

                assert_eq!(usage.meta_block_size, Sectors(8));

                // Even an empty cache dev takes up some metadata space.
                assert!(usage.used_meta > MetaBlocks(0));

                assert_eq!(usage.cache_block_size, MIN_CACHE_BLOCK_SIZE);
                assert_eq!(usage.cache_block_size, cache.block_size);

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
                assert_eq!(status.feature_args, vec!["writeback"]);
                assert_eq!(status.core_args,
                           vec![("migration_threshold".to_string(), "2048".to_string())]);
                assert_eq!(status.policy, "smq");

                assert_eq!(status.policy_args, vec![] as Vec<(String, String)>);

                assert_eq!(status.metadata_mode, CacheDevMetadataMode::Good);

                assert_eq!(status.needs_check, false);


            }
            _ => assert!(false),
        }

        let table = CacheDev::load_table(&dm, &DevId::Name(cache.name())).unwrap();
        assert_eq!(table.len(), 1);

        let line = &table[0];
        let params = &line.params;
        assert_eq!(params.cache_block_size, MIN_CACHE_BLOCK_SIZE);
        assert_eq!(params.feature_args, HashSet::new());
        assert_eq!(params.policy, "default");

        cache.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_minimal_cache_dev() {
        test_with_spec(2, test_minimal_cache_dev);
    }
}
