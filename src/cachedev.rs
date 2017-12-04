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
use super::shared::{DmDevice, device_create, device_exists, device_setup};
use super::types::{DataBlocks, DevId, DmName, DmUuid, MetaBlocks, Sectors, TargetLine,
                   TargetParams, TargetTypeBuf};


struct CacheDevStatusParams {
    pub meta_block_size: Sectors,
    pub meta_usage: (MetaBlocks, MetaBlocks),
    pub cache_block_size: Sectors,
    pub cache_usage: (DataBlocks, DataBlocks),
    pub read_hits: u64,
    pub read_misses: u64,
    pub write_hits: u64,
    pub write_misses: u64,
    pub demotions: u64,
    pub promotions: u64,
    pub dirty: u64,
    pub feature_args: Vec<String>,
    pub core_args: Vec<(String, String)>,
    pub policy: String,
    pub policy_args: Vec<(String, String)>,
    pub cache_metadata_mode: CacheDevMetadataMode,
    pub needs_check: bool,
}

impl CacheDevStatusParams {
    pub fn new(meta_block_size: Sectors,
               meta_usage: (MetaBlocks, MetaBlocks),
               cache_block_size: Sectors,
               cache_usage: (DataBlocks, DataBlocks),
               read_hits: u64,
               read_misses: u64,
               write_hits: u64,
               write_misses: u64,
               demotions: u64,
               promotions: u64,
               dirty: u64,
               feature_args: Vec<String>,
               core_args: Vec<(String, String)>,
               policy: String,
               policy_args: Vec<(String, String)>,
               cache_metadata_mode: CacheDevMetadataMode,
               needs_check: bool)
               -> CacheDevStatusParams {
        CacheDevStatusParams {
            meta_block_size: meta_block_size,
            meta_usage: meta_usage,
            cache_block_size: cache_block_size,
            cache_usage: cache_usage,
            read_hits: read_hits,
            read_misses: read_misses,
            write_hits: write_hits,
            write_misses: write_misses,
            demotions: demotions,
            promotions: promotions,
            dirty: dirty,
            feature_args: feature_args,
            core_args: core_args,
            policy: policy,
            policy_args: policy_args,
            cache_metadata_mode: cache_metadata_mode,
            needs_check: needs_check,
        }
    }
}

impl FromStr for CacheDevStatusParams {
    type Err = DmError;

    fn from_str(s: &str) -> Result<CacheDevStatusParams, DmError> {

        // Parse pairs of arguments from a slice
        fn parse_pairs(start_index: usize,
                       vals: &[&str])
                       -> Result<(usize, Vec<(String, String)>), DmError> {
            let num_items = vals[start_index]
                .parse::<usize>()
                .map_err(|e| {
                             DmError::Dm(ErrorEnum::ParseError,
                                         format!("could not parse number key/value items\"{}\": {}",
                                                 vals[start_index],
                                                 e))
                         })?;
            if num_items % 2 != 0 {
                return Err(DmError::Dm(ErrorEnum::ParseError,
                                       format!("expected even number of key/value items, found {}",
                                               num_items)));
            }

            let next_start_index = start_index + num_items + 1;
            Ok((next_start_index,
                vals[start_index + 1..next_start_index]
                    .chunks(2)
                    .map(|p| (p[0].to_string(), p[1].to_string()))
                    .collect()))
        }


        let vals = s.split(' ').collect::<Vec<_>>();
        if vals.len() < 17 {
            return Err(DmError::Dm(ErrorEnum::ParseError,
                                   format!("expected at least 17 values in \"{}\", found {}",
                                           s,
                                           vals.len())));
        }

        let meta_block_size = vals[0]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse meta block size \"{}\": {}",
                                             vals[0],
                                             e))
                     })?;
        let meta_usage = vals[1].split('/').collect::<Vec<_>>();
        if meta_usage.len() != 2 {
            return Err(DmError::Dm(ErrorEnum::ParseError,
                                   format!("expected exactly 2 values in \"{}\", found {}",
                                           vals[1],
                                           meta_usage.len())));
        }

        let meta_usage = (meta_usage[0].parse::<u64>().map(MetaBlocks).map_err(|e| {
            DmError::Dm(ErrorEnum::ParseError,
                        format!("could not parse used metadata blocks \"{}\": {}",
                                meta_usage[0],
                                e))})?,
            meta_usage[1].parse::<u64>().map(MetaBlocks).map_err(|e| {
            DmError::Dm(ErrorEnum::ParseError,
                        format!("could not parse total metadata blocks \"{}\": {}",
                            meta_usage[1],
                        e))})?);

        let cache_block_size = vals[2]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse cache block size \"{}\": {}",
                                             vals[2],
                                             e))
                     })?;

        let cache_usage = vals[3].split('/').collect::<Vec<_>>();
        if cache_usage.len() != 2 {
            return Err(DmError::Dm(ErrorEnum::ParseError,
                                   format!("expected exactly 2 values in \"{}\", found {}",
                                           vals[3],
                                           cache_usage.len())));
        }

        let cache_usage =
            (cache_usage[0]
                 .parse::<u64>()
                 .map(DataBlocks)
                 .map_err(|e| {
                              DmError::Dm(ErrorEnum::ParseError,
                                          format!("could not parse used cache blocks \"{}\": {}",
                                                  cache_usage[0],
                                                  e))
                          })?,
             cache_usage[1]
                 .parse::<u64>()
                 .map(DataBlocks)
                 .map_err(|e| {
                              DmError::Dm(ErrorEnum::ParseError,
                                          format!("could not parse total cache blocks \"{}\": {}",
                                                  cache_usage[1],
                                                  e))
                          })?);

        let read_hits = vals[4]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse read hits \"{}\": {}", vals[4], e))
                     })?;
        let read_misses = vals[5]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse read misses \"{}\": {}", vals[5], e))
                     })?;
        let write_hits = vals[6]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse write hits \"{}\": {}", vals[6], e))
                     })?;
        let write_misses = vals[7]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse write misses \"{}\": {}", vals[7], e))
                     })?;
        let demotions = vals[8]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse demotions\"{}\": {}", vals[8], e))
                     })?;
        let promotions = vals[9]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse promotions\"{}\": {}", vals[9], e))
                     })?;
        let dirty = vals[10]
            .parse::<u64>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse number dirty blocks\"{}\": {}",
                                             vals[10],
                                             e))
                     })?;

        let num_feature_args = vals[11]
            .parse::<usize>()
            .map_err(|e| {
                         DmError::Dm(ErrorEnum::ParseError,
                                     format!("could not parse number feature args\"{}\": {}",
                                             vals[11],
                                             e))
                     })?;
        let core_args_start_index = 12usize + num_feature_args;
        let feature_args: Vec<String> = vals[12..core_args_start_index]
            .iter()
            .map(|x| x.to_string())
            .collect();

        let (policy_start_index, core_args) = parse_pairs(core_args_start_index, &vals)?;
        let policy = vals[policy_start_index].to_string();
        let (rest_start_index, policy_args) = parse_pairs(policy_start_index + 1, &vals)?;

        let cache_metadata_mode = match vals[rest_start_index] {
            "rw" => CacheDevMetadataMode::Good,
            "ro" => CacheDevMetadataMode::ReadOnly,
            val => {
                return Err(DmError::Dm(ErrorEnum::ParseError,
                                       format!("unexpected value \"{}\" for cache metadata mode",
                                               val)));
            }
        };

        let needs_check = match vals[rest_start_index + 1] {
            "-" => false,
            "needs_check" => true,
            val => {
                return Err(DmError::Dm(ErrorEnum::ParseError,
                                       format!("unexpected value \"{}\" for needs check", val)));
            }
        };

        Ok(CacheDevStatusParams::new(meta_block_size,
                                     meta_usage,
                                     cache_block_size,
                                     cache_usage,
                                     read_hits,
                                     read_misses,
                                     write_hits,
                                     write_misses,
                                     demotions,
                                     promotions,
                                     dirty,
                                     feature_args,
                                     core_args,
                                     policy,
                                     policy_args,
                                     cache_metadata_mode,
                                     needs_check))

    }
}

impl From<CacheDevStatusParams> for CacheDevWorkingStatus {
    fn from(params: CacheDevStatusParams) -> CacheDevWorkingStatus {
        let (used_meta, total_meta) = params.meta_usage;
        let (used_cache, total_cache) = params.cache_usage;
        let usage = CacheDevUsage::new(params.meta_block_size,
                                       used_meta,
                                       total_meta,
                                       params.cache_block_size,
                                       used_cache,
                                       total_cache);
        let performance = CacheDevPerformance::new(params.read_hits,
                                                   params.read_misses,
                                                   params.write_hits,
                                                   params.write_misses,
                                                   params.demotions,
                                                   params.promotions,
                                                   params.dirty);
        CacheDevWorkingStatus::new(usage,
                                   performance,
                                   params.feature_args,
                                   params.core_args,
                                   params.policy,
                                   params.policy_args,
                                   params.cache_metadata_mode,
                                   params.needs_check)
    }
}


/// CacheDev target params
#[derive(Debug, PartialEq)]
pub struct CacheDevTargetParams {
    /// meta device
    pub meta: Device,
    /// cache device
    pub cache: Device,
    /// cache origin device
    pub origin: Device,
    /// cache block size
    pub cache_block_size: Sectors,
    /// feature args
    pub feature_args: Vec<String>,
    /// replacement policy
    pub policy: String,
    /// policy args
    pub policy_args: Vec<(String, String)>,
}

impl CacheDevTargetParams {
    /// Make a new CacheDevTargetParams struct
    pub fn new(meta: Device,
               cache: Device,
               origin: Device,
               cache_block_size: Sectors)
               -> CacheDevTargetParams {
        CacheDevTargetParams {
            meta: meta,
            cache: cache,
            origin: origin,
            cache_block_size: cache_block_size,
            feature_args: vec![],
            policy: "default".to_owned(),
            policy_args: vec![],
        }
    }
}

impl fmt::Display for CacheDevTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let feature_args = if self.feature_args.is_empty() {
            "0".to_owned()
        } else {
            format!("{} {}",
                    self.feature_args.len(),
                    self.feature_args.join(" "))
        };

        let policy_args = if self.policy_args.is_empty() {
            "0".to_owned()
        } else {
            format!("{} {}",
                    self.policy_args.len(),
                    self.policy_args
                        .iter()
                        .map(|&(ref k, ref v)| format!("{} {}", k, v))
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

impl TargetParams for CacheDevTargetParams {}


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

impl DmDevice for CacheDev {
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
        self.origin_dev.size()
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        self.cache_dev.teardown(dm)?;
        self.origin_dev.teardown(dm)?;
        self.meta_dev.teardown(dm)?;
        Ok(())
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

        let table = CacheDev::dm_table(&meta, &cache, &origin, cache_block_size);
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
        let table = CacheDev::dm_table(&meta, &origin, &cache, cache_block_size);
        let dev_info = device_setup(dm, name, uuid, &table)?;

        Ok(CacheDev {
               dev_info: Box::new(dev_info),
               meta_dev: meta,
               cache_dev: cache,
               origin_dev: origin,
               block_size: cache_block_size,
           })
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start sec> <length> "cache" <cache-specific string>
    /// where the cache-specific string has the format:
    /// <meta maj:min> <cache maj:min> <origin maj:min> <block size>
    /// <#num feature args (0)> <replacement policy (default)>
    /// <#num policy args (0)>
    /// There is exactly one entry in the table.
    fn dm_table(meta: &LinearDev,
                cache: &LinearDev,
                origin: &LinearDev,
                cache_block_size: Sectors)
                -> Vec<TargetLine<CacheDevTargetParams>> {
        vec![TargetLine {
                 start: Sectors::default(),
                 length: origin.size(),
                 target_type: TargetTypeBuf::new("cache".into()).expect("< length limit"),
                 params: CacheDevTargetParams::new(meta.device(),
                                                   cache.device(),
                                                   origin.device(),
                                                   cache_block_size),
             }]
    }

    /// Get the current status of the cache device.
    // Note: This method is not entirely complete. In particular, *_args values
    // may require more or better checking or processing.
    pub fn status(&self, dm: &DM) -> DmResult<CacheDevStatus> {
        let (_, status) = dm.table_status(&DevId::Name(self.name()), DmFlags::empty())?;

        assert_eq!(status.len(),
                   1,
                   "Kernel must return 1 line from cache dev status");

        let status_line = &status.get(0).expect("assertion above holds").params;
        if status_line.starts_with("Fail") {
            return Ok(CacheDevStatus::Fail);
        }

        let params = status_line.parse::<CacheDevStatusParams>()?;
        Ok(CacheDevStatus::Working(Box::new(params.into())))
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::super::consts::IEC;
    use super::super::loopbacked::{devnode_to_devno, test_with_spec};
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
        let dev1 = Device::from(devnode_to_devno(paths[0]).unwrap());

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

        let dev2 = Device::from(devnode_to_devno(paths[1]).unwrap());

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

        cache.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_minimal_cache_dev() {
        test_with_spec(2, test_minimal_cache_dev);
    }
}
