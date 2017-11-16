// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::path::PathBuf;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::lineardev::LinearDev;
use super::result::{DmResult, DmError, ErrorEnum};
use super::shared::{DmDevice, device_create, device_exists, device_setup};
use super::types::{DataBlocks, DevId, DmName, DmUuid, MetaBlocks, Sectors, TargetLine,
                   TargetTypeBuf};

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
               cache_block_size: Sectors,
               meta: LinearDev,
               cache: LinearDev,
               origin: LinearDev)
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
                 cache_block_size: Sectors,
                 meta: LinearDev,
                 cache: LinearDev,
                 origin: LinearDev)
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
                -> Vec<TargetLine> {
        let params = format!("{} {} {} {} 0 default 0",
                             meta.device(),
                             cache.device(),
                             origin.device(),
                             *cache_block_size);
        vec![TargetLine {
                 start: Sectors::default(),
                 length: origin.size(),
                 target_type: TargetTypeBuf::new("cache".into()).expect("< length limit"),
                 params: params,
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

        let num_core_args = status_vals[core_args_start_index]
            .parse::<usize>()
            .expect("number value must be valid");
        if num_core_args % 2 != 0 {
            panic!(format!("Number of core args \"{}\" is not even", num_core_args));
        }

        let policy_start_index = core_args_start_index + num_core_args + 1;
        let core_args: Vec<(String, String)> = status_vals[core_args_start_index + 1..
        policy_start_index]
                .chunks(2)
                .map(|p| (p[0].to_string(), p[1].to_string()))
                .collect();


        let policy = status_vals[policy_start_index].to_string();
        let num_policy_args = status_vals[policy_start_index + 1]
            .parse::<usize>()
            .expect("number value must be valid");
        if num_policy_args % 2 != 0 {
            panic!(format!("Number of policy args \"{}\" is not even", num_policy_args));
        }
        let rest_start_index = policy_start_index + 1 + num_policy_args + 1;
        let policy_args: Vec<(String, String)> = status_vals[policy_start_index + 2..
        rest_start_index]
                .chunks(2)
                .map(|p| (p[0].to_string(), p[1].to_string()))
                .collect();

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
                                  MIN_CACHE_BLOCK_SIZE,
                                  meta,
                                  cache,
                                  origin)
                .unwrap();

        match cache.status(&dm).unwrap() {
            CacheDevStatus::Working(ref status) => {
                let usage = &status.usage;

                assert_eq!(usage.meta_block_size, Sectors(8));

                // Even an empty cache dev takes up some metadata space.
                assert!(usage.used_meta > MetaBlocks(0));

                assert_eq!(usage.cache_block_size, MIN_CACHE_BLOCK_SIZE);
                assert_eq!(usage.cache_block_size, cache.cache_block_size);

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
