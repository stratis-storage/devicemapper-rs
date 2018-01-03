// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::result::{DmError, DmResult, ErrorEnum};
use super::shared::{DmDevice, TargetLine, TargetParams, device_create, device_exists,
                    device_match, message, parse_device, table_reload};
use super::thindevid::ThinDevId;
use super::thinpooldev::ThinPoolDev;
use super::types::{DevId, DmName, DmUuid, Sectors, TargetTypeBuf};


#[derive(Debug, Eq, PartialEq)]
pub struct ThinDevTargetParams {
    pub pool: Device,
    pub thin_id: ThinDevId,
    pub external_origin_dev: Option<Device>,
}

impl ThinDevTargetParams {
    pub fn new(pool: Device,
               thin_id: ThinDevId,
               external_origin_dev: Option<Device>)
               -> ThinDevTargetParams {
        ThinDevTargetParams {
            pool: pool,
            thin_id: thin_id,
            external_origin_dev: external_origin_dev,
        }
    }
}

impl fmt::Display for ThinDevTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.external_origin_dev {
            None => write!(f, "{} {}", self.pool, self.thin_id),
            Some(dev) => write!(f, "{} {} {}", self.pool, self.thin_id, dev),
        }
    }
}

impl FromStr for ThinDevTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<ThinDevTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();
        let len = vals.len();
        if len < 2 || len > 3 {
            let err_msg = format!("expected two or three values in params string \"{}\", found {}",
                                  s,
                                  len);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        Ok(ThinDevTargetParams::new(parse_device(vals[0])?,
                                    vals[1].parse::<ThinDevId>()?,
                                    if len == 2 {
                                        None
                                    } else {
                                        Some(parse_device(vals[2])?)
                                    }))
    }
}

impl TargetParams for ThinDevTargetParams {}


/// DM construct for a thin block device
#[derive(Debug)]
pub struct ThinDev {
    dev_info: Box<DeviceInfo>,
    thin_id: ThinDevId,
    size: Sectors,
    thinpool: Device,
}

impl DmDevice<ThinDevTargetParams> for ThinDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    // This method is incomplete. It is expected that it will be refined so
    // that it will return true in more cases, i.e., to be less stringent.
    fn equivalent_tables(left: &[TargetLine<ThinDevTargetParams>],
                         right: &[TargetLine<ThinDevTargetParams>])
                         -> DmResult<bool> {
        Ok(left == right)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.size
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        Ok(())
    }

    fn uuid(&self) -> Option<&DmUuid> {
        uuid!(self)
    }
}

/// Status values for a thin device that is working
#[derive(Clone, Debug)]
pub struct ThinDevWorkingStatus {
    /// The number of mapped sectors
    pub nr_mapped_sectors: Sectors,
    /// The highest mapped sector if any.
    pub highest_mapped_sector: Option<Sectors>,
}

impl ThinDevWorkingStatus {
    /// Make a new ThinDevWorkingStatus struct
    pub fn new(nr_mapped_sectors: Sectors,
               highest_mapped_sector: Option<Sectors>)
               -> ThinDevWorkingStatus {
        ThinDevWorkingStatus {
            nr_mapped_sectors: nr_mapped_sectors,
            highest_mapped_sector: highest_mapped_sector,
        }
    }
}

#[derive(Clone, Debug)]
/// Thin device status.
pub enum ThinStatus {
    /// Thin device is good. Includes number of mapped sectors, and
    /// highest mapped sector.
    Working(Box<ThinDevWorkingStatus>),
    /// Thin device is failed.
    Fail,
}

/// support use of DM for thin provisioned devices over pools
impl ThinDev {
    /// Create a ThinDev using thin_pool as the backing store.
    /// If the specified thin_id is already in use by the thin pool an error
    /// is returned. If the device is already among the list of devices that
    /// dm is aware of, return an error.
    pub fn new(dm: &DM,
               name: &DmName,
               uuid: Option<&DmUuid>,
               length: Sectors,
               thin_pool: &ThinPoolDev,
               thin_id: ThinDevId)
               -> DmResult<ThinDev> {

        message(dm, thin_pool, &format!("create_thin {}", thin_id))?;

        if device_exists(dm, name)? {
            let err_msg = "Uncreated device should not be known to kernel";
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg.into()));
        }

        let thin_pool_device = thin_pool.device();
        let table = ThinDev::gen_default_table(length, thin_pool_device, thin_id);
        let dev_info = device_create(dm, name, uuid, &table)?;

        Ok(ThinDev {
               dev_info: Box::new(dev_info),
               thin_id: thin_id,
               size: length,
               thinpool: thin_pool_device,
           })
    }

    /// Set up a thin device which already belongs to the given thin_pool.
    /// The thin device is identified by the thin_id, which is already
    /// known to the pool.
    ///
    /// If the device is already known to kernel, just verify that specified
    /// data matches and return an error if it does not.
    ///
    /// If the device has no thin id already registered with the thin pool
    /// an error is returned.
    pub fn setup(dm: &DM,
                 name: &DmName,
                 uuid: Option<&DmUuid>,
                 length: Sectors,
                 thin_pool: &ThinPoolDev,
                 thin_id: ThinDevId)
                 -> DmResult<ThinDev> {

        let thin_pool_device = thin_pool.device();
        let table = ThinDev::gen_default_table(length, thin_pool_device, thin_id);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = ThinDev {
                dev_info: Box::new(dev_info),
                thin_id: thin_id,
                size: length,
                thinpool: thin_pool_device,
            };
            device_match(dm, &dev, uuid, &table)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table)?;
            ThinDev {
                dev_info: Box::new(dev_info),
                thin_id: thin_id,
                size: length,
                thinpool: thin_pool_device,
            }
        };
        Ok(dev)

    }

    /// Create a snapshot of a ThinDev.  Once created a snapshot
    /// is the same as any other thin provisioned device.  There is
    /// no need to track any connection between the source and the
    /// snapshot.
    pub fn snapshot(&self,
                    dm: &DM,
                    thin_pool: &ThinPoolDev,
                    snapshot_name: &DmName,
                    snapshot_thin_id: ThinDevId)
                    -> DmResult<ThinDev> {
        let source_id = DevId::Name(self.name());
        dm.device_suspend(&source_id, DmFlags::DM_SUSPEND)?;
        message(dm,
                thin_pool,
                &format!("create_snap {} {}", snapshot_thin_id, self.thin_id))?;
        dm.device_suspend(&source_id, DmFlags::empty())?;
        let dev_info = Box::new(device_create(dm,
                                              snapshot_name,
                                              None,
                                              &ThinDev::gen_default_table(self.size(),
                                                                          thin_pool.device(),
                                                                          snapshot_thin_id))?);
        Ok(ThinDev {
               dev_info: dev_info,
               thin_id: snapshot_thin_id,
               size: self.size(),
               thinpool: thin_pool.device(),
           })
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start> <length> "thin" <thin device specific string>
    /// where the thin device specific string has the format:
    /// <thinpool maj:min> <thin_id>
    /// There is exactly one entry in the table.
    /// Various defaults are hard coded in the method.
    fn gen_default_table(length: Sectors,
                         thin_pool: Device,
                         thin_id: ThinDevId)
                         -> Vec<TargetLine<ThinDevTargetParams>> {
        vec![TargetLine {
                 start: Sectors::default(),
                 length: length,
                 target_type: TargetTypeBuf::new("thin".into()).expect("< length limit"),
                 params: ThinDevTargetParams::new(thin_pool, thin_id, None),
             }]
    }

    /// return the thin id of the linear device
    pub fn id(&self) -> ThinDevId {
        self.thin_id
    }

    /// Get the current status of the thin device.
    pub fn status(&self, dm: &DM) -> DmResult<ThinStatus> {
        let (_, table) = dm.table_status(&DevId::Name(self.name()), DmFlags::empty())?;

        assert_eq!(table.len(),
                   1,
                   "Kernel must return 1 line table for thin status");

        let status_line = &table.first().expect("assertion above holds").3;
        if status_line.starts_with("Fail") {
            return Ok(ThinStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(status_vals.len() >= 2,
                "Kernel must return at least 2 values from thin pool status");

        let count = status_vals[0]
            .parse::<u64>()
            .map(Sectors)
            .expect("Kernel always returns a parseable u64 for sector count");

        let highest = if count == Sectors(0) {
            None
        } else {
            Some(Sectors(status_vals[1]
                             .parse::<u64>()
                             .expect("Kernel always returns a parseable u64 when count > 0")))
        };

        Ok(ThinStatus::Working(Box::new(ThinDevWorkingStatus::new(count, highest))))
    }

    /// Extend the thin device's (virtual) size by the number of
    /// sectors given.
    pub fn extend(&mut self, dm: &DM, sectors: Sectors) -> DmResult<()> {
        let new_size = self.size + sectors;
        table_reload(dm,
                     &DevId::Name(self.name()),
                     &ThinDev::gen_default_table(new_size, self.thinpool, self.thin_id))?;
        self.size = new_size;
        Ok(())
    }

    /// Tear down the DM device, and also delete resources associated
    /// with its thin id from the thinpool.
    pub fn destroy(self, dm: &DM, thin_pool: &ThinPoolDev) -> DmResult<()> {
        let thin_id = self.thin_id;
        self.teardown(dm)?;
        message(dm, thin_pool, &format!("delete {}", thin_id))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use std::fs::OpenOptions;
    use std::io::Write;
    use std::path::Path;
    use std::process::Command;

    use nix::mount::{MNT_DETACH, MsFlags, mount, umount2};
    use tempdir::TempDir;
    use uuid::Uuid;

    use super::super::consts::IEC;
    use super::super::loopbacked::{blkdev_size, test_with_spec};
    use super::super::thinpooldev::{ThinPoolStatus, minimal_thinpool};
    use super::super::types::DataBlocks;

    use super::super::errors::{Error, ErrorKind};

    use super::*;

    const MIN_THIN_DEV_SIZE: Sectors = Sectors(1);

    /// Verify that specifying a size of 0 Sectors will cause a failure.
    fn test_zero_size(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        assert!(ThinDev::new(&dm,
                             &DmName::new("name").expect("is valid DM name"),
                             None,
                             Sectors(0),
                             &tp,
                             ThinDevId::new_u64(0).expect("is below limit"))
                        .is_err());
        tp.teardown(&dm).unwrap();
    }

    /// Verify that setting up a thin device without first calling new()
    /// causes an error. The underlying reason is that the thin pool hasn't
    /// been informed about the thin device by messaging the value of the
    /// thin id.
    fn test_setup_without_new(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        let td_size = MIN_THIN_DEV_SIZE;
        assert!(match ThinDev::setup(&dm,
                                     &DmName::new("name").expect("is valid DM name"),
                                     None,
                                     td_size,
                                     &tp,
                                     ThinDevId::new_u64(0).expect("is below limit")) {
                    Err(DmError::Core(Error(ErrorKind::IoctlError(_), _))) => true,
                    _ => false,
                });

        tp.teardown(&dm).unwrap();
    }

    /// Verify success when constructing a new ThinDev. Check that the
    /// status of the device is as expected. Verify that it is now possible
    /// to call setup() on the thin dev specifying the same name and id.
    /// Verify that calling new() for the second time fails. Verify that
    /// setup() is idempotent, calling setup() twice in succession succeeds.
    /// Verify that setup() succeeds on an existing device, whether or not
    /// it has been torn down.
    fn test_basic(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);
        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let id = DmName::new("name").expect("is valid DM name");

        let td_size = MIN_THIN_DEV_SIZE;
        let td = ThinDev::new(&dm, &id, None, td_size, &tp, thin_id).unwrap();

        let table = td.table(&dm).unwrap();
        assert_eq!(table.len(), 1);

        let line = &table[0];
        assert_eq!(line.params.pool, tp.device());
        assert_eq!(line.params.thin_id, thin_id);

        assert!(match td.status(&dm).unwrap() {
                    ThinStatus::Fail => false,
                    _ => true,
                });

        assert_eq!(blkdev_size(&OpenOptions::new()
                                    .read(true)
                                    .open(td.devnode())
                                    .unwrap()),
                   td_size.bytes());

        // New thindev w/ same id fails.
        assert!(match ThinDev::new(&dm, &id, None, td_size, &tp, thin_id) {
                    Err(DmError::Core(Error(ErrorKind::IoctlError(_), _))) => true,
                    _ => false,
                });

        // Verify that the device of that name does exist.
        assert!(device_exists(&dm, id).unwrap());

        // Setting up the just created thin dev succeeds.
        assert!(ThinDev::setup(&dm, &id, None, td_size, &tp, thin_id).is_ok());

        // Setting up the just created thin dev once more succeeds.
        assert!(ThinDev::setup(&dm, &id, None, td_size, &tp, thin_id).is_ok());

        // Teardown the thindev, then set it back up.
        td.teardown(&dm).unwrap();
        let td = ThinDev::setup(&dm, &id, None, td_size, &tp, thin_id);
        assert!(td.is_ok());

        td.unwrap().destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify success when taking a snapshot of a ThinDev.  Check that
    /// the size of the snapshot is the same as the source.
    /// Verify that empty thindev has no data usage.
    fn test_snapshot(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);
        let td_size = MIN_THIN_DEV_SIZE;
        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        let orig_data_usage = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };

        assert_eq!(orig_data_usage, DataBlocks(0));

        // Create new ThinDev as source for snapshot
        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = DmName::new("name").expect("is valid DM name");
        let td = ThinDev::new(&dm, &thin_name, None, td_size, &tp, thin_id).unwrap();

        let data_usage_1 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };

        assert_eq!(data_usage_1, DataBlocks(0));

        // Create a snapshot of the source
        let ss_id = ThinDevId::new_u64(1).expect("is below limit");
        let ss_name = DmName::new("snap_name").expect("is valid DM name");
        let ss = td.snapshot(&dm, &tp, ss_name, ss_id).unwrap();

        let data_usage_2 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };

        assert_eq!(data_usage_2, DataBlocks(0));

        // Verify the source and the snapshot are the same size.
        assert_eq!(td.size(), ss.size());

        ss.destroy(&dm, &tp).unwrap();
        td.destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify no failures when creating a thindev from a pool, mounting a
    /// filesystem on the thin device, and writing to that filesystem.
    /// Verify reasonable usage behavior.
    fn test_filesystem(paths: &[&Path]) -> () {
        assert!(paths.len() > 0);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = DmName::new("name").expect("is valid DM name");
        let td = ThinDev::new(&dm, &thin_name, None, tp.size(), &tp, thin_id).unwrap();

        let orig_data_usage = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(orig_data_usage, DataBlocks(0));

        Command::new("mkfs.xfs")
            .arg("-f")
            .arg("-q")
            .arg(&td.devnode())
            .status()
            .unwrap();

        let data_usage_1 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert!(data_usage_1 > DataBlocks(0));

        let tmp_dir = TempDir::new("stratis_testing").unwrap();
        mount(Some(&td.devnode()),
              tmp_dir.path(),
              Some("xfs"),
              MsFlags::empty(),
              None as Option<&str>)
                .unwrap();

        for i in 0..100 {
            let file_path = tmp_dir.path().join(format!("stratis_test{}.txt", i));
            writeln!(&OpenOptions::new()
                          .create(true)
                          .write(true)
                          .open(file_path)
                          .unwrap(),
                     "data")
                    .unwrap();
        }
        umount2(tmp_dir.path(), MNT_DETACH).unwrap();

        let data_usage_2 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert!(data_usage_2 > data_usage_1);

        td.destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify reasonable usage behavior when taking a snapshot of a thindev
    /// with an existing filesystem. In particular, just taking a snapshot
    /// should not increase the pool usage at all.
    /// If ThindevA is one GiB and ThindevB is 1 TiB, the making a filesystem
    /// on ThindevB consumes at least 32 times the space as making a filesystem
    /// on ThindevA. Verify that setting the UUID of a snapshot causes the
    /// snapshot to consume approximately the same amount of space as its
    /// source.
    fn test_snapshot_usage(paths: &[&Path]) -> () {
        assert!(paths.len() > 0);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = DmName::new("name").expect("is valid DM name");
        let td = ThinDev::new(&dm, &thin_name, None, Sectors(2 * IEC::Mi), &tp, thin_id).unwrap();

        let orig_data_usage = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(orig_data_usage, DataBlocks(0));

        Command::new("mkfs.xfs")
            .arg("-f")
            .arg("-q")
            .arg(&td.devnode())
            .status()
            .unwrap();

        let data_usage_1 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert!(data_usage_1 > DataBlocks(0));

        // Create a snapshot of the source
        let ss_id = ThinDevId::new_u64(1).expect("is below limit");
        let ss_name = DmName::new("snap_name").expect("is valid DM name");
        let ss = td.snapshot(&dm, &tp, ss_name, ss_id).unwrap();

        let data_usage_2 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(data_usage_2, data_usage_1);

        Command::new("xfs_admin")
            .arg("-U")
            .arg(format!("{}", Uuid::new_v4()))
            .arg(&ss.devnode())
            .status()
            .unwrap();

        // Setting the uuid of the snapshot filesystem bumps the usage,
        // but does not increase the usage quite as much as establishing
        // the origin.
        let data_usage_3 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert!(data_usage_3 - data_usage_2 > DataBlocks(0));
        assert!(data_usage_3 - data_usage_2 < data_usage_1);
        assert!(data_usage_3 - data_usage_2 > data_usage_1 / 2usize);

        let thin_id = ThinDevId::new_u64(2).expect("is below limit");
        let thin_name = DmName::new("name1").expect("is valid DM name");
        let td1 = ThinDev::new(&dm, &thin_name, None, Sectors(2 * IEC::Gi), &tp, thin_id).unwrap();

        let data_usage_4 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(data_usage_4, data_usage_3);

        Command::new("mkfs.xfs")
            .arg("-f")
            .arg("-q")
            .arg(&td1.devnode())
            .status()
            .unwrap();

        let data_usage_5 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert!(data_usage_5 - data_usage_4 > 32usize * data_usage_1);

        ss.destroy(&dm, &tp).unwrap();
        td1.destroy(&dm, &tp).unwrap();
        td.destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }


    #[test]
    fn loop_test_basic() {
        test_with_spec(1, test_basic);
    }

    #[test]
    fn loop_test_zero_size() {
        test_with_spec(1, test_zero_size);
    }

    #[test]
    fn loop_test_setup_without_new() {
        test_with_spec(1, test_setup_without_new);
    }

    #[test]
    fn loop_test_snapshot() {
        test_with_spec(1, test_snapshot);
    }

    #[test]
    fn loop_test_snapshot_usage() {
        test_with_spec(1, test_snapshot_usage);
    }

    #[test]
    fn loop_test_filesystem() {
        test_with_spec(1, test_filesystem);
    }
}
