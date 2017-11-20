// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::path::PathBuf;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DM_SUSPEND, DmFlags};
use super::result::{DmError, DmResult, ErrorEnum};
use super::shared::{DmDevice, device_create, device_exists, device_setup, message, table_reload};
use super::thindevid::ThinDevId;
use super::thinpooldev::ThinPoolDev;
use super::types::{DevId, DmName, DmUuid, Sectors, TargetLine, TargetTypeBuf};

/// DM construct for a thin block device
#[derive(Debug)]
pub struct ThinDev {
    dev_info: Box<DeviceInfo>,
    thin_id: ThinDevId,
    size: Sectors,
    thinpool: Device,
}

impl DmDevice for ThinDev {
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
        self.size
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
/// Thin device status.
pub enum ThinStatus {
    /// Thin device is good. Includes number of mapped sectors, and
    /// highest mapped sector.
    Good((Sectors, Option<Sectors>)),
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
               thin_pool: &ThinPoolDev,
               thin_id: ThinDevId,
               length: Sectors)
               -> DmResult<ThinDev> {

        message(dm, thin_pool, &format!("create_thin {}", thin_id))?;

        if device_exists(dm, name)? {
            let err_msg = "Uncreated device should not be known to kernel";
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg.into()));
        }

        let thin_pool_device = thin_pool.device();
        let table = ThinDev::dm_table(thin_pool_device, thin_id, length);
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
                 thin_pool: &ThinPoolDev,
                 thin_id: ThinDevId,
                 length: Sectors)
                 -> DmResult<ThinDev> {

        let thin_pool_device = thin_pool.device();
        let table = ThinDev::dm_table(thin_pool_device, thin_id, length);
        let dev_info = device_setup(dm, name, uuid, &table)?;

        Ok(ThinDev {
               dev_info: Box::new(dev_info),
               thin_id: thin_id,
               size: length,
               thinpool: thin_pool_device,
           })
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
        dm.device_suspend(&source_id, DM_SUSPEND)?;
        message(dm,
                thin_pool,
                &format!("create_snap {} {}", snapshot_thin_id, self.thin_id))?;
        dm.device_suspend(&source_id, DmFlags::empty())?;
        let dev_info = Box::new(device_create(dm,
                                              snapshot_name,
                                              None,
                                              &ThinDev::dm_table(thin_pool.device(),
                                                                 snapshot_thin_id,
                                                                 self.size()))?);
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
    fn dm_table(thin_pool: Device, thin_id: ThinDevId, length: Sectors) -> Vec<TargetLine> {
        let params = format!("{} {}", thin_pool, thin_id);
        vec![TargetLine {
                 start: Sectors::default(),
                 length: length,
                 target_type: TargetTypeBuf::new("thin".into()).expect("< length limit"),
                 params: params,
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

        let status_line = &table.first().expect("assertion above holds").params;
        if status_line.starts_with("Fail") {
            return Ok(ThinStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(status_vals.len() >= 2,
                "Kernel must return at least 2 values from thin pool status");

        let count = status_vals[0]
            .parse::<u64>()
            .expect("Kernel always returns a parseable u64 for sector count");

        let highest = if count == 0 {
            None
        } else {
            Some(Sectors(status_vals[1]
                             .parse::<u64>()
                             .expect("Kernel always returns a parseable u64 when count > 0")))
        };

        Ok(ThinStatus::Good((Sectors(count), highest)))
    }

    /// Extend the thin device's (virtual) size by the number of
    /// sectors given.
    pub fn extend(&mut self, dm: &DM, sectors: Sectors) -> DmResult<()> {
        let new_size = self.size + sectors;
        table_reload(dm,
                     &DevId::Name(self.name()),
                     &ThinDev::dm_table(self.thinpool, self.thin_id, new_size))?;
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

    use super::super::loopbacked::{blkdev_size, test_with_spec};
    use super::super::thinpooldev::minimal_thinpool;

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
                             &tp,
                             ThinDevId::new_u64(0).expect("is below limit"),
                             Sectors(0))
                        .is_err());
        tp.teardown(&dm).unwrap();
    }

    /// Verify that setting up a thin device without first calling new()
    /// causes an error.
    fn test_setup_without_new(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        let td_size = MIN_THIN_DEV_SIZE;
        assert!(ThinDev::setup(&dm,
                               &DmName::new("name").expect("is valid DM name"),
                               None,
                               &tp,
                               ThinDevId::new_u64(0).expect("is below limit"),
                               td_size)
                        .is_err());

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
        let td = ThinDev::new(&dm, &id, None, &tp, thin_id, td_size).unwrap();

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
        assert!(ThinDev::new(&dm, &id, None, &tp, thin_id, td_size).is_err());

        // Setting up the just created thin dev succeeds.
        assert!(ThinDev::setup(&dm, &id, None, &tp, thin_id, td_size).is_ok());

        // Setting up the just created thin dev once more succeeds.
        assert!(ThinDev::setup(&dm, &id, None, &tp, thin_id, td_size).is_ok());

        // Teardown the thindev, then set it back up.
        td.teardown(&dm).unwrap();
        let td = ThinDev::setup(&dm, &id, None, &tp, thin_id, td_size);
        assert!(td.is_ok());

        td.unwrap().destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify success when taking a snapshot of a ThinDev.  Check that
    /// the size of the snapshot is the same as the source.
    fn test_snapshot(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);
        let td_size = MIN_THIN_DEV_SIZE;
        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        // Create new ThinDev as source for snapshot
        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = DmName::new("name").expect("is valid DM name");
        let td = ThinDev::new(&dm, &thin_name, None, &tp, thin_id, td_size).unwrap();

        // Create a snapshot of the source
        let ss_id = ThinDevId::new_u64(1).expect("is below limit");
        let ss_name = DmName::new("snap_name").expect("is valid DM name");
        let ss = td.snapshot(&dm, &tp, ss_name, ss_id).unwrap();

        // Verify the source and the snapshot are the same size.
        assert_eq!(td.size(), ss.size());

        ss.destroy(&dm, &tp).unwrap();
        td.destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify no failures when creating a thindev from a pool, mounting a
    /// filesystem on the thin device, and writing to that filesystem.
    fn test_filesystem(paths: &[&Path]) -> () {
        assert!(paths.len() > 0);

        let dm = DM::new().unwrap();
        let tp = minimal_thinpool(&dm, paths[0]);

        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = DmName::new("name").expect("is valid DM name");
        let td = ThinDev::new(&dm, &thin_name, None, &tp, thin_id, tp.size()).unwrap();

        Command::new("mkfs.xfs")
            .arg("-f")
            .arg("-q")
            .arg(&td.devnode())
            .status()
            .unwrap();

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
        td.teardown(&dm).unwrap();
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
    fn loop_test_filesystem() {
        test_with_spec(1, test_filesystem);
    }
}
