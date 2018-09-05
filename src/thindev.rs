// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::DM;
use super::dm_flags::{DmCookie, DmFlags};
use super::dm_options::DmOptions;
use super::result::{DmError, DmResult, ErrorEnum};
use super::shared::{
    device_create, device_exists, device_match, message, parse_device, DmDevice, TargetLine,
    TargetParams, TargetTable,
};
use super::thindevid::ThinDevId;
use super::thinpooldev::ThinPoolDev;
use super::types::{DevId, DmName, DmUuid, Sectors, TargetTypeBuf};

const THIN_TARGET_NAME: &str = "thin";

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ThinTargetParams {
    pub pool: Device,
    pub thin_id: ThinDevId,
    pub external_origin_dev: Option<Device>,
}

impl ThinTargetParams {
    pub fn new(
        pool: Device,
        thin_id: ThinDevId,
        external_origin_dev: Option<Device>,
    ) -> ThinTargetParams {
        ThinTargetParams {
            pool,
            thin_id,
            external_origin_dev,
        }
    }
}

impl fmt::Display for ThinTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", THIN_TARGET_NAME, self.param_str())
    }
}

impl FromStr for ThinTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<ThinTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();
        let len = vals.len();
        if len < 3 || len > 4 {
            let err_msg = format!(
                "expected 3 or 4 values in params string \"{}\", found {}",
                s, len
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        if vals[0] != THIN_TARGET_NAME {
            let err_msg = format!(
                "Expected a thin target entry but found target type {}",
                vals[0]
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        Ok(ThinTargetParams::new(
            parse_device(vals[1])?,
            vals[2].parse::<ThinDevId>()?,
            if len == 3 {
                None
            } else {
                Some(parse_device(vals[3])?)
            },
        ))
    }
}

impl TargetParams for ThinTargetParams {
    fn param_str(&self) -> String {
        match self.external_origin_dev {
            None => format!("{} {}", self.pool, self.thin_id),
            Some(dev) => format!("{} {} {}", self.pool, self.thin_id, dev),
        }
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(THIN_TARGET_NAME.into()).expect("THIN_TARGET_NAME is valid")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ThinDevTargetTable {
    pub table: TargetLine<ThinTargetParams>,
}

impl ThinDevTargetTable {
    pub fn new(start: Sectors, length: Sectors, params: ThinTargetParams) -> ThinDevTargetTable {
        ThinDevTargetTable {
            table: TargetLine::new(start, length, params),
        }
    }
}

impl fmt::Display for ThinDevTargetTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let table = &self.table;
        writeln!(f, "{} {} {}", *table.start, *table.length, table.params)
    }
}

impl TargetTable for ThinDevTargetTable {
    fn from_raw_table(
        table: &[(Sectors, Sectors, TargetTypeBuf, String)],
    ) -> DmResult<ThinDevTargetTable> {
        if table.len() != 1 {
            let err_msg = format!(
                "ThinDev table should have exactly one line, has {} lines",
                table.len()
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }
        let line = table.first().expect("table.len() == 1");
        Ok(ThinDevTargetTable::new(
            line.0,
            line.1,
            format!("{} {}", line.2.to_string(), line.3).parse::<ThinTargetParams>()?,
        ))
    }

    fn to_raw_table(&self) -> Vec<(Sectors, Sectors, TargetTypeBuf, String)> {
        to_raw_table_unique!(self)
    }
}

/// DM construct for a thin block device
#[derive(Debug)]
pub struct ThinDev {
    dev_info: Box<DeviceInfo>,
    table: ThinDevTargetTable,
}

impl DmDevice<ThinDevTargetTable> for ThinDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    // This method is incomplete. It is expected that it will be refined so
    // that it will return true in more cases, i.e., to be less stringent.
    fn equivalent_tables(left: &ThinDevTargetTable, right: &ThinDevTargetTable) -> DmResult<bool> {
        Ok(left == right)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.table.table.length
    }

    fn table(&self) -> &ThinDevTargetTable {
        table!(self)
    }

    fn teardown(&mut self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), &DmOptions::new())?;
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
    pub fn new(
        nr_mapped_sectors: Sectors,
        highest_mapped_sector: Option<Sectors>,
    ) -> ThinDevWorkingStatus {
        ThinDevWorkingStatus {
            nr_mapped_sectors,
            highest_mapped_sector,
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
    pub fn new(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        length: Sectors,
        thin_pool: &ThinPoolDev,
        thin_id: ThinDevId,
    ) -> DmResult<ThinDev> {
        message(dm, thin_pool, &format!("create_thin {}", thin_id))?;

        if device_exists(dm, name)? {
            let err_msg = "Uncreated device should not be known to kernel";
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg.into()));
        }

        let thin_pool_device = thin_pool.device();
        let table = ThinDev::gen_default_table(length, thin_pool_device, thin_id);
        let dev_info = device_create(
            dm,
            name,
            uuid,
            &table,
            &DmOptions::new().set_cookie(DmCookie::DM_UDEV_PRIMARY_SOURCE_FLAG),
        )?;

        Ok(ThinDev {
            dev_info: Box::new(dev_info),
            table,
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
    pub fn setup(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        length: Sectors,
        thin_pool: &ThinPoolDev,
        thin_id: ThinDevId,
    ) -> DmResult<ThinDev> {
        let thin_pool_device = thin_pool.device();
        let table = ThinDev::gen_default_table(length, thin_pool_device, thin_id);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = ThinDev {
                dev_info: Box::new(dev_info),
                table,
            };
            device_match(dm, &dev, uuid)?;
            dev
        } else {
            let dev_info = device_create(
                dm,
                name,
                uuid,
                &table,
                &DmOptions::new().set_cookie(DmCookie::DM_UDEV_PRIMARY_SOURCE_FLAG),
            )?;
            ThinDev {
                dev_info: Box::new(dev_info),
                table,
            }
        };
        Ok(dev)
    }

    /// Create a snapshot of a ThinDev.  Once created a snapshot
    /// is the same as any other thin provisioned device.  There is
    /// no need to track any connection between the source and the
    /// snapshot.
    pub fn snapshot(
        &self,
        dm: &DM,
        snapshot_name: &DmName,
        snapshot_uuid: Option<&DmUuid>,
        thin_pool: &ThinPoolDev,
        snapshot_thin_id: ThinDevId,
    ) -> DmResult<ThinDev> {
        let source_id = DevId::Name(self.name());
        dm.device_suspend(&source_id, &DmOptions::new().set_flags(DmFlags::DM_SUSPEND))?;
        message(
            dm,
            thin_pool,
            &format!(
                "create_snap {} {}",
                snapshot_thin_id, self.table.table.params.thin_id
            ),
        )?;
        dm.device_suspend(&source_id, &DmOptions::new())?;
        let table = ThinDev::gen_default_table(self.size(), thin_pool.device(), snapshot_thin_id);
        let dev_info = Box::new(device_create(
            dm,
            snapshot_name,
            snapshot_uuid,
            &table,
            &DmOptions::new().set_cookie(DmCookie::DM_UDEV_PRIMARY_SOURCE_FLAG),
        )?);
        Ok(ThinDev { dev_info, table })
    }

    /// Generate a table to be passed to DM. The format of the table
    /// entries is:
    /// <start (0)> <length> "thin" <thin device specific string>
    /// where the thin device specific string has the format:
    /// <thinpool maj:min> <thin_id>
    /// There is exactly one entry in the table.
    /// Various defaults are hard coded in the method.
    fn gen_default_table(
        length: Sectors,
        thin_pool: Device,
        thin_id: ThinDevId,
    ) -> ThinDevTargetTable {
        ThinDevTargetTable::new(
            Sectors::default(),
            length,
            ThinTargetParams::new(thin_pool, thin_id, None),
        )
    }

    /// return the thin id of the linear device
    pub fn id(&self) -> ThinDevId {
        self.table.table.params.thin_id
    }

    /// Get the current status of the thin device.
    pub fn status(&self, dm: &DM) -> DmResult<ThinStatus> {
        let (_, table) = dm.table_status(&DevId::Name(self.name()), &DmOptions::new())?;

        assert_eq!(
            table.len(),
            1,
            "Kernel must return 1 line table for thin status"
        );

        let status_line = &table.first().expect("assertion above holds").3;
        if status_line.starts_with("Fail") {
            return Ok(ThinStatus::Fail);
        }

        let status_vals = status_line.split(' ').collect::<Vec<_>>();
        assert!(
            status_vals.len() >= 2,
            "Kernel must return at least 2 values from thin pool status"
        );

        let count = status_vals[0]
            .parse::<u64>()
            .map(Sectors)
            .expect("Kernel always returns a parseable u64 for sector count");

        let highest = if count == Sectors(0) {
            None
        } else {
            Some(Sectors(status_vals[1].parse::<u64>().expect(
                "Kernel always returns a parseable u64 when count > 0",
            )))
        };

        Ok(ThinStatus::Working(Box::new(ThinDevWorkingStatus::new(
            count, highest,
        ))))
    }

    /// Set the table for the thin device's target
    pub fn set_table(&mut self, dm: &DM, table: TargetLine<ThinTargetParams>) -> DmResult<()> {
        let table = ThinDevTargetTable::new(table.start, table.length, table.params);
        self.suspend(dm, false)?;
        self.table_load(dm, &table)?;
        self.resume(dm)?;

        self.table = table;
        Ok(())
    }

    /// Tear down the DM device, and also delete resources associated
    /// with its thin id from the thinpool.
    pub fn destroy(&mut self, dm: &DM, thin_pool: &ThinPoolDev) -> DmResult<()> {
        let thin_id = self.table.table.params.thin_id;
        self.teardown(dm)?;
        message(dm, thin_pool, &format!("delete {}", thin_id))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;
    use std::fs::{canonicalize, OpenOptions};
    use std::io::Write;
    use std::path::Path;

    use libudev;
    use nix::mount::{mount, umount2, MntFlags, MsFlags};
    use tempfile;
    use uuid::Uuid;

    use super::super::consts::IEC;
    use super::super::loopbacked::{blkdev_size, test_with_spec};
    use super::super::shared::DmDevice;
    use super::super::test_lib::{
        test_name, test_string, test_uuid, udev_settle, xfs_create_fs, xfs_set_uuid,
    };
    use super::super::thinpooldev::{minimal_thinpool, ThinPoolStatus};
    use super::super::types::DataBlocks;

    use super::super::errors::{Error, ErrorKind};

    use super::*;

    const MIN_THIN_DEV_SIZE: Sectors = Sectors(1);

    // Return a hashmap of key-value pairs for udev entry.
    fn get_udev_db_entry(dev_node_search: &PathBuf) -> Option<HashMap<String, String>> {
        // Takes a libudev device entry and returns the properties as a HashMap.
        fn device_as_map(device: &libudev::Device) -> HashMap<String, String> {
            let rc: HashMap<_, _> = device
                .properties()
                .map(|i| {
                    (
                        String::from(i.name().to_str().unwrap()),
                        String::from(i.value().to_str().unwrap()),
                    )
                })
                .collect();
            rc
        }

        let context = libudev::Context::new().unwrap();
        let mut enumerator = libudev::Enumerator::new(&context).unwrap();
        enumerator.match_subsystem("block").unwrap();

        let result = enumerator
            .scan_devices()
            .unwrap()
            .find(|x| x.devnode().map_or(false, |d| dev_node_search == d))
            .map_or(None, |dev| Some(device_as_map(&dev)));
        result
    }

    /// Verify that specifying a size of 0 Sectors will cause a failure.
    fn test_zero_size(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);

        assert!(
            ThinDev::new(
                &dm,
                &test_name("name").expect("is valid DM name"),
                None,
                Sectors(0),
                &tp,
                ThinDevId::new_u64(0).expect("is below limit")
            ).is_err()
        );

        udev_settle().unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify that setting up a thin device without first calling new()
    /// causes an error. The underlying reason is that the thin pool hasn't
    /// been informed about the thin device by messaging the value of the
    /// thin id.
    fn test_setup_without_new(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);

        let td_size = MIN_THIN_DEV_SIZE;
        assert!(match ThinDev::setup(
            &dm,
            &test_name("name").expect("is valid DM name"),
            None,
            td_size,
            &tp,
            ThinDevId::new_u64(0).expect("is below limit")
        ) {
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
    /// it has been torn down. Verify that it is possible to suspend and resume
    /// the device.
    fn test_basic(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);
        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let id = test_name("name").expect("is valid DM name");

        let td_size = MIN_THIN_DEV_SIZE;
        let mut td = ThinDev::new(&dm, &id, None, td_size, &tp, thin_id).unwrap();

        udev_settle().unwrap();

        let table = ThinDev::read_kernel_table(&dm, &DevId::Name(td.name()))
            .unwrap()
            .table;

        assert_eq!(table.params.pool, tp.device());
        assert_eq!(table.params.thin_id, thin_id);

        assert!(match td.status(&dm).unwrap() {
            ThinStatus::Fail => false,
            _ => true,
        });

        assert_eq!(
            blkdev_size(&OpenOptions::new().read(true).open(td.devnode()).unwrap()),
            td_size.bytes()
        );

        // New thindev w/ same id fails.
        assert!(match ThinDev::new(&dm, &id, None, td_size, &tp, thin_id) {
            Err(DmError::Core(Error(ErrorKind::IoctlError(_), _))) => true,
            _ => false,
        });

        // Verify that the device of that name does exist.
        assert!(device_exists(&dm, &id).unwrap());

        // Setting up the just created thin dev succeeds.
        assert!(ThinDev::setup(&dm, &id, None, td_size, &tp, thin_id).is_ok());
        udev_settle().unwrap();

        // Setting up the just created thin dev once more succeeds.
        assert!(ThinDev::setup(&dm, &id, None, td_size, &tp, thin_id).is_ok());

        // Teardown the thindev, then set it back up.
        td.teardown(&dm).unwrap();
        let mut td = ThinDev::setup(&dm, &id, None, td_size, &tp, thin_id).unwrap();
        udev_settle().unwrap();

        td.suspend(&dm, false).unwrap();
        td.suspend(&dm, false).unwrap();
        td.resume(&dm).unwrap();
        td.resume(&dm).unwrap();

        td.destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Test thin device create, load, and snapshot and make sure that all is well with udev
    /// db and symlink generation.
    fn test_udev_userspace(paths: &[&Path]) -> () {
        // Make sure we are meeting all our expectations in user space with regards to udev
        // handling.
        fn validate(path_uuid: &Uuid, devnode: &PathBuf) {
            udev_settle().unwrap();

            // Make sure the uuid symlink was created
            let symlink = PathBuf::from(format!("/dev/disk/by-uuid/{}", path_uuid));

            assert!(symlink.exists());

            // Make sure the symlink points to devnode
            let uuid_sym = canonicalize(symlink).unwrap();

            assert_eq!(*devnode, uuid_sym);

            // Check the udev db to see what it's showing for the device, make sure SYSTEMD_READY=0
            // is not present
            let entry = get_udev_db_entry(devnode).unwrap();
            assert!(!entry.contains_key("SYSTEMD_READY"));
        }

        // Set the FS with devnode to a new auto generated UUID
        fn set_new_fs_uuid(devnode: &PathBuf) -> Uuid {
            // Tmp mount & umount to complete the XFS transactions so that we can change the UUID
            let tmp_dir = tempfile::Builder::new()
                .prefix(&test_string("test_udev_userspace_mp"))
                .tempdir()
                .unwrap();
            mount(
                Some(devnode),
                tmp_dir.path(),
                Some("xfs"),
                MsFlags::empty(),
                None as Option<&str>,
            ).unwrap();
            umount2(tmp_dir.path(), MntFlags::MNT_DETACH).unwrap();

            // Set the fs UUID to something new
            let new_uuid = Uuid::new_v4();
            xfs_set_uuid(devnode, &new_uuid).unwrap();
            new_uuid
        }

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);
        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let id = test_name("udev_test_thin_dev").expect("is valid DM name");

        let mut td = ThinDev::new(&dm, &id, None, tp.size(), &tp, thin_id).unwrap();
        udev_settle().unwrap();

        // Create the XFS FS on top of the thin device
        xfs_create_fs(&td.devnode()).unwrap();

        // Travis CI is old doesn't support setting uuid during FS creation.
        let uuid = set_new_fs_uuid(&td.devnode());

        validate(&uuid, &td.devnode());

        // Teardown the thindev, then set it back up and make sure all is well with udev
        td.teardown(&dm).unwrap();
        td = ThinDev::setup(&dm, &id, None, tp.size(), &tp, thin_id).unwrap();
        validate(&uuid, &td.devnode());

        // Create a snapshot and make sure we get correct actions in user space WRT udev
        let ss_id = ThinDevId::new_u64(1).expect("is below limit");
        let ss_name = test_name("snap_name").expect("is valid DM name");
        let mut ss = td.snapshot(&dm, &ss_name, None, &tp, ss_id).unwrap();
        udev_settle().unwrap();

        let ss_new_uuid = set_new_fs_uuid(&ss.devnode());

        // Validate that the symlink for original and snapshot are correct
        validate(&ss_new_uuid, &ss.devnode());
        validate(&uuid, &td.devnode());

        // Tear everything down
        ss.destroy(&dm, &tp).unwrap();
        td.destroy(&dm, &tp).unwrap();
        tp.teardown(&dm).unwrap();
    }

    /// Verify success when taking a snapshot of a ThinDev.  Check that
    /// the size of the snapshot is the same as the source.
    /// Verify that empty thindev has no data usage.
    fn test_snapshot(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);
        let td_size = MIN_THIN_DEV_SIZE;
        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);

        let orig_data_usage = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };

        assert_eq!(orig_data_usage, DataBlocks(0));

        // Create new ThinDev as source for snapshot
        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = test_name("name").expect("is valid DM name");
        let mut td = ThinDev::new(&dm, &thin_name, None, td_size, &tp, thin_id).unwrap();
        udev_settle().unwrap();

        let data_usage_1 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };

        assert_eq!(data_usage_1, DataBlocks(0));

        // Create a snapshot of the source
        let ss_id = ThinDevId::new_u64(1).expect("is below limit");
        let ss_name = test_name("snap_name").expect("is valid DM name");
        let mut ss = td.snapshot(&dm, &ss_name, None, &tp, ss_id).unwrap();
        udev_settle().unwrap();

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
        let mut tp = minimal_thinpool(&dm, paths[0]);

        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = test_name("name").expect("is valid DM name");
        let mut td = ThinDev::new(&dm, &thin_name, None, tp.size(), &tp, thin_id).unwrap();
        udev_settle().unwrap();

        let orig_data_usage = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(orig_data_usage, DataBlocks(0));

        xfs_create_fs(&td.devnode()).unwrap();

        let data_usage_1 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert!(data_usage_1 > DataBlocks(0));

        let tmp_dir = tempfile::Builder::new()
            .prefix(&test_string("stratis_testing"))
            .tempdir()
            .unwrap();
        mount(
            Some(&td.devnode()),
            tmp_dir.path(),
            Some("xfs"),
            MsFlags::empty(),
            None as Option<&str>,
        ).unwrap();

        for i in 0..100 {
            let file_path = tmp_dir.path().join(format!("stratis_test{}.txt", i));
            writeln!(
                &OpenOptions::new()
                    .create(true)
                    .write(true)
                    .open(file_path)
                    .unwrap(),
                "data"
            ).unwrap();
        }
        umount2(tmp_dir.path(), MntFlags::MNT_DETACH).unwrap();

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
        let mut tp = minimal_thinpool(&dm, paths[0]);

        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = test_name("name").expect("is valid DM name");
        let mut td =
            ThinDev::new(&dm, &thin_name, None, Sectors(2 * IEC::Mi), &tp, thin_id).unwrap();
        udev_settle().unwrap();

        let orig_data_usage = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(orig_data_usage, DataBlocks(0));

        xfs_create_fs(&td.devnode()).unwrap();

        let data_usage_1 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert!(data_usage_1 > DataBlocks(0));

        // Create a snapshot of the source
        let ss_id = ThinDevId::new_u64(1).expect("is below limit");
        let ss_name = test_name("snap_name").expect("is valid DM name");
        let ss_uuid = test_uuid("snap_uuid").expect("is valid DM uuid");
        let mut ss = td.snapshot(&dm, &ss_name, Some(&ss_uuid), &tp, ss_id)
            .unwrap();
        udev_settle().unwrap();

        let data_usage_2 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(data_usage_2, data_usage_1);

        xfs_set_uuid(&ss.devnode(), &Uuid::new_v4()).unwrap();

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
        let thin_name = test_name("name1").expect("is valid DM name");
        let mut td1 =
            ThinDev::new(&dm, &thin_name, None, Sectors(2 * IEC::Gi), &tp, thin_id).unwrap();
        udev_settle().unwrap();

        let data_usage_4 = match tp.status(&dm).unwrap() {
            ThinPoolStatus::Working(ref status) => status.usage.used_data,
            ThinPoolStatus::Fail => panic!("failed to get thinpool status"),
        };
        assert_eq!(data_usage_4, data_usage_3);

        xfs_create_fs(&td1.devnode()).unwrap();

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

    /// Verify that destroy() actually deallocates the space from the
    /// thinpool, by attempting to reinstantiate it using the same thin id and
    /// verifying that it fails.
    fn test_thindev_destroy(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let mut tp = minimal_thinpool(&dm, paths[0]);

        let thin_id = ThinDevId::new_u64(0).expect("is below limit");
        let thin_name = test_name("name").expect("is valid DM name");

        let mut td = ThinDev::new(&dm, &thin_name, None, tp.size(), &tp, thin_id).unwrap();
        td.teardown(&dm).unwrap();

        // This should work
        let mut td = ThinDev::setup(&dm, &thin_name, None, tp.size(), &tp, thin_id).unwrap();
        td.destroy(&dm, &tp).unwrap();

        // This should fail
        assert!(
            match ThinDev::setup(&dm, &thin_name, None, tp.size(), &tp, thin_id) {
                Err(DmError::Core(Error(ErrorKind::IoctlError(_), _))) => true,
                _ => false,
            }
        );

        tp.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_basic() {
        test_with_spec(1, test_basic);
    }

    #[test]
    fn loop_test_basic_udev() {
        test_with_spec(1, test_udev_userspace);
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

    #[test]
    pub fn loop_test_thindev_destroy() {
        test_with_spec(1, test_thindev_destroy);
    }
}
