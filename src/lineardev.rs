// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::path::PathBuf;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DevId, DmFlags, DmName};
use super::errors::{Error, ErrorKind, Result};
use super::segment::Segment;
use super::shared::{DmDevice, device_create, device_exists, device_setup, table_reload};
use super::types::{Sectors, TargetLine};
use super::util::chain_error;


/// A DM construct of combined Segments
#[derive(Debug)]
pub struct LinearDev {
    /// Data about the device
    dev_info: Box<DeviceInfo>,
    segments: Vec<Segment>,
}

impl DmDevice for LinearDev {
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
        self.segments.iter().map(|s| s.length).sum()
    }

    fn teardown(self, dm: &DM) -> Result<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        Ok(())
    }
}

/// Use DM to concatenate a list of segments together into a
/// linear block device of continuous sectors.
impl LinearDev {
    /// Construct a new block device by concatenating the given segments
    /// into linear space.
    /// If the device is already known to the kernel, just verifies that the
    /// segments argument passed exactly matches the kernel data.
    ///
    /// Warning: If the segments overlap, this method will succeed. However,
    /// the behavior of the linear device in that case should be treated as
    /// undefined.
    ///
    /// Note: A linear device is just a mapping in the kernel from locations
    /// in that device to locations on other devices which are specified by
    /// their device number. There is usually a device node so that data can
    /// be read from and written to the device. Other than that, it really
    /// has no existence. Consequently, there is no conflict in overloading
    /// this method to mean both "make a wholly new device" and "establish
    /// the existence of the requested device". Of course, a linear device
    /// is usually expected to hold data, so it is important to get the
    /// mapping just right.
    pub fn setup(name: &DmName, dm: &DM, segments: &[Segment]) -> Result<LinearDev> {
        let dev_info =
            chain_error(|| {
                if segments.is_empty() {
                    let err_msg = "linear device must have at least one segment";
                    return Err(Error::from_kind(ErrorKind::InvalidArgument(err_msg.into()).into()));
                }
                let table = LinearDev::dm_table(segments);
                if device_exists(dm, name)? {
                    device_setup(dm, &DevId::Name(name), &table)
                } else {
                    device_create(dm, name, &table)
                }
            },
                        || format!("Failed to create or setup linear device {}", name).into())?;

        DM::wait_for_dm();
        Ok(LinearDev {
               dev_info: Box::new(dev_info),
               segments: segments.to_vec(),
           })
    }

    /// Return a reference to the segments that back this linear device.
    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    /// Generate a table to be passed to DM.  The format of the table entries
    /// is:
    /// <logical start offset> <length> "linear" <linear-specific string>
    /// where the linear-specific string has the format:
    /// <maj:min> <physical start offset>
    fn dm_table(segments: &[Segment]) -> Vec<TargetLine> {
        assert_ne!(segments.len(), 0);

        let mut table = Vec::new();
        let mut logical_start_offset = Sectors(0);
        for segment in segments {
            let (physical_start_offset, length) = (segment.start, segment.length);
            let line = (logical_start_offset,
                        length,
                        "linear".to_owned(),
                        format!("{} {}", segment.device, *physical_start_offset));
            debug!("dmtable line : {:?}", line);
            table.push(line);
            logical_start_offset += length;
        }

        table
    }

    /// Extend an existing LinearDev with additional new segments.
    /// In the event that the first segments in new_segs is contiguous with
    /// the device's last segment, these segments are coalesced into a single
    /// segment.
    /// Warning: If the segments overlap, either with each other or with the
    /// segments already in the device, this method will succeed. However,
    /// the behavior of the linear device in that case should be treated as
    /// undefined.
    pub fn extend(&mut self, dm: &DM, new_segs: &[Segment]) -> Result<()> {
        if new_segs.is_empty() {
            return Ok(());
        }

        // Last existing and first new may be contiguous. Coalesce into
        // a single Segment if so.
        let coalesced_new_first = {
            let old_last = self.segments
                .last_mut()
                .expect("every LinearDev must have at least one segment");
            let new_first = new_segs.first().expect("new_segs.is_empty() is false");
            if old_last.device == new_first.device &&
               (old_last.start + old_last.length == new_first.start) {
                old_last.length += new_first.length;
                true
            } else {
                false
            }
        };

        if coalesced_new_first {
            self.segments.extend(new_segs.iter().skip(1).cloned());
        } else {
            self.segments.extend(new_segs.iter().cloned());
        }

        let table = LinearDev::dm_table(&self.segments);
        chain_error(|| table_reload(dm, &DevId::Name(self.name()), &table),
                    || "Failed to extend linear device with new segments".into())?;
        Ok(())
    }

    /// Set the name for this LinearDev.
    pub fn set_name(&mut self, dm: &DM, name: &DmName) -> Result<()> {
        let old_name = self.name().to_owned();
        if old_name.as_ref() == name {
            return Ok(());
        }
        chain_error(|| {
                        dm.device_rename(&old_name, &DevId::Name(name))?;
                        Ok(self.dev_info = Box::new(dm.device_status(&DevId::Name(name))?))
                    },
                    || {
                        format!("Failed to rename device {} to {}", old_name.as_ref(), name).into()
                    })?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::OpenOptions;
    use std::path::Path;

    use super::super::device::Device;
    use super::super::dm::DM_STATUS_TABLE;
    use super::super::loopbacked::{blkdev_size, devnode_to_devno, test_with_spec};

    use super::*;

    /// Verify that a new linear dev with 0 segments fails.
    fn test_empty(_paths: &[&Path]) -> () {
        assert!(LinearDev::setup(DmName::new("new").expect("valid format"),
                                 &DM::new().unwrap(),
                                 &[])
                        .is_err());
    }

    /// Verify that id rename succeeds.
    fn test_rename_id(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap());
        let mut ld = LinearDev::setup(DmName::new(name).expect("valid format"),
                                      &dm,
                                      &[Segment::new(dev, Sectors(0), Sectors(1))])
                .unwrap();

        ld.set_name(&dm, DmName::new(name).expect("valid format"))
            .unwrap();
        assert_eq!(ld.name(), DmName::new(name).expect("valid format"));

        ld.teardown(&dm).unwrap();
    }

    /// Verify that after a rename, the device has the new name.
    fn test_rename(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap());
        let mut ld = LinearDev::setup(DmName::new(name).expect("valid format"),
                                      &dm,
                                      &[Segment::new(dev, Sectors(0), Sectors(1))])
                .unwrap();

        let new_name = "new_name";
        ld.set_name(&dm, DmName::new(new_name).expect("valid format"))
            .unwrap();
        assert_eq!(ld.name(), DmName::new(new_name).expect("valid format"));

        ld.teardown(&dm).unwrap();
    }

    /// Verify that passing the same segments two times gets two segments.
    /// Verify that the size of the devnode is the size of the sum of the
    /// ranges of the segments.
    fn test_duplicate_segments(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap());
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1)),
                         Segment::new(dev, Sectors(0), Sectors(1))];
        let range: Sectors = segments.iter().map(|s| s.length).sum();
        let count = segments.len();
        let ld = LinearDev::setup(DmName::new(name).expect("valid format"), &dm, segments).unwrap();
        assert_eq!(dm.table_status(&DevId::Name(DmName::new(name).expect("valid format")),
                                   DM_STATUS_TABLE)
                       .unwrap()
                       .1
                       .len(),
                   count);
        assert_eq!(blkdev_size(&OpenOptions::new()
                                    .read(true)
                                    .open(ld.devnode())
                                    .unwrap())
                           .sectors(),
                   range);

        ld.teardown(&dm).unwrap();
    }

    /// Verify that constructing a second dev with the same name succeeds
    /// only if it has the same list of segments.
    fn test_same_name(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap());
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1))];
        let table = LinearDev::dm_table(segments);
        let ld = LinearDev::setup(DmName::new(name).expect("valid format"), &dm, segments).unwrap();
        assert!(LinearDev::setup(DmName::new(name).expect("valid format"),
                                 &dm,
                                 &[Segment::new(dev, Sectors(1), Sectors(1))])
                        .is_err());
        assert!(LinearDev::setup(DmName::new(name).expect("valid format"), &dm, segments).is_ok());
        assert_eq!(table,
                   dm.table_status(&DevId::Name(DmName::new(name).expect("valid format")),
                                   DM_STATUS_TABLE)
                       .unwrap()
                       .1);

        ld.teardown(&dm).unwrap();
    }

    /// Verify constructing a second linear dev with the same segment succeeds.
    fn test_same_segment(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap());
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1))];
        let ld = LinearDev::setup(DmName::new("name").expect("valid format"), &dm, segments)
            .unwrap();
        let ld2 = LinearDev::setup(DmName::new("ersatz").expect("valid format"), &dm, segments);
        assert!(ld2.is_ok());

        ld2.unwrap().teardown(&dm).unwrap();
        ld.teardown(&dm).unwrap();
    }

    /// Verify that table status returns the expected table.
    fn test_table_status(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap());
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1))];
        let table = LinearDev::dm_table(segments);
        let ld = LinearDev::setup(DmName::new(name).expect("valid format"), &dm, segments).unwrap();
        assert_eq!(table,
                   dm.table_status(&DevId::Name(DmName::new(name).expect("valid format")),
                                   DM_STATUS_TABLE)
                       .unwrap()
                       .1);
        ld.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_duplicate_segments() {
        test_with_spec(1, test_duplicate_segments);
    }

    #[test]
    fn loop_test_empty() {
        test_with_spec(0, test_empty);
    }

    #[test]
    fn loop_test_rename() {
        test_with_spec(1, test_rename);
    }

    #[test]
    fn loop_test_rename_id() {
        test_with_spec(1, test_rename_id);
    }

    #[test]
    fn loop_test_same_name() {
        test_with_spec(1, test_same_name);
    }

    #[test]
    fn loop_test_segment() {
        test_with_spec(1, test_same_segment);
    }

    #[test]
    fn loop_test_table_status() {
        test_with_spec(1, test_table_status);
    }
}
