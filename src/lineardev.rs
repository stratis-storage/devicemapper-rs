// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::path::PathBuf;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::result::{DmResult, DmError, ErrorEnum};
use super::segment::Segment;
use super::shared::{DmDevice, device_setup, table_reload};
use super::types::{DevId, DmName, DmUuid, Sectors, TargetLine, TargetParams, TargetTypeBuf};


/// LinearDev target params
#[derive(Debug, PartialEq)]
pub struct LinearDevTargetParams {
    /// The device for this segment
    pub device: Device,
    /// The offset on this device where the segment starts
    pub physical_start_offset: Sectors,
}

impl LinearDevTargetParams {
    /// Make a new LinearDevTargetParams struct
    pub fn new(device: Device, physical_start_offset: Sectors) -> LinearDevTargetParams {
        LinearDevTargetParams {
            device: device,
            physical_start_offset: physical_start_offset,
        }
    }
}

impl fmt::Display for LinearDevTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.device, *self.physical_start_offset)
    }
}

impl TargetParams for LinearDevTargetParams {}


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

    fn teardown(self, dm: &DM) -> DmResult<()> {
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
    pub fn setup(dm: &DM,
                 name: &DmName,
                 uuid: Option<&DmUuid>,
                 segments: &[Segment])
                 -> DmResult<LinearDev> {
        if segments.is_empty() {
            return Err(DmError::Dm(ErrorEnum::Invalid,
                                   "linear device must have at least one segment".into()));
        }

        let table = LinearDev::dm_table(segments);
        let dev_info = device_setup(dm, name, uuid, &table)?;

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
    fn dm_table(segments: &[Segment]) -> Vec<TargetLine<LinearDevTargetParams>> {
        assert_ne!(segments.len(), 0);

        let mut table = Vec::new();
        let mut logical_start_offset = Sectors(0);
        for segment in segments {
            let (physical_start_offset, length) = (segment.start, segment.length);
            let line = TargetLine {
                start: logical_start_offset,
                length: length,
                target_type: TargetTypeBuf::new("linear".into()).expect("< length limit"),
                params: LinearDevTargetParams::new(segment.device, physical_start_offset),
            };
            debug!("dmtable line : {:?}", line);
            table.push(line);
            logical_start_offset += length;
        }

        table
    }

    /// Set the segments for this linear device.
    /// Warning: It is the client's responsibility to make sure the designated
    /// segments are compatible with the device's existing segments.
    /// If they are not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_segments(&mut self, dm: &DM, segments: &[Segment]) -> DmResult<()> {
        if segments.is_empty() {
            return Err(DmError::Dm(ErrorEnum::Invalid,
                                   "linear device must have at least one segment".into()));
        }

        let table = LinearDev::dm_table(segments);
        table_reload(dm, &DevId::Name(self.name()), &table)?;
        self.segments = segments.to_vec();
        Ok(())
    }

    /// Set the name for this LinearDev.
    pub fn set_name(&mut self, dm: &DM, name: &DmName) -> DmResult<()> {
        if self.name() == name {
            return Ok(());
        }
        dm.device_rename(self.name(), &DevId::Name(name))?;
        self.dev_info = Box::new(dm.device_status(&DevId::Name(name))?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::OpenOptions;
    use std::path::Path;

    use super::super::device::Device;
    use super::super::loopbacked::{blkdev_size, devnode_to_devno, test_with_spec};

    use super::*;

    /// Verify that a new linear dev with 0 segments fails.
    fn test_empty(_paths: &[&Path]) -> () {
        assert!(LinearDev::setup(&DM::new().unwrap(),
                                 DmName::new("new").expect("valid format"),
                                 None,
                                 &[])
                        .is_err());
    }

    /// Verify that id rename succeeds.
    fn test_rename_id(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap());
        let mut ld = LinearDev::setup(&dm,
                                      DmName::new(name).expect("valid format"),
                                      None,
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
        let mut ld = LinearDev::setup(&dm,
                                      DmName::new(name).expect("valid format"),
                                      None,
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
        let ld = LinearDev::setup(&dm,
                                  DmName::new(name).expect("valid format"),
                                  None,
                                  segments)
                .unwrap();
        assert_eq!(dm.table_status(&DevId::Name(DmName::new(name).expect("valid format")),
                                   DmFlags::DM_STATUS_TABLE)
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
        let ld = LinearDev::setup(&dm,
                                  DmName::new(name).expect("valid format"),
                                  None,
                                  segments)
                .unwrap();
        assert!(LinearDev::setup(&dm,
                                 DmName::new(name).expect("valid format"),
                                 None,
                                 &[Segment::new(dev, Sectors(1), Sectors(1))])
                        .is_err());
        assert!(LinearDev::setup(&dm,
                                 DmName::new(name).expect("valid format"),
                                 None,
                                 segments)
                        .is_ok());
        assert_eq!(table,
                   dm.table_status(&DevId::Name(DmName::new(name).expect("valid format")),
                                   DmFlags::DM_STATUS_TABLE)
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
        let ld = LinearDev::setup(&dm,
                                  DmName::new("name").expect("valid format"),
                                  None,
                                  segments)
                .unwrap();
        let ld2 = LinearDev::setup(&dm,
                                   DmName::new("ersatz").expect("valid format"),
                                   None,
                                   segments);
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
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1)),
                         Segment::new(dev, Sectors(1), Sectors(1))];
        let table = LinearDev::dm_table(segments);
        let ld = LinearDev::setup(&dm,
                                  DmName::new(name).expect("valid format"),
                                  None,
                                  segments)
                .unwrap();
        assert_eq!(table,
                   dm.table_status(&DevId::Name(DmName::new(name).expect("valid format")),
                                   DmFlags::DM_STATUS_TABLE)
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
