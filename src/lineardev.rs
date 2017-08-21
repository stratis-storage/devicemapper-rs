// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::fs::File;
use std::path::PathBuf;

use consts::DmFlags;
use deviceinfo::DeviceInfo;
use dm::{DM, DevId, DmName};
use result::{DmResult, DmError, ErrorEnum};
use segment::Segment;
use shared::{device_exists, table_load, table_reload};
use types::{Sectors, TargetLine};
use util::blkdev_size;

/// A DM construct of combined Segments
pub struct LinearDev {
    /// Data about the device
    dev_info: Box<DeviceInfo>,
    segments: Vec<Segment>,
}

impl fmt::Debug for LinearDev {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

/// Use DM to concatenate a list of segments together into a
/// linear block device of continuous sectors.
impl LinearDev {
    /// Construct a new block device by concatenating the given segments
    /// into linear space.  Use DM to reserve enough space for the stratis
    /// metadata on each DmDev.
    /// Warning: If the segments overlap, this method will succeed. However,
    /// the behavior of the linear device in that case should be treated as
    /// undefined.
    /// TODO: If the linear device already exists, verify that the kernel's
    /// model matches the segments argument.
    pub fn new(name: &DmName, dm: &DM, segments: Vec<Segment>) -> DmResult<LinearDev> {
        if segments.is_empty() {
            return Err(DmError::Dm(ErrorEnum::Invalid,
                                   "linear device must have at least one segment".into()));
        }

        let id = DevId::Name(name);
        let dev_info = if device_exists(dm, name)? {
            // TODO: Verify that kernel's model matches up with segments.
            Box::new(dm.device_status(&id)?)
        } else {
            dm.device_create(name, None, DmFlags::empty())?;
            let table = LinearDev::dm_table(&segments);
            Box::new(table_load(dm, &id, &table)?)
        };

        DM::wait_for_dm();
        Ok(LinearDev {
               dev_info: dev_info,
               segments: segments,
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
                        format!("{} {}", segment.device.dstr(), *physical_start_offset));
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
    pub fn extend(&mut self, new_segs: Vec<Segment>) -> DmResult<()> {
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
            self.segments.extend(new_segs.into_iter().skip(1));
        } else {
            self.segments.extend(new_segs);
        }

        let table = LinearDev::dm_table(&self.segments);
        table_reload(&DM::new()?, &DevId::Name(self.name()), &table)?;
        Ok(())
    }

    /// DM name - from the DeviceInfo struct
    pub fn name(&self) -> &DmName {
        self.dev_info.name()
    }

    /// Set the name for this LinearDev.
    pub fn set_name(&mut self, dm: &DM, name: &DmName) -> DmResult<()> {
        self.dev_info = Box::new(dm.device_rename(self.dev_info.name(), name, DmFlags::empty())?);

        Ok(())
    }

    /// Get the "x:y" device string for this LinearDev
    pub fn dstr(&self) -> String {
        self.dev_info.device().dstr()
    }

    /// return the total size of the linear device
    pub fn size(&self) -> DmResult<Sectors> {
        let f = File::open(self.devnode()?)?;
        Ok(blkdev_size(&f)?.sectors())
    }

    /// path of the device node
    pub fn devnode(&self) -> DmResult<PathBuf> {
        self.dev_info
            .device()
            .devnode()
            .ok_or_else(|| {
                            DmError::Dm(ErrorEnum::NotFound,
                                        "No path associated with dev_info".into())
                        })
    }

    /// Remove the device from DM
    pub fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::OpenOptions;
    use std::path::Path;
    use std::str::FromStr;

    use super::super::consts::DM_STATUS_TABLE;
    use super::super::device::Device;
    use super::super::loopbacked::test_with_spec;

    use super::*;

    /// Verify that a new linear dev with 0 segments fails.
    fn test_empty(_paths: &[&Path]) -> () {
        assert!(LinearDev::new("new", &DM::new().unwrap(), vec![]).is_err());
    }

    /// Verify that passing the same segments two times gets two segments.
    /// Verify that the size of the devnode is the size of the sum of the
    /// ranges of the segments.
    fn test_duplicate_segments(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from_str(&paths[0].to_string_lossy()).unwrap();
        let segments = vec![Segment::new(dev, Sectors(0), Sectors(1)),
                            Segment::new(dev, Sectors(0), Sectors(1))];
        let range: Sectors = segments.iter().map(|s| s.length).sum();
        let count = segments.len();
        let ld = LinearDev::new(name, &dm, segments).unwrap();
        assert_eq!(dm.table_status(&DevId::Name(name), DM_STATUS_TABLE)
                       .unwrap()
                       .1
                       .len(),
                   count);
        assert_eq!(blkdev_size(&OpenOptions::new()
                                    .read(true)
                                    .open(ld.devnode().unwrap())
                                    .unwrap())
                           .unwrap()
                           .sectors(),
                   range);

        ld.teardown(&dm).unwrap();
    }

    /// Verify that constructing a second dev with the same name succeeds.
    /// Verify that the segment table is, however, the segment table of the
    /// previously constructed device.
    fn test_same_name(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from_str(&paths[0].to_string_lossy()).unwrap();
        let segments = vec![Segment::new(dev, Sectors(0), Sectors(1))];
        let table = LinearDev::dm_table(&segments);
        let ld = LinearDev::new(name, &dm, vec![Segment::new(dev, Sectors(0), Sectors(1))])
            .unwrap();
        assert!(LinearDev::new(name, &dm, vec![Segment::new(dev, Sectors(1), Sectors(1))]).is_ok());
        assert_eq!(table,
                   dm.table_status(&DevId::Name(name), DM_STATUS_TABLE)
                       .unwrap()
                       .1);

        ld.teardown(&dm).unwrap();
    }

    /// Verify constructing a second linear dev with the same segment succeeds.
    fn test_same_segment(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let dev = Device::from_str(&paths[0].to_string_lossy()).unwrap();
        let ld = LinearDev::new("name", &dm, vec![Segment::new(dev, Sectors(0), Sectors(1))])
            .unwrap();
        let ld2 = LinearDev::new("ersatz",
                                 &dm,
                                 vec![Segment::new(dev, Sectors(0), Sectors(1))]);
        assert!(ld2.is_ok());

        ld2.unwrap().teardown(&dm).unwrap();
        ld.teardown(&dm).unwrap();
    }

    /// Verify that table status returns the expected table.
    fn test_table_status(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from_str(&paths[0].to_string_lossy()).unwrap();
        let segments = vec![Segment::new(dev, Sectors(0), Sectors(1))];
        let table = LinearDev::dm_table(&segments);
        let ld = LinearDev::new(name, &dm, segments).unwrap();
        assert_eq!(table,
                   dm.table_status(&DevId::Name(name), DM_STATUS_TABLE)
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
