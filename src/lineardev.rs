// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::fs::File;
use std::path::PathBuf;

use consts::DmFlags;
use deviceinfo::DeviceInfo;
use dm::{DM, DevId};
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
    /// TODO: If the linear device already exists, verify that the kernel's
    /// model matches the segments argument.
    pub fn new(name: &str, dm: &DM, segments: Vec<Segment>) -> DmResult<LinearDev> {
        if segments.is_empty() {
            return Err(DmError::Dm(ErrorEnum::Invalid,
                                   "linear device must have at least one segment".into()));
        }

        let id = DevId::Name(name);
        let dev_info = if try!(device_exists(dm, name)) {
            // TODO: Verify that kernel's model matches up with segments.
            Box::new(try!(dm.device_status(&id)))
        } else {
            try!(dm.device_create(name, None, DmFlags::empty()));
            let table = LinearDev::dm_table(&segments);
            Box::new(try!(table_load(dm, &id, &table)))
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

    /// Generate a Vec<> to be passed to DM.  The format of the Vec entries is:
    /// <logical start sec> <length> "linear" <maj:min> <start offset>
    fn dm_table(segments: &[Segment]) -> Vec<TargetLine> {
        assert_ne!(segments.len(), 0);

        let mut table = Vec::new();
        let mut logical_start_sector = Sectors(0);
        for segment in segments {
            let (start, length) = (segment.start, segment.length);
            let line = (logical_start_sector,
                        length,
                        "linear".to_owned(),
                        format!("{} {}", segment.device.dstr(), *start));
            debug!("dmtable line : {:?}", line);
            table.push(line);
            logical_start_sector += length;
        }

        table
    }

    /// Extend an existing LinearDev with additional new segments.
    pub fn extend(&mut self, new_segs: Vec<Segment>) -> DmResult<()> {
        if new_segs.is_empty() {
            return Ok(());
        }

        // Last existing and first new may be contiguous. Coalesce into
        // a single Segment if so.
        let coalesced_new_first = {
            let mut old_last = self.segments
                .last_mut()
                .expect("Existing segment list must not be empty");
            let new_first = new_segs.first().expect("new_segs must not be empty");
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

        let dm = try!(DM::new());
        let id = &DevId::Name(self.name());

        try!(table_reload(&dm, id, &table));

        Ok(())
    }

    /// DM name - from the DeviceInfo struct
    pub fn name(&self) -> &str {
        self.dev_info.name()
    }

    /// Set the name for this LinearDev.
    pub fn set_name(&mut self, dm: &DM, name: &str) -> DmResult<()> {
        self.dev_info =
            Box::new(try!(dm.device_rename(self.dev_info.name(), name, DmFlags::empty())));

        Ok(())
    }

    /// Get the "x:y" device string for this LinearDev
    pub fn dstr(&self) -> String {
        self.dev_info.device().dstr()
    }

    /// return the total size of the linear device
    pub fn size(&self) -> DmResult<Sectors> {
        let f = try!(File::open(try!(self.devnode())));
        Ok(try!(blkdev_size(&f)).sectors())
    }

    /// path of the device node
    pub fn devnode(&self) -> DmResult<PathBuf> {
        self.dev_info
            .device()
            .devnode()
            .ok_or(DmError::Dm(ErrorEnum::NotFound,
                               "No path associated with dev_info".into()))
    }

    /// Remove the device from DM
    pub fn teardown(self, dm: &DM) -> DmResult<()> {
        try!(dm.device_remove(&DevId::Name(self.name()), DmFlags::empty()));
        Ok(())
    }
}
