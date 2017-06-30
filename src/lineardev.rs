// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::fs::File;
use std::path::PathBuf;

use consts::{DmFlags, DM_SUSPEND};
use deviceinfo::DeviceInfo;
use dm::{DM, DevId};
use result::{DmResult, DmError, ErrorEnum};
use segment::Segment;
use types::{Sectors, TargetLine};
use util::blkdev_size;

/// A DM construct of combined Segments
pub struct LinearDev {
    /// Data about the device
    dev_info: DeviceInfo,
    segments: Vec<Segment>,
}

impl fmt::Debug for LinearDev {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name())
    }
}

/// Use DM to concatenate a set of blockdevs together into a
/// /dev/mapper/xxx block device of continuous sectors.
impl LinearDev {
    /// Construct a new block device by concatenating the given block_devs
    /// into linear space.  Use DM to reserve enough space for the stratis
    /// metadata on each DmDev.
    pub fn new(name: &str, dm: &DM, block_devs: Vec<Segment>) -> DmResult<LinearDev> {
        if block_devs.is_empty() {
            return Err(DmError::Dm(ErrorEnum::Invalid,
                                   "linear device must have at least one segment".into()));
        }

        try!(dm.device_create(name, None, DmFlags::empty()));
        let table = LinearDev::dm_table(&block_devs);
        let id = &DevId::Name(name);
        let dev_info = try!(dm.table_load(id, &table));
        try!(dm.device_suspend(id, DmFlags::empty()));

        DM::wait_for_dm();
        Ok(LinearDev {
               dev_info: dev_info,
               segments: block_devs,
           })
    }

    /// Return a reference to the segments that back this linear device.
    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    /// Generate a Vec<> to be passed to DM.  The format of the Vec entries is:
    /// <logical start sec> <length> "linear" /dev/xxx <start offset>
    fn dm_table(block_devs: &[Segment]) -> Vec<TargetLine> {
        assert_ne!(block_devs.len(), 0);

        let mut table = Vec::new();
        let mut logical_start_sector = Sectors(0);
        for block_dev in block_devs {
            let (start, length) = (block_dev.start, block_dev.length);
            let line = (*logical_start_sector,
                        *length,
                        "linear".to_owned(),
                        format!("{} {}", block_dev.device.dstr(), *start));
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

        try!(dm.table_load(id, &table));
        try!(dm.device_suspend(id, DM_SUSPEND));
        try!(dm.device_suspend(id, DmFlags::empty()));

        Ok(())
    }

    /// DM name - from the DeviceInfo struct
    pub fn name(&self) -> &str {
        self.dev_info.name()
    }

    /// Set the name for this LinearDev.
    pub fn set_name(&mut self, dm: &DM, name: &str) -> DmResult<()> {
        self.dev_info = try!(dm.device_rename(self.dev_info.name(), name, DmFlags::empty()));

        Ok(())
    }

    /// Get the "x:y" device string for this LinearDev
    pub fn dstr(&self) -> String {
        self.dev_info.device().dstr()
    }

    /// return the total size of the linear device
    pub fn size(&self) -> DmResult<Sectors> {
        let blockdev_path = try!(self.devnode());
        let f = try!(File::open(blockdev_path));
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
