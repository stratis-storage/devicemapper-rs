// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use types::Sectors;

use Device;

/// struct to represent a continuous set of sectors on a disk
#[derive(Debug)]
pub struct Segment {
    start: Sectors,
    length: Sectors,
    device: Device,
}


impl Segment {
    /// Create a new Segment with given attributes
    pub fn new(device: Device, start: Sectors, length: Sectors) -> Segment {
        Segment {
            device: device,
            start: start,
            length: length,
        }
    }

    /// Return the start and length sectors
    pub fn range(&self) -> (Sectors, Sectors) {
        (self.start, self.length)
    }

    /// Get the "x:y" device string for this blockdev
    pub fn dstr(&self) -> String {
        self.device.dstr()
    }
}
