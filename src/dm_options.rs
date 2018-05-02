// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::dm_flags::DmFlags;

/// Encapsulates options for device mapper calls
#[derive(Debug, Clone)]
pub struct DmOptions {
    flags: Option<DmFlags>,
    event_nr: Option<u32>,
}


impl DmOptions {
    /// Create a new empty option
    pub fn new() -> DmOptions {
        DmOptions {
            flags: None,
            event_nr: None,
        }
    }

    /// Set the DmFlags value for option
    pub fn set_flags(mut self, flags: DmFlags) -> DmOptions {
        self.flags = Some(flags);
        self
    }

    /// Set the event_nr value for option
    pub fn set_event_nr(mut self, event_nr: u32) -> DmOptions {
        self.event_nr = Some(event_nr);
        self
    }

    /// Retrieve the flags value
    pub fn get_flags(&self) -> DmFlags {
        if let Some(flags) = self.flags {
            flags
        } else {
            DmFlags::empty()
        }
    }

    /// Retrieve the event_nr value
    pub fn get_event_nr(&self) -> u32 {
        self.event_nr.unwrap_or(0)
    }
}
