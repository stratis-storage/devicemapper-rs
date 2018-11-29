// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! Low-level devicemapper configuration of the running kernel.
//!
//! # Overview
//!
//! Linux's devicemapper allows the creation of block devices whose
//! storage is mapped to other block devices in useful ways, either by
//! changing the location of its data blocks, or performing some
//! operation on the data itself. This is a low-level facility that is
//! used by higher-level volume managers such as LVM2. Uses may
//! include:
//!
//! * Dividing a large block device into smaller logical volumes (dm-linear)
//! * Combining several separate block devices into a single block
//!   device with better performance and/or redundancy (dm-raid)
//! * Encrypting a block device (dm-crypt)
//! * Performing Copy-on-Write (COW) allocation of a volume's blocks
//!   enabling fast volume cloning and snapshots (dm-thin)
//! * Configuring a smaller, faster block device to act as a cache for a
//!   larger, slower one (dm-cache)
//! * Verifying the contents of a read-only volume (dm-verity)
//!
//! # Usage
//!
//! Before they can be used, DM devices must be created using
//! `DM::device_create()`, have a mapping table loaded using
//! `DM::table_load()`, and then activated with
//! `DM::device_suspend()`. (This function is used for both suspending
//! and activating a device.) Once activated, they can be used as a
//! regular block device, including having other DM devices map to
//! them.
//!
//! Devices have "active" and "inactive" mapping tables. See function
//! descriptions for which table they affect.
//!
//! # Polling for Events
//!
//! Since DM minor version 37, first available in Linux kernel 4.14, the file
//! descriptor associated with a `DM` context may be polled for events generated by
//! DM devices.
//!
//! The fd will indicate POLLIN if any events have occurred on any DM devices
//! since the fd was opened, or since `DM::arm_poll()` was called. Therefore,
//! in order to determine which DM devices have generated an event, the
//! following usage is required:
//!
//! 1. Create a `DM`.
//! 2. Call `DM::list_devices()` and track the `event_nr`s for any DM devices
//! of interest.
//! 3. `poll()` on the `DM`'s file descriptor, obtained by calling
//! `DM::file().as_raw_fd()`.
//! 4. If the fd indicates activity, first clear the event by calling
//! `DM::arm_poll()`.  This must be done before event processing to ensure
//! events are not missed.
//! 5. Process events. Call `DM::list_devices()` again, and compare `event_nr`
//! returned by the more recent call with `event_nr` values from the earlier
//! call.  If `event_nr` differs, an event has occurred on that specific
//! device. Handle the event(s). Update the list of last-seen `event_nr`s.
//! 6. Optionally loop and re-invoke `poll()` on the fd to wait for more
//! events.

#![cfg_attr(not(features = "clippy"), allow(unknown_lints))]
#![allow(doc_markdown)]
#![warn(missing_docs)]

#[macro_use]
extern crate macro_attr;
#[macro_use]
extern crate newtype_derive;

#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate error_chain;
extern crate libc;
#[macro_use]
extern crate nix;
extern crate serde;

#[cfg(test)]
extern crate libmount;
#[cfg(test)]
extern crate libudev;
#[cfg(test)]
extern crate loopdev;
#[cfg(test)]
#[macro_use]
extern crate matches;
#[cfg(test)]
extern crate tempfile;
#[cfg(test)]
extern crate uuid;

/// shared constants
mod consts;
/// rust definitions of ioctl structs and consts
mod dm_ioctl;
/// basic types (Bytes, Sectors, DataBlocks)
mod types;
/// public utilities
mod util;
/// Macros shared by device mapper devices.
#[macro_use]
mod shared_macros;
/// cachedev
mod cachedev;
/// contains device major/minor and associated functions
mod device;
/// wrapper for C interface for DM
mod deviceinfo;
/// core lower level API
mod dm;
/// DM flags
mod dm_flags;
/// Options for dm function calls
mod dm_options;
/// error chain errors for core dm
mod errors;
/// functions to create continuous linear space given device segments
mod lineardev;
/// return results container
mod result;
/// functionality shared between devices
mod shared;
/// allocate a device from a pool
mod thindev;
/// the id the pool uses to track its devices
mod thindevid;
/// thinpooldev is shared space for  other thin provisioned devices to use
mod thinpooldev;

#[cfg(test)]
mod loopbacked;

#[cfg(test)]
mod test_lib;

pub use cachedev::{
    CacheDev, CacheDevPerformance, CacheDevStatus, CacheDevUsage, CacheDevWorkingStatus,
    MAX_CACHE_BLOCK_SIZE, MIN_CACHE_BLOCK_SIZE,
};
pub use consts::{IEC, SECTOR_SIZE};
pub use device::{devnode_to_devno, Device};
pub use dm::DM;
pub use dm_flags::{DmCookie, DmFlags};
pub use dm_options::DmOptions;
pub use lineardev::{
    FlakeyTargetParams, LinearDev, LinearDevTargetParams, LinearDevTargetTable, LinearTargetParams,
};
pub use result::{DmError, DmResult, ErrorEnum};
pub use shared::{device_exists, DmDevice, TargetLine};
pub use thindev::{ThinDev, ThinDevWorkingStatus, ThinStatus};
pub use thindevid::ThinDevId;
pub use thinpooldev::{
    ThinPoolDev, ThinPoolNoSpacePolicy, ThinPoolStatus, ThinPoolStatusSummary, ThinPoolUsage,
    ThinPoolWorkingStatus,
};
pub use types::{
    Bytes, DataBlocks, DevId, DmName, DmNameBuf, DmUuid, DmUuidBuf, MetaBlocks, Sectors,
    TargetType, TargetTypeBuf,
};
