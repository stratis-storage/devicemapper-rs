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

#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]
#![cfg_attr(not(features = "clippy"), allow(unknown_lints))]

#![allow(doc_markdown)]

#![warn(missing_docs)]

extern crate serde;

#[cfg(test)]
#[macro_use]
extern crate nix;
#[cfg(test)]
extern crate loopdev;
#[cfg(test)]
extern crate tempdir;

extern crate devicemapper;

/// Macros shared by device mapper devices.
#[macro_use]
mod shared_macros;
/// functions to create continuous linear space given device segments
mod lineardev;
/// allocate a device from a pool
mod thindev;
/// the id the pool uses to track its devices
mod thindevid;
/// thinpooldev is shared space for  other thin provisioned devices to use
mod thinpooldev;
/// cachedev
mod cachedev;
/// struct to represent a location, offset and size of a set of disk sectors
mod segment;
/// return results container
mod result;
/// functionality shared between devices
mod shared;
/// Some useful types.
mod types;

#[cfg(test)]
mod loopbacked;


pub use cachedev::{CacheDev, CacheDevPerformance, CacheDevStatus, CacheDevUsage,
                   CacheDevWorkingStatus};
pub use lineardev::LinearDev;
pub use result::{DmResult, DmError, ErrorEnum};
pub use segment::Segment;
pub use shared::{DmDevice, device_exists};
pub use thinpooldev::{ThinPoolUsage, ThinPoolDev, ThinPoolNoSpacePolicy, ThinPoolStatus,
                      ThinPoolStatusSummary, ThinPoolWorkingStatus};
pub use thindev::{ThinDev, ThinDevWorkingStatus, ThinStatus};
pub use thindevid::ThinDevId;
pub use types::{TargetLine, TargetParams};
