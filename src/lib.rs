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
#![cfg_attr(not(feature = "clippy"), allow(unknown_lints))]

#![warn(missing_docs)]

#![allow(used_underscore_binding)]
#![allow(if_not_else)]

#[macro_use]
extern crate custom_derive;
#[macro_use]
extern crate newtype_derive;

extern crate libc;
#[macro_use]
extern crate nix;
extern crate serde;
#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate log;

#[allow(dead_code, non_camel_case_types)]
mod dm_ioctl;
/// public utilities
mod util;
/// basic types (Bytes, Sectors, DataBlocks)
mod types;
/// shared constants
pub mod consts;
/// functions to create continuous linear space given device segments
mod lineardev;
/// allocate a device from a pool
mod thindev;
/// thinpooldev is shared space for  other thin provisioned devices to use
mod thinpooldev;
/// struct to represent a location, offset and size of a set of disk sectors
mod segment;
/// return results container
mod result;
/// wrapper for C interface for DM
mod deviceinfo;
/// contains device major/minor and associated functions
mod device;
/// core lower level API
mod dm;

pub use dm::{DM, DevId};
pub use device::Device;
pub use lineardev::LinearDev;
pub use result::{DmResult, DmError, ErrorEnum};
pub use segment::Segment;
pub use thinpooldev::{ThinPoolBlockUsage, ThinPoolDev, ThinPoolStatus, ThinPoolWorkingStatus};
pub use thindev::{ThinDev, ThinDevId, ThinStatus};
pub use types::{Bytes, DataBlocks, Sectors};
