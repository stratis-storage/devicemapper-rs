// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! Modules that support handling of devicemapper ioctls at a low-level.

mod device;
pub(self) mod deviceinfo;
mod dm;
mod dm_flags;
pub(self) mod dm_ioctl;
mod dm_options;
pub mod errors;
mod types;
pub(self) mod util;

pub use self::{
    device::{devnode_to_devno, Device},
    deviceinfo::DeviceInfo,
    dm::DM,
    dm_flags::{DmCookie, DmFlags},
    dm_options::DmOptions,
    types::{DevId, DmName, DmNameBuf, DmUuid, DmUuidBuf},
};
