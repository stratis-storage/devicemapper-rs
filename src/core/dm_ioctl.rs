// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;
use std::sync::LazyLock;

pub use devicemapper_sys::{
    dm_ioctl as Struct_dm_ioctl, dm_name_list as Struct_dm_name_list,
    dm_target_deps as Struct_dm_target_deps, dm_target_msg as Struct_dm_target_msg,
    dm_target_spec as Struct_dm_target_spec, dm_target_versions as Struct_dm_target_versions, *,
};

// Map device-mapper ioctl commands to the minimum ioctl interface version
// required. The mapping is based on the _cmd_data_v4 table defined in
// libdm/ioctl/libdm-iface.c in the lvm2/libdevmapper sources.
static IOCTL_VERSIONS: LazyLock<HashMap<u32, (u32, u32, u32)>> = LazyLock::new(|| {
    HashMap::from([
        (DM_VERSION_CMD, (4, 0, 0)),
        (DM_REMOVE_ALL_CMD, (4, 0, 0)),
        (DM_LIST_DEVICES_CMD, (4, 0, 0)),
        (DM_DEV_CREATE_CMD, (4, 0, 0)),
        (DM_DEV_REMOVE_CMD, (4, 0, 0)),
        (DM_DEV_RENAME_CMD, (4, 0, 0)),
        (DM_DEV_SUSPEND_CMD, (4, 0, 0)),
        (DM_DEV_STATUS_CMD, (4, 0, 0)),
        (DM_DEV_WAIT_CMD, (4, 0, 0)),
        (DM_TABLE_LOAD_CMD, (4, 0, 0)),
        (DM_TABLE_CLEAR_CMD, (4, 0, 0)),
        (DM_TABLE_DEPS_CMD, (4, 0, 0)),
        (DM_TABLE_STATUS_CMD, (4, 0, 0)),
        #[cfg(devicemapper41supported)]
        (DM_LIST_VERSIONS_CMD, (4, 1, 0)),
        #[cfg(devicemapper42supported)]
        (DM_TARGET_MSG_CMD, (4, 2, 0)),
        #[cfg(devicemapper46supported)]
        (DM_DEV_SET_GEOMETRY_CMD, (4, 6, 0)),
        // libdevmapper sets DM_DEV_ARM_POLL to (4, 36, 0) however the command was
        // added after 4.36.0: depend on 4.37 to reliably access ARM_POLL.
        #[cfg(devicemapper437supported)]
        (DM_DEV_ARM_POLL_CMD, (4, 37, 0)),
        #[cfg(devicemapper441supported)]
        (DM_GET_TARGET_VERSION_CMD, (4, 41, 0)),
    ])
});

// Map device-mapper ioctl commands to (major, minor, patchlevel)
// tuple specifying the required kernel ioctl interface version.
pub(crate) fn ioctl_to_version(ioctl: u8) -> (u32, u32, u32) {
    let ioctl = &(ioctl as u32);
    if IOCTL_VERSIONS.contains_key(ioctl) {
        IOCTL_VERSIONS[ioctl]
    } else {
        unreachable!("Unknown device-mapper ioctl command: {}", ioctl);
    }
}

/// Returns true for ioctls that use `event_nr` as a udev cookie and
/// participate in udev synchronization via SysV semaphore.
#[inline]
pub(crate) fn ioctl_uses_udev_cookie(ioctl: u8) -> bool {
    matches!(
        ioctl as u32,
        DM_DEV_REMOVE_CMD | DM_DEV_RENAME_CMD | DM_DEV_SUSPEND_CMD
    )
}

/// Returns true for ioctls whose `event_nr` header field carries meaningful
/// input that must not be cleared before the ioctl is issued.
///
/// Per the kernel header (`dm_ioctl.h`):
/// - `DM_SUSPEND`, `DM_DEV_REMOVE`, and `DM_DEV_RENAME` use `event_nr` as a
///   udev cookie for synchronization (see [`ioctl_uses_udev_cookie`]).
/// - `DM_DEV_WAIT` uses `event_nr` as an event number input but does not
///   participate in udev synchronization.
///   For output, all ioctls return the event number, not the cookie.
#[inline]
pub(crate) fn ioctl_uses_event_number(ioctl: u8) -> bool {
    ioctl_uses_udev_cookie(ioctl) || ioctl as u32 == DM_DEV_WAIT_CMD
}
