// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::dm_ioctl as dmi;

bitflags! {
    /// Flags used by devicemapper.
    #[derive(Default)]
    pub struct DmFlags: dmi::__u32 {
        /// In: Device should be read-only.
        /// Out: Device is read-only.
        #[allow(identity_op)]
        const DM_READONLY             = (1 << 0);
        /// In: Device should be suspended.
        /// Out: Device is suspended.
        const DM_SUSPEND              = (1 << 1);
        /// In: Use passed-in minor number.
        const DM_PERSISTENT_DEV       = (1 << 3);
        /// In: STATUS command returns table info instead of status.
        const DM_STATUS_TABLE         = (1 << 4);
        /// Out: Active table is present.
        const DM_ACTIVE_PRESENT       = (1 << 5);
        /// Out: Inactive table is present.
        const DM_INACTIVE_PRESENT     = (1 << 6);
        /// Out: Passed-in buffer was too small.
        const DM_BUFFER_FULL          = (1 << 8);
        /// Obsolete.
        const DM_SKIP_BDGET           = (1 << 9);
        /// In: Avoid freezing filesystem when suspending.
        const DM_SKIP_LOCKFS          = (1 << 10);
        /// In: Suspend without flushing queued I/Os.
        const DM_NOFLUSH              = (1 << 11);
        /// In: Query inactive table instead of active.
        const DM_QUERY_INACTIVE_TABLE = (1 << 12);
        /// Out: A uevent was generated, the caller may need to wait for it.
        const DM_UEVENT_GENERATED     = (1 << 13);
        /// In: Rename affects UUID field, not name field.
        const DM_UUID                 = (1 << 14);
        /// In: All buffers are wiped after use. Use when handling crypto keys.
        const DM_SECURE_DATA          = (1 << 15);
        /// Out: A message generated output data.
        const DM_DATA_OUT             = (1 << 16);
        /// In: Do not remove in-use devices.
        /// Out: Device scheduled to be removed when closed.
        const DM_DEFERRED_REMOVE      = (1 << 17);
        /// Out: Device is suspended internally.
        const DM_INTERNAL_SUSPEND     = (1 << 18);
    }
}

bitflags! {
    /// Flags used by devicemapper, see:
    /// https://sourceware.org/git/?p=lvm2.git;a=blob;f=libdm/libdevmapper.h#l3627
    /// for complete information about the meaning of the flags.
    #[derive(Default)]
    pub struct DmCookie: dmi::__u16 {
        #[allow(identity_op)]
        /// Disables basic device-mapper udev rules that create symlinks in /dev/<DM_DIR>
        /// directory.
        const DM_UDEV_DISABLE_DM_RULES_FLAG = (1 << 0);
        /// Disable subsystem udev rules, but allow general DM udev rules to run.
        const DM_UDEV_DISABLE_SUBSYSTEM_RULES_FLAG = (1 << 1);
        /// Disable dm udev rules which create symlinks in /dev/disk/* directory.
        const DM_UDEV_DISABLE_DISK_RULES_FLAG = (1 << 2);
        /// Disable all rules that are not general dm nor subsystem related.
        const DM_UDEV_DISABLE_OTHER_RULES_FLAG = (1 << 3);
        /// Instruct udev rules to give lower priority to the device.
        const DM_UDEV_LOW_PRIORITY_FLAG = (1 << 4);
        /// Disable libdevmapper's node management.
        const DM_UDEV_DISABLE_LIBRARY_FALLBACK = (1 << 5);
        /// Automatically appended to all IOCTL calls issues by libdevmapper for generating
        /// udev uevents.
        const DM_UDEV_PRIMARY_SOURCE_FLAG = (1 << 6);
    }
}
