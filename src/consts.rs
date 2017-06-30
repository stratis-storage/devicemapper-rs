// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// disk sector size in bytes
pub const SECTOR_SIZE: usize = 512;

use dm_ioctl as dmi;

#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
/// International Electrotechnical Commission Units Standards
pub mod IEC {
    /// kibi
    pub const Ki: u64 = 1024;
    /// mebi
    pub const Mi: u64 = 1024 * Ki;
    /// gibi
    pub const Gi: u64 = 1024 * Mi;
    /// tebi
    pub const Ti: u64 = 1024 * Gi;
    /// pebi
    pub const Pi: u64 = 1024 * Ti;
    /// exbi
    pub const Ei: u64 = 1024 * Pi;
    // Ei is the maximum IEC unit expressible in u64.
}

/// Indicator to send IOCTL to DM
pub const DM_IOCTL: u8 = 0xfd;
/// Control path for user space to pass IOCTL to kernel DM
pub const DM_CTL_PATH: &'static str = "/dev/mapper/control";
/// Major version
pub const DM_VERSION_MAJOR: u32 = 4;
/// Minor version
pub const DM_VERSION_MINOR: u32 = 30;
/// Patch level
pub const DM_VERSION_PATCHLEVEL: u32 = 0;

/// Name max length
pub const DM_NAME_LEN: usize = 128;
/// UUID max length
pub const DM_UUID_LEN: usize = 129;
/// Start with a large buffer to make BUFFER_FULL rare. Libdm does this too.s
pub const MIN_BUF_SIZE: usize = 16 * 1024;


bitflags!(
    /// Flags used by devicemapper.
    flags DmFlags: dmi::__u32 {
        /// In: Device should be read-only.
        /// Out: Device is read-only.
        #[allow(identity_op)]
        const DM_READONLY             = (1 << 0),
        /// In: Device should be suspended.
        /// Out: Device is suspended.
        const DM_SUSPEND              = (1 << 1),
        /// In: Use passed-in minor number.
        const DM_PERSISTENT_DEV       = (1 << 3),
        /// In: STATUS command returns table info instead of status.
        const DM_STATUS_TABLE         = (1 << 4),
        /// Out: Active table is present.
        const DM_ACTIVE_PRESENT       = (1 << 5),
        /// Out: Inactive table is present.
        const DM_INACTIVE_PRESENT     = (1 << 6),
        /// Out: Passed-in buffer was too small.
        const DM_BUFFER_FULL          = (1 << 8),
        /// Obsolete.
        const DM_SKIP_BDGET           = (1 << 9),
        /// In: Avoid freezing filesystem when suspending.
        const DM_SKIP_LOCKFS          = (1 << 10),
        /// In: Suspend without flushing queued I/Os.
        const DM_NOFLUSH              = (1 << 11),
        /// In: Query inactive table instead of active.
        const DM_QUERY_INACTIVE_TABLE = (1 << 12),
        /// Out: A uevent was generated, the caller may need to wait for it.
        const DM_UEVENT_GENERATED     = (1 << 13),
        /// In: Rename affects UUID field, not name field.
        const DM_UUID                 = (1 << 14),
        /// In: All buffers are wiped after use. Use when handling crypto keys.
        const DM_SECURE_DATA          = (1 << 15),
        /// Out: A message generated output data.
        const DM_DATA_OUT             = (1 << 16),
        /// In: Do not remove in-use devices.
        /// Out: Device scheduled to be removed when closed.
        const DM_DEFERRED_REMOVE      = (1 << 17),
        /// Out: Device is suspended internally.
        const DM_INTERNAL_SUSPEND     = (1 << 18),
    }
);
