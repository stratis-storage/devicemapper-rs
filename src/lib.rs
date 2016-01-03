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

#![feature(clone_from_slice, convert, custom_derive, plugin)]
#![plugin(serde_macros)]
#![warn(missing_docs)]

extern crate libc;
extern crate nix;
extern crate serde;

#[macro_use]
extern crate bitflags;

#[allow(dead_code, non_camel_case_types)]
mod dm_ioctl;
mod util;

use std::fs::File;
use std::io;
use std::io::{Error, BufReader, BufRead};
use std::path::{Path, PathBuf};
use std::str::{FromStr, from_utf8};
use std::io::ErrorKind::{Other, InvalidInput};
use std::os::unix::io::AsRawFd;
use std::mem::{size_of, transmute};
use std::slice;
use std::collections::BTreeSet;
use std::os::unix::fs::MetadataExt;
use std::cmp;
use std::borrow::Borrow;

use nix::sys::ioctl;

use dm_ioctl as dmi;
use util::align_to;

const DM_IOCTL: u8 = 0xfd;
const DM_CTL_PATH: &'static str= "/dev/mapper/control";

const DM_VERSION_MAJOR: u32 = 4;
const DM_VERSION_MINOR: u32 = 30;
const DM_VERSION_PATCHLEVEL: u32 = 0;

const DM_NAME_LEN: usize = 128;
const DM_UUID_LEN: usize = 129;

const MIN_BUF_SIZE: usize = 16 * 1024;

bitflags!(
    /// Flags used by devicemapper.
    flags DmFlags: dmi::__u32 {
        /// In: Device should be read-only.
        /// Out: Device is read-only.
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


/// A struct containing the device's major and minor numbers
///
/// Also allows conversion to/from a single 64bit value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
pub struct Device {
    /// Device major number
    pub major: u32,
    /// Device minor number
    pub minor: u8,
}

impl Device {
    /// Returns the path in `/dev` that corresponds with the device number.
    pub fn path(&self) -> Option<PathBuf> {
        let f = File::open("/proc/partitions")
            .ok().expect("Could not open /proc/partitions");

        let reader = BufReader::new(f);

        for line in reader.lines().skip(2) {
            if let Ok(line) = line {
                let spl: Vec<_> = line.split_whitespace().collect();

                if spl[0].parse::<u32>().unwrap() == self.major
                    && spl[1].parse::<u8>().unwrap() == self.minor {
                        return Some(PathBuf::from(format!("/dev/{}", spl[3])));
                    }
            }
        }
        None
    }
}

impl FromStr for Device {
    type Err = Error;
    fn from_str(s: &str) -> io::Result<Device> {
        match s.parse::<i64>() {
            Ok(x) => Ok(Device::from(x as u64)),
            Err(_) => {
                match Path::new(s).metadata() {
                    Ok(x) => {
                        match x.mode() & 0x6000 == 0x6000 { // S_IFBLK
                            true => Ok(Device::from(x.rdev())),
                            false => Err(Error::new(
                                InvalidInput, format!("{} not block device", s))),
                        }
                    },
                    Err(x) => Err(x)
                }
            }
        }
    }
}

impl From<u64> for Device {
    fn from(val: u64) -> Device {
        Device { major: (val >> 8) as u32, minor: (val & 0xff) as u8 }
    }
}

impl From<Device> for u64 {
    fn from(dev: Device) -> u64 {
        ((dev.major << 8) ^ (dev.minor as u32 & 0xff)) as u64
    }
}


/// Major numbers used by DM.
pub fn dev_majors() -> BTreeSet<u32> {
    let mut set = BTreeSet::new();

    let f = File::open("/proc/devices")
        .ok().expect("Could not open /proc/devices");

    let reader = BufReader::new(f);

    for line in reader.lines()
        .filter_map(|x| x.ok())
        .skip_while(|x| x != "Block devices:")
        .skip(1) {
            let spl: Vec<_> = line.split_whitespace().collect();

            if spl[1] == "device-mapper" {
                set.insert(spl[0].parse::<u32>().unwrap());
            }
        }

    set
}

/// Contains information about the device.
#[derive(Clone, Copy)]
pub struct DeviceInfo {
    hdr: dmi::Struct_dm_ioctl,
}

impl DeviceInfo {
    /// The major, minor, and patchlevel versions of devicemapper.
    pub fn version(&self) -> (u32, u32, u32) {
        (self.hdr.version[0], self.hdr.version[1], self.hdr.version[2])
    }

    /// The number of times the device is currently open.
    pub fn open_count(&self) -> i32 {
        self.hdr.open_count
    }

    /// The last event number for the device.
    pub fn event_nr(&self) -> u32 {
        self.hdr.event_nr
    }

    /// The device's major and minor device numbers, as a Device.
    pub fn device(&self) -> Device {
        self.hdr.dev.into()
    }

    /// The device's name.
    pub fn name(&self) -> &str {
        let name: &[u8; DM_NAME_LEN] = unsafe { transmute(&self.hdr.name) };
        // no chance not null-terminated
        let slc = slice_to_null(name).unwrap();
        // no chance this isn't utf8 (ascii, really)
        from_utf8(slc).unwrap()
    }

    /// The device's UUID.
    pub fn uuid(&self) -> &str {
        let uuid: &[u8; DM_UUID_LEN] = unsafe { transmute(&self.hdr.uuid) };
        // no chance not null-terminated
        let slc = slice_to_null(uuid).unwrap();
        // no chance this isn't utf8 (ascii, really)
        from_utf8(slc).unwrap()
    }

    /// The flags returned from the device.
    pub fn flags(&self) -> DmFlags {
        DmFlags::from_bits_truncate(self.hdr.flags)
    }
}

/// Used as a parameter for functions that take either a Device name
/// or a Device UUID.
pub enum DevId<'a> {
    /// The parameter is the device's name
    Name(&'a str),
    /// The parameter is the device's UUID
    Uuid(&'a str),
}

/// Context needed for communicating with devicemapper.
pub struct DM {
    file: File,
}

impl DM {
    /// Create a new context for communicating with DM.
    pub fn new() -> io::Result<DM> {
        Ok(DM {
            file: try!(File::open(DM_CTL_PATH)),
        })
    }

    fn initialize_hdr(hdr: &mut dmi::Struct_dm_ioctl, flags: DmFlags) -> () {
        hdr.version[0] = DM_VERSION_MAJOR;
        hdr.version[1] = DM_VERSION_MINOR;
        hdr.version[2] = DM_VERSION_PATCHLEVEL;

        hdr.flags = flags.bits;

        hdr.data_start = size_of::<dmi::Struct_dm_ioctl>() as u32;
    }

    fn hdr_set_name(hdr: &mut dmi::Struct_dm_ioctl, name: &str) -> () {
        let name_dest: &mut [u8; DM_NAME_LEN] = unsafe { transmute(&mut hdr.name) };
        name_dest.clone_from_slice(name.as_bytes());
    }

    fn hdr_set_uuid(hdr: &mut dmi::Struct_dm_ioctl, uuid: &str) -> () {
        let uuid_dest: &mut [u8; DM_UUID_LEN] = unsafe { transmute(&mut hdr.uuid) };
        uuid_dest.clone_from_slice(uuid.as_bytes());
    }

    //
    // Give this a filled-in header and optionally add'l stuff.
    // Does the ioctl and maybe returns stuff. Handles BUFFER_FULL flag.
    //
    fn do_ioctl(&self, ioctl: u8, hdr: &mut dmi::Struct_dm_ioctl, in_data: Option<&[u8]>)
                -> io::Result<Vec<u8>> {

        let op = ioctl::op_read_write(DM_IOCTL, ioctl, size_of::<dmi::Struct_dm_ioctl>());

        // Create in-buf by copying hdr and any in-data into a linear Vec v.
        // 'slc' also aliases hdr as a &[u8], used later to copy the possibly-
        // modified hdr back.

        hdr.data_size = cmp::max(
            MIN_BUF_SIZE,
            size_of::<dmi::Struct_dm_ioctl>() + in_data.map_or(0, |x| x.len())) as u32;
        let mut v: Vec<u8> = Vec::with_capacity(hdr.data_size as usize);

        let hdr_slc = unsafe {
            let len = hdr.data_start as usize;
            let ptr: *mut u8 = transmute(hdr);
            slice::from_raw_parts_mut(ptr, len)
        };
        v.extend(&hdr_slc[..]);
        if let Some(in_data) = in_data {
            v.extend(in_data.iter().cloned());
        }
        let cap = v.capacity();
        v.resize(cap, 0);

        loop {
            match unsafe {
                ioctl::read_into_ptr(self.file.as_raw_fd(), op, v.as_mut_ptr())
            } {
                Err(_) => return Err((Error::last_os_error())),
                _ => {},
            };

            let hdr: &mut dmi::Struct_dm_ioctl = unsafe {
                transmute(v.as_ptr())
            };

            if (hdr.flags & DM_BUFFER_FULL.bits) == 0 {
                break
            }

            let len = v.len();
            v.resize(len * 2, 0);
            hdr.data_size = v.len() as u32;
        }

        let hdr: &mut dmi::Struct_dm_ioctl = unsafe {
            transmute(v.as_ptr())
        };

        // hdr possibly modified so copy back
        hdr_slc.clone_from_slice(&v[..hdr.data_start as usize]);

        // Maybe we got some add'l data back?
        Ok(v[hdr.data_start as usize..hdr.data_size as usize].to_vec())
    }

    /// Devicemapper version information: Major, Minor, and patchlevel versions.
    pub fn version(&self) -> io::Result<(u32, u32, u32)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());

        try!(self.do_ioctl(dmi::DM_VERSION_CMD as u8, &mut hdr, None));

        Ok((hdr.version[0], hdr.version[1], hdr.version[2]))
    }

    /// Remove all DM devices and tables. Use discouraged other than
    /// for debugging.
    ///
    /// If DM_DEFERRED_REMOVE is set, the request will succeed for
    /// in-use devices, and they will be removed when released.
    ///
    /// Valid flags: DM_DEFERRED_REMOVE
    pub fn remove_all(&self, flags: DmFlags) -> io::Result<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_DEFERRED_REMOVE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);

        try!(self.do_ioctl(dmi::DM_REMOVE_ALL_CMD as u8, &mut hdr, None));

        Ok(())
    }

    /// Returns a list of tuples containing DM device names and a
    /// Device, which holds their major and minor device numbers.
    pub fn list_devices(&self) -> io::Result<Vec<(String, Device)>> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());

        let data_out = try!(self.do_ioctl(dmi::DM_LIST_DEVICES_CMD as u8,
                                          &mut hdr, None));

        let mut devs = Vec::new();
        if data_out.len() > 0 {
            let mut result = data_out.as_slice();

            loop {
                let device: &dmi::Struct_dm_name_list = unsafe {
                    transmute(result.as_ptr())
                };

                let slc = slice_to_null(
                    &result[size_of::<dmi::Struct_dm_name_list>()..])
                    .expect("Bad data from ioctl");
                let dm_name = String::from_utf8_lossy(slc).into_owned();
                devs.push((dm_name, device.dev.into()));

                if device.next == 0 { break }

                result = &result[device.next as usize..];
            }
        }

        Ok(devs)
    }

    /// Create a DM device. It starts out in a "suspended" state.
    ///
    /// Valid flags: DM_READONLY, DM_PERSISTENT_DEV
    ///
    /// # Example
    ///
    /// ```no_run
    /// use devicemapper::{DM, DmFlags};
    /// let dm = DM::new().unwrap();
    ///
    /// // Setting a uuid is optional
    /// let dev = dm.device_create("example-dev", None, DmFlags::empty()).unwrap();
    /// ```
    pub fn device_create(&self, name: &str, uuid: Option<&str>, flags: DmFlags)
                         -> io::Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = (DM_READONLY | DM_PERSISTENT_DEV) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);

        Self::hdr_set_name(&mut hdr, name);
        if let Some(uuid) = uuid {
            Self::hdr_set_uuid(&mut hdr, uuid);
        }

        try!(self.do_ioctl(dmi::DM_DEV_CREATE_CMD as u8, &mut hdr, None));

        Ok(DeviceInfo {hdr: hdr})
    }

    /// Remove a DM device and its mapping tables.
    ///
    /// If DM_DEFERRED_REMOVE is set, the request for an in-use
    /// devices will succeed, and it will be removed when no longer
    /// used.
    ///
    /// Valid flags: DM_DEFERRED_REMOVE
    pub fn device_remove(&self, name: &DevId, flags: DmFlags) -> io::Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_DEFERRED_REMOVE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        try!(self.do_ioctl(dmi::DM_DEV_REMOVE_CMD as u8, &mut hdr, None));

        Ok(DeviceInfo {hdr: hdr})
    }

    /// Change a DM device's name.
    ///
    /// If DM_UUID is set, change the UUID instead.
    ///
    /// Valid flags: DM_UUID
    pub fn device_rename(&self, old_name: &str, new_name: &str, flags: DmFlags)
                         -> io::Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_UUID & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);

        let max_len = match clean_flags.contains(DM_UUID) {
            true => {
                Self::hdr_set_uuid(&mut hdr, old_name);
                DM_UUID_LEN - 1
            },
            false => {
                Self::hdr_set_name(&mut hdr, old_name);
                DM_NAME_LEN - 1
            },
        };

        if new_name.as_bytes().len() > max_len {
            return Err(
                Error::new(Other, format!("New name {} too long", new_name)));
        }

        let mut data_in = new_name.as_bytes().to_vec();
        data_in.push(b'\0');

        try!(self.do_ioctl(dmi::DM_DEV_RENAME_CMD as u8, &mut hdr, Some(&data_in)));

        Ok(DeviceInfo {hdr: hdr})
    }

    /// Suspend or resume a DM device, depending on if DM_SUSPEND flag
    /// is set or not.
    ///
    /// Resuming a DM device moves a table loaded into the "inactive"
    /// slot by `table_load()` into the "active" slot.
    ///
    /// Will block until pending I/O is completed unless DM_NOFLUSH
    /// flag is given. Will freeze filesystem unless DM_SKIP_LOCKFS
    /// flags is given. Additional I/O to a suspended device will be
    /// held until it is resumed.
    ///
    /// Valid flags: DM_SUSPEND, DM_NOFLUSH, DM_SKIP_LOCKFS
    ///
    /// # Example
    ///
    /// ```no_run
    /// use devicemapper::{DM, DmFlags, DM_SUSPEND, DevId};
    /// let dm = DM::new().unwrap();
    ///
    /// dm.device_suspend(&DevId::Name("example-dev"), DM_SUSPEND).unwrap();
    /// ```
    pub fn device_suspend(&self, name: &DevId, flags: DmFlags) -> io::Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = (DM_SUSPEND | DM_NOFLUSH | DM_SKIP_LOCKFS) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        try!(self.do_ioctl(dmi::DM_DEV_SUSPEND_CMD as u8, &mut hdr, None));

        Ok(DeviceInfo {hdr: hdr})
    }

    /// Get DeviceInfo for a device. This is also returned by other
    /// methods, but if just the DeviceInfo is desired then this just
    /// gets it.
    pub fn device_status(&self, name: &DevId) -> io::Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        try!(self.do_ioctl(dmi::DM_DEV_STATUS_CMD as u8, &mut hdr, None));

        Ok(DeviceInfo {hdr: hdr})
    }

    /// Wait for a device to report an event.
    ///
    /// Once an event occurs, this function behaves just like
    /// `table_status`, see that function for more details.
    pub fn device_wait(&self, name: &DevId, flags: DmFlags)
                        -> io::Result<(DeviceInfo, Vec<(u64, u64, String, String)>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_QUERY_INACTIVE_TABLE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        let data_out = try!(self.do_ioctl(dmi::DM_DEV_WAIT_CMD as u8, &mut hdr, None));

        let status = try!(Self::parse_table_status(hdr.target_count, &data_out));

        Ok((DeviceInfo {hdr: hdr}, status))

    }

    /// Load targets for a device into its inactive table slot.
    ///
    /// `targets` is an array of (sector_start, sector_length, type, params).
    ///
    /// `params` are target-specific, please see [Linux kernel documentation](https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/tree/Documentation/device-mapper) for more.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use devicemapper::{DM, DmFlags, DevId};
    /// let dm = DM::new().unwrap();
    ///
    /// // Create a 16MiB device (32768 512-byte sectors) that maps to /dev/sdb1
    /// // starting 1MiB into sdb1
    /// let table = vec![(0, 32768, "linear", "/dev/sdb1 2048")];
    ///
    /// dm.table_load(&DevId::Name("example-dev"), &table).unwrap();
    /// ```
    pub fn table_load<T1, T2>(&self, name: &DevId, targets: &[(u64, u64, T1, T2)])
                      -> io::Result<DeviceInfo>
        where T1: Borrow<str>,
              T2: Borrow<str>,
    {
        let mut targs = Vec::new();

        // Construct targets first, since we need to know how many & size
        // before initializing the header.
        for t in targets {
            let mut targ: dmi::Struct_dm_target_spec = Default::default();
            targ.sector_start = t.0;
            targ.length = t.1;
            targ.status = 0;

            let mut dst: &mut [u8] = unsafe {
                transmute(&mut targ.target_type[..])
            };
            dst.clone_from_slice(t.2.borrow().as_bytes());

            let mut params = t.3.borrow().to_string();

            let pad_bytes = align_to(
                params.len() + 1usize, 8usize) - params.len();
            params.extend(vec!["\0"; pad_bytes]);

            targ.next = (size_of::<dmi::Struct_dm_target_spec>()
                         + params.len()) as u32;

            targs.push((targ, params));
        }

        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        // io_ioctl() will set hdr.data_size but we must set target_count
        hdr.target_count = targs.len() as u32;

        // Flatten targets into a buf
        let mut data_in = Vec::new();

        for (targ, param) in targs {
            unsafe {
                let ptr: *mut u8 = transmute(&targ);
                let slc = slice::from_raw_parts(
                    ptr, size_of::<dmi::Struct_dm_target_spec>());
                data_in.extend(slc);
            }

            data_in.extend(param.as_bytes());
        }

        try!(self.do_ioctl(dmi::DM_TABLE_LOAD_CMD as u8, &mut hdr, Some(&data_in)));

        Ok(DeviceInfo {hdr: hdr})
    }

    /// Clear the "inactive" table for a device.
    pub fn table_clear(&self, name: &DevId) -> io::Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        try!(self.do_ioctl(dmi::DM_TABLE_CLEAR_CMD as u8, &mut hdr, None));

        Ok(DeviceInfo {hdr: hdr})
    }

    /// Query DM for which devices are referenced by the "active"
    /// table for this device.
    ///
    /// If DM_QUERY_INACTIVE_TABLE is set, instead return for the
    /// inactive table.
    ///
    /// Valid flags: DM_QUERY_INACTIVE_TABLE
    pub fn table_deps(&self, dev: Device, flags: DmFlags) -> io::Result<Vec<Device>> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_QUERY_INACTIVE_TABLE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        hdr.dev = dev.into();

        let data_out = try!(self.do_ioctl(dmi::DM_TABLE_DEPS_CMD as u8,
                                          &mut hdr, None));

        let mut devs = Vec::new();
        if data_out.len() > 0 {
            let result = data_out.as_slice();
            let deps: &dmi::Struct_dm_target_deps = unsafe {
                transmute(result.as_ptr())
            };

            let dev_slc = unsafe {
                slice::from_raw_parts(
                    result[size_of::<dmi::Struct_dm_target_deps>()..]
                        .as_ptr() as *const u64,
                    deps.count as usize)
            };

            for dev in dev_slc {
                devs.push(Device::from(*dev));
            }
        }

        Ok(devs)
    }

    // Both table_status and dev_wait return table status, so
    // unify table status parsing.
    fn parse_table_status(count: u32, buf: &[u8])
                          -> io::Result<Vec<(u64, u64, String, String)>> {
        let mut targets = Vec::new();
        if buf.len() > 0 {
            let mut next_off = 0;
            let mut result = &buf[..];

            for _ in 0..count {
                result = &result[next_off..];
                let targ: &dmi::Struct_dm_target_spec = unsafe {
                    transmute(result.as_ptr())
                };

                let target_type = unsafe {
                    let cast: &[u8; 16] = transmute(&targ.target_type);
                    let slc = slice_to_null(cast).expect("bad data from ioctl");
                    String::from_utf8_lossy(slc).into_owned()
                };

                let params = {
                    let slc = slice_to_null(
                        &buf[size_of::<dmi::Struct_dm_target_spec>()..])
                        .expect("bad data from ioctl");
                    String::from_utf8_lossy(slc).into_owned()
                };

                targets.push((targ.sector_start, targ.length, target_type, params));

                next_off = targ.next as usize;
            }
        }
        Ok(targets)
    }

    /// Return the status of all targets for a device's "active"
    /// table.
    ///
    /// Returns DeviceInfo and a Vec of (sector_start, sector_length, type, params).
    ///
    /// If DM_STATUS_TABLE flag is set, returns the current table value. Otherwise
    /// returns target-specific status information.
    ///
    /// If DM_NOFLUSH is set, retrieving the target-specific status information for
    /// targets with metadata will not cause a metadata write.
    ///
    /// If DM_QUERY_INACTIVE_TABLE is set, instead return the status of the
    /// inactive table.
    ///
    /// Valid flags: DM_NOFLUSH, DM_STATUS_TABLE, DM_QUERY_INACTIVE_TABLE
    ///
    /// # Example
    ///
    /// ```no_run
    /// use devicemapper::{DM, DmFlags, DM_STATUS_TABLE, DevId};
    /// let dm = DM::new().unwrap();
    ///
    /// let res = dm.table_status(&DevId::Name("example-dev"), DM_STATUS_TABLE).unwrap();
    /// println!("{} {:?}", res.0.name(), res.1);
    /// ```
    pub fn table_status(&self, name: &DevId, flags: DmFlags)
                        -> io::Result<(DeviceInfo, Vec<(u64, u64, String, String)>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags =
            (DM_NOFLUSH | DM_STATUS_TABLE | DM_QUERY_INACTIVE_TABLE) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        let data_out = try!(self.do_ioctl(dmi::DM_TABLE_STATUS_CMD as u8,
                                          &mut hdr, None));

        let status = try!(Self::parse_table_status(hdr.target_count, &data_out));

        Ok((DeviceInfo {hdr: hdr}, status))
    }

    /// Returns a list of each loaded target type with its name, and
    /// version broken into major, minor, and patchlevel.
    pub fn list_versions(&self) -> io::Result<Vec<(String, u32, u32, u32)>> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());

        let data_out = try!(self.do_ioctl(dmi::DM_LIST_VERSIONS_CMD as u8,
                                          &mut hdr, None));

        let mut targets = Vec::new();
        if data_out.len() > 0 {
            let mut result = data_out.as_slice();

            loop {
                let tver: &dmi::Struct_dm_target_versions = unsafe {
                    transmute(result.as_ptr())
                };

                let name_slc = slice_to_null(
                    &result[size_of::<dmi::Struct_dm_target_versions>()..])
                    .expect("bad data from ioctl");
                let name = String::from_utf8_lossy(name_slc).into_owned();
                targets.push((name, tver.version[0], tver.version[1], tver.version[2]));

                if tver.next == 0 { break }

                result = &result[tver.next as usize..];
            }
        }

        Ok(targets)
    }

    /// Send a message to the target at a given sector. If sector is
    /// not needed use 0.  DM-wide messages start with '@', and may
    /// return a string; targets do not.
    pub fn target_msg(&self, name: &DevId, sector: u64, msg: &str)
                      -> io::Result<(DeviceInfo, Option<String>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match name {
            &DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            &DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        let mut msg_struct: dmi::Struct_dm_target_msg = Default::default();
        msg_struct.sector = sector;
        let mut data_in = unsafe {
            let ptr: *mut u8 = transmute(&msg_struct);
            let slc = slice::from_raw_parts(ptr, size_of::<dmi::Struct_dm_target_msg>());
            slc.to_vec()
        };

        data_in.extend(msg.as_bytes());
        data_in.push(b'\0');

        let data_out = try!(self.do_ioctl(dmi::DM_TARGET_MSG_CMD as u8,
                                          &mut hdr, Some(&data_in)));

        Ok((DeviceInfo {hdr: hdr},
           match (hdr.flags & DM_DATA_OUT.bits) > 0 {
               true =>
                   Some(String::from_utf8_lossy(
                       &data_out[..data_out.len()-1]).into_owned()),
               false => None
           }))
    }

    /// Unimplemented.
    pub fn device_set_geometry(&self, _flags: DmFlags) {
        unimplemented!()
    }

    /// Recursively walk DM deps to see if `dev` might be its own dependency.
    pub fn depends_on(&self, dev: Device, dm_majors: &BTreeSet<u32>) -> bool {
        if !dm_majors.contains(&dev.major) {
            return false;
        }

        if let Ok(dep_list) = self.table_deps(dev, DmFlags::empty()) {
            for d in dep_list {
                if d == dev {
                    return true;
                } else if self.depends_on(d, dm_majors) {
                    return true;
                }
            }
        }

        false
    }
}

//
// Return up to the first \0, or None
//
fn slice_to_null(slc: &[u8]) -> Option<&[u8]> {
    for (i, c) in slc.iter().enumerate() {
        if *c == b'\0' { return Some(&slc[..i]) };
    }
    None
}
