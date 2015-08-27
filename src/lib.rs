// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(slice_bytes, path_ext, iter_arith)]

extern crate libc;
extern crate byteorder;
extern crate nix;

/// Low-level devicemapper configuration of the running kernel.

#[allow(dead_code, non_camel_case_types)]
mod dm_ioctl;
mod util;

use std::fs::{File, PathExt};
use std::io::{Result, Error, BufReader, BufRead};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::io::ErrorKind::Other;
use std::os::unix::io::AsRawFd;
use std::mem;
use std::slice;
use std::slice::bytes::copy_memory;
use std::collections::BTreeSet;
use std::os::unix::fs::MetadataExt;

use byteorder::{NativeEndian, ByteOrder};

use nix::sys::ioctl;

use dm_ioctl as dmi;
use util::align_to;

const DM_IOCTL: u8 = 0xfd;
const DM_CTL_PATH: &'static str= "/dev/mapper/control";

const DM_VERSION_MAJOR: u32 = 4;
const DM_VERSION_MINOR: u32 = 30;
const DM_VERSION_PATCHLEVEL: u32 = 0;

const DM_IOCTL_STRUCT_LEN: usize = 312;
const DM_NAME_LEN: usize = 128;
const DM_UUID_LEN: usize = 129;

// Status bits
//const DM_READONLY_FLAG: u32 = 1;
const DM_SUSPEND_FLAG: u32 = 2;
//const DM_PERSISTENT_DEV_FLAG: u32 = 8;

const DM_STATUS_TABLE_FLAG: u32 = (1 << 4);

#[derive(Debug, Clone, Copy)]
pub enum StatusType {
    Info,
    Table,
}

/// A struct containing the device's major and minor numbers
///
/// Also allows conversion to/from a single 64bit value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Device {
    /// Device major number
    pub major: u32,
    /// Device minor number
    pub minor: u8,
}

impl Device {
    /// Returns the path in `/dev` that corresponds with the device number
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
    fn from_str(s: &str) -> Result<Device> {
        match s.parse::<i64>() {
            Ok(x) => Ok(Device::from(x as u64)),
            Err(_) => {
                match Path::new(s).metadata() {
                    Ok(x) => Ok(Device::from(x.rdev())),
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

/// Context needed for communicating with devicemapper.
pub struct DM {
    file: File,
}

impl DM {
    /// Create a new context for communicating with DM.
    pub fn new() -> Result<DM> {
        Ok(DM {
            file: try!(File::open(DM_CTL_PATH)),
        })
    }

    fn initialize_hdr(hdr: &mut dmi::Struct_dm_ioctl) -> () {
        hdr.version[0] = DM_VERSION_MAJOR;
        hdr.version[1] = DM_VERSION_MINOR;
        hdr.version[2] = DM_VERSION_PATCHLEVEL;

        hdr.data_start = mem::size_of::<dmi::Struct_dm_ioctl>() as u32;
    }

    fn hdr_set_name(hdr: &mut dmi::Struct_dm_ioctl, name: &str) -> () {
        let name_dest: &mut [u8; DM_NAME_LEN] = unsafe { mem::transmute(&mut hdr.name) };
        copy_memory(name.as_bytes(), &mut name_dest[..]);
    }

    fn hdr_set_uuid(hdr: &mut dmi::Struct_dm_ioctl, uuid: &str) -> () {
        let uuid_dest: &mut [u8; DM_UUID_LEN] = unsafe { mem::transmute(&mut hdr.uuid) };
        copy_memory(uuid.as_bytes(), &mut uuid_dest[..]);
    }

    /// Devicemapper version information: Major, Minor, and patchlevel versions.
    pub fn version(&self) -> Result<(u32, u32, u32)> {

        let mut hdr: dmi::Struct_dm_ioctl = Default::default();
        hdr.version[0] = DM_VERSION_MAJOR;
        hdr.version[1] = DM_VERSION_MINOR;
        hdr.version[2] = DM_VERSION_PATCHLEVEL;

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_VERSION_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => {},
        };

        Ok((hdr.version[0], hdr.version[1], hdr.version[2]))
    }

    /// Remove all DM devices.
    pub fn remove_all(&self) -> Result<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_REMOVE_ALL_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(())
        }
    }

    /// Returns a list of tuples containing DM device names and their
    /// major/minor device numbers.
    pub fn list_devices(&self) -> Result<Vec<(String, Device)>> {
        let mut buf = [0u8; 16 * 1024];
        let mut hdr: &mut dmi::Struct_dm_ioctl = unsafe {mem::transmute(&mut buf)};

        Self::initialize_hdr(&mut hdr);
        hdr.data_size = buf.len() as u32;

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_LIST_DEVICES_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut buf) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => {},
        };

        let mut devs = Vec::new();
        if (hdr.data_size - hdr.data_start as u32) != 0 {
            let mut result = &buf[hdr.data_start as usize..];

            loop {
                let device: &dmi::Struct_dm_name_list = unsafe {
                    mem::transmute(result.as_ptr())
                };

                let slc = slice_to_null(
                    &result[mem::size_of::<dmi::Struct_dm_name_list>()..])
                    .expect("Bad data from ioctl");
                let dm_name = String::from_utf8_lossy(slc).into_owned();
                devs.push((dm_name, device.dev.into()));

                if device.next == 0 { break }

                result = &result[device.next as usize..];
            }
        }

        Ok(devs)
    }

    /// Create a DM device.
    pub fn device_create(&self, name: &str, id: &str) -> Result<Device> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);
        Self::hdr_set_uuid(&mut hdr, id);

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_DEV_CREATE_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => { }
        };

        Ok(Device::from(hdr.dev))
    }

    /// Remove a DM device.
    pub fn device_remove(&self, name: &str) -> Result<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_DEV_REMOVE_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(())
        }
    }

    /// Rename a DM device.
    pub fn device_rename(&self, old_name: &str, new_name: &str) -> Result<()> {
        let mut buf = [0u8; DM_IOCTL_STRUCT_LEN + DM_NAME_LEN];
        let mut hdr: &mut dmi::Struct_dm_ioctl = unsafe {mem::transmute(&mut buf)};

        if new_name.as_bytes().len() > (DM_NAME_LEN - 1) {
            return Err(
                Error::new(Other, format!("New name {} too long", new_name)));
        }

        Self::initialize_hdr(&mut hdr);
        hdr.data_size = buf.len() as u32;
        Self::hdr_set_name(&mut hdr, old_name);

        copy_memory(new_name.as_bytes(), &mut buf[DM_IOCTL_STRUCT_LEN..]);

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_DEV_RENAME_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(())
        }
    }

    /// Suspend a DM device.
    pub fn device_suspend(&self, name: &str) -> Result<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);
        hdr.flags = DM_SUSPEND_FLAG;

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_DEV_SUSPEND_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(())
        }
    }

    /// Resume a DM device.
    pub fn device_resume(&self, name: &str) -> Result<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);
        // DM_SUSPEND_FLAG not set = resume

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_DEV_SUSPEND_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(())
        }
    }

    /// Get device status.
    pub fn device_status(&self, name: &str) -> Result<dmi::Struct_dm_ioctl> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);
        // DM_SUSPEND_FLAG not set = resume

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_DEV_SUSPEND_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(hdr)
        }
    }

    pub fn device_wait(&self, _name: &str) -> Result<()> {
        unimplemented!()
    }

    /// Load targets for a device.
    /// `targets` is a Vec of (sector_start, sector_length, type, params)
    pub fn table_load(&self, name: &str, targets: &Vec<(u64, u64, &str, &str)>) -> Result<()> {
        let mut targs = Vec::new();

        // Construct targets first, since we need to know how many & size
        // before initializing the header.
        for t in targets {
            let mut targ: dmi::Struct_dm_target_spec = Default::default();
            targ.sector_start = t.0;
            targ.length = t.1;
            targ.status = 0;

            let mut dst: &mut [u8] = unsafe {
                mem::transmute(&mut targ.target_type[..])
            };
            copy_memory(t.2.as_bytes(), &mut dst);

            let mut params = t.3.to_string();

            let pad_bytes = align_to(
                params.len() + 1usize, 8usize) - params.len();
            params.extend(vec!["\0"; pad_bytes]);

            targ.next = (mem::size_of::<dmi::Struct_dm_target_spec>()
                         + params.len()) as u32;

            targs.push((targ, params));
        }

        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);

        hdr.data_start = mem::size_of::<dmi::Struct_dm_ioctl>() as u32;
        hdr.data_size = hdr.data_start + targs.iter()
            .map(|&(t, _)| t.next)
            .sum::<u32>();
        hdr.target_count = targs.len() as u32;

        // Flatten into buf
        let mut buf: Vec<u8> = Vec::with_capacity(hdr.data_size as usize);
        unsafe {
            let ptr: *mut u8 = mem::transmute(&mut hdr);
            let slc = slice::from_raw_parts(ptr, hdr.data_start as usize);
            buf.extend(slc);
        }

        for (targ, param) in targs {
            unsafe {
                let ptr: *mut u8 = mem::transmute(&targ);
                let slc = slice::from_raw_parts(
                    ptr, mem::size_of::<dmi::Struct_dm_target_spec>());
                buf.extend(slc);
            }

            buf.extend(param.as_bytes());
        }

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_TABLE_LOAD_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into_ptr(self.file.as_raw_fd(), op, buf.as_mut_ptr()) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(())
        }
    }

    /// Clear the table for a device.
    pub fn table_clear(&self, name: &str) -> Result<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_TABLE_CLEAR_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut hdr) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => Ok(())
        }
    }

    /// Query DM for which devices depend on this device.
    pub fn table_deps(&self, dev: Device) -> Result<Vec<Device>> {
        let mut buf = [0u8; 16 * 1024];
        let mut hdr: &mut dmi::Struct_dm_ioctl = unsafe {mem::transmute(&mut buf)};

        Self::initialize_hdr(&mut hdr);
        hdr.data_size = buf.len() as u32;
        hdr.dev = dev.into();

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_TABLE_DEPS_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut buf) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => {},
        };

        // TODO: Check DM_BUFFER_FULL_FLAG for:
        // DM_DEVICE_LIST_VERSIONS, DM_DEVICE_LIST, DM_DEVICE_DEPS,
        // DM_DEVICE_STATUS, DM_DEVICE_TABLE, DM_DEVICE_WAITEVENT,
        // DM_DEVICE_TARGET_MSG

        let mut devs = Vec::new();
        if (hdr.data_size - hdr.data_start as u32) != 0 {
            let result = &buf[hdr.data_start as usize..];
            let entries = NativeEndian::read_u32(&result[..4]) as usize;

            for entry in 0..entries {
                let dev = &result[(8*entry)+8..(8*entry)+16];
                devs.push(Device::from(NativeEndian::read_u64(&dev)));
            }
        }

        Ok(devs)
    }

    /// Return the status of all targets for a device.
    /// Returned is a Vec of (sector_start, sector_length, type, params).
    pub fn table_status(&self, name: &str, statustype: StatusType)
                        -> Result<Vec<(u64, u64, String, String)>> {
        let mut buf = [0u8; 16 * 1024];
        let mut hdr: &mut dmi::Struct_dm_ioctl = unsafe {mem::transmute(&mut buf)};

        Self::initialize_hdr(&mut hdr);
        Self::hdr_set_name(&mut hdr, name);
        hdr.data_size = buf.len() as u32;
        hdr.flags = match statustype {
            StatusType::Info => 0,
            StatusType::Table => DM_STATUS_TABLE_FLAG,
        };

        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_TABLE_STATUS_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut buf) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => {},
        };

        let mut targets = Vec::new();
        if (hdr.data_size - hdr.data_start as u32) != 0 {
            let mut result = &buf[hdr.data_start as usize..];

            for _ in 0..hdr.target_count {
                let targ: &dmi::Struct_dm_target_spec = unsafe {
                    mem::transmute(result.as_ptr())
                };

                let target_type = unsafe {
                    let cast: &[u8; 16] = mem::transmute(&targ.target_type);
                    let slc = slice_to_null(cast).expect("bad data from ioctl");
                    String::from_utf8_lossy(slc).into_owned()
                };

                let params = {
                    let slc = slice_to_null(
                        &result[mem::size_of::<dmi::Struct_dm_target_spec>()..])
                        .expect("bad data from ioctl");
                    String::from_utf8_lossy(slc).into_owned()
                };

                targets.push((targ.sector_start, targ.length, target_type, params));

                result = &result[targ.next as usize..];
            }
        }

        Ok(targets)
    }

    pub fn list_versions(&self) -> Result<Vec<(String, u32, u32, u32)>> {
        let mut buf = [0u8; 16 * 1024];
        let mut hdr: &mut dmi::Struct_dm_ioctl = unsafe {mem::transmute(&mut buf)};

        Self::initialize_hdr(&mut hdr);
        hdr.data_size = buf.len() as u32;
        let op = ioctl::op_read_write(DM_IOCTL, dmi::DM_LIST_VERSIONS_CMD as u8,
                                      mem::size_of::<dmi::Struct_dm_ioctl>());

        match unsafe { ioctl::read_into(self.file.as_raw_fd(), op, &mut buf) } {
            Err(_) => return Err((Error::last_os_error())),
            _ => {},
        };

        let mut targets = Vec::new();
        if (hdr.data_size - hdr.data_start as u32) != 0 {
            let mut result = &buf[hdr.data_start as usize..];

            loop {
                let tver: &dmi::Struct_dm_target_versions = unsafe {
                    mem::transmute(result.as_ptr())
                };

                let name_slc = slice_to_null(
                    &result[mem::size_of::<dmi::Struct_dm_target_versions>()..])
                    .expect("bad data from ioctl");
                let name = String::from_utf8_lossy(name_slc).into_owned();
                targets.push((name, tver.version[0], tver.version[1], tver.version[2]));

                if tver.next == 0 { break }

                result = &result[tver.next as usize..];
            }
        }

        Ok(targets)
    }

    pub fn target_msg(&self) -> Result<()> {
        unimplemented!()
    }

    pub fn device_set_geometry(&self) {
        unimplemented!()
    }

    /// Recursively walk DM deps to see if `dev` might be its own dependency.
    pub fn depends_on(&self, dev: Device, dm_majors: &BTreeSet<u32>) -> bool {
        if !dm_majors.contains(&dev.major) {
            return false;
        }

        if let Ok(dep_list) = self.table_deps(dev) {
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
