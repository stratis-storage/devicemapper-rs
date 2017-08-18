// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
use std::fs::File;
use std::io::Error;
use std::os::unix::io::AsRawFd;
use std::mem::{size_of, transmute};
use std::slice;
use std::collections::BTreeSet;
use std::cmp;
use std::thread;
use std::time::Duration;

use nix::libc::ioctl as nix_ioctl;
use nix::libc::c_ulong;

use dm_ioctl as dmi;
use util::align_to;

use consts::{DM_NAME_LEN, DM_UUID_LEN, MIN_BUF_SIZE, DM_IOCTL, DmFlags, DM_CTL_PATH,
             DM_VERSION_MAJOR, DM_VERSION_MINOR, DM_VERSION_PATCHLEVEL, DM_READONLY, DM_SUSPEND,
             DM_PERSISTENT_DEV, DM_STATUS_TABLE, DM_BUFFER_FULL, DM_SKIP_LOCKFS, DM_NOFLUSH,
             DM_QUERY_INACTIVE_TABLE, DM_UUID, DM_DATA_OUT, DM_DEFERRED_REMOVE};
use device::Device;
use deviceinfo::DeviceInfo;
use result::{DmError, DmResult, ErrorEnum};
use types::{Sectors, TargetLine, TargetLineArg};
use util::slice_to_null;

/// Used as a parameter for functions that take either a Device name
/// or a Device UUID.
pub enum DevId<'a> {
    /// The parameter is the device's name
    Name(&'a str),
    /// The parameter is the device's "DM UUID"
    /// Note that this UUID is not a canonical UUID, but rather a
    /// devicemapper-specific format that is supposed to be unique.
    Uuid(&'a str),
}

/// Context needed for communicating with devicemapper.
pub struct DM {
    file: File,
}

impl DM {
    /// Create a new context for communicating with DM.
    pub fn new() -> DmResult<DM> {
        Ok(DM { file: File::open(DM_CTL_PATH)? })
    }

    /// The /dev/mapper/<name> device is not immediately available for use.
    /// TODO: Implement wait for event or poll.
    pub fn wait_for_dm() {
        thread::sleep(Duration::from_millis(500))
    }

    fn initialize_hdr(hdr: &mut dmi::Struct_dm_ioctl, flags: DmFlags) -> () {
        hdr.version[0] = DM_VERSION_MAJOR;
        hdr.version[1] = DM_VERSION_MINOR;
        hdr.version[2] = DM_VERSION_PATCHLEVEL;

        hdr.flags = flags.bits();

        hdr.data_start = size_of::<dmi::Struct_dm_ioctl>() as u32;
    }

    fn hdr_set_name(hdr: &mut dmi::Struct_dm_ioctl, name: &str) -> () {
        let name_dest: &mut [u8; DM_NAME_LEN] = unsafe { transmute(&mut hdr.name) };
        let len = name.as_bytes().len();
        name_dest[..len].clone_from_slice(name.as_bytes());
    }

    /// Set the devicemapper "UUID".
    /// Note that this is a devicemapper specific id value, not a UUID.
    fn hdr_set_uuid(hdr: &mut dmi::Struct_dm_ioctl, uuid: &str) -> () {
        let uuid_dest: &mut [u8; DM_UUID_LEN] = unsafe { transmute(&mut hdr.uuid) };
        let len = uuid.as_bytes().len();
        uuid_dest[..len].clone_from_slice(uuid.as_bytes());
    }

    // Give this a filled-in header and optionally add'l stuff.
    // Does the ioctl and maybe returns stuff. Handles BUFFER_FULL flag.
    //
    fn do_ioctl(&self,
                ioctl: u8,
                hdr: &mut dmi::Struct_dm_ioctl,
                in_data: Option<&[u8]>)
                -> DmResult<Vec<u8>> {
        // Create in-buf by copying hdr and any in-data into a linear
        // Vec v.  'hdr_slc' also aliases hdr as a &[u8], used first
        // to copy the hdr into v, and later to update the
        // possibly-modified hdr.

        // Start with a large buffer to make BUFFER_FULL rare. Libdm
        // does this too.
        hdr.data_size = cmp::max(MIN_BUF_SIZE,
                                 size_of::<dmi::Struct_dm_ioctl>() +
                                 in_data.map_or(0, |x| x.len())) as u32;
        let mut v: Vec<u8> = Vec::with_capacity(hdr.data_size as usize);

        let hdr_slc = unsafe {
            let len = hdr.data_start as usize;
            let ptr = hdr as *mut dmi::Struct_dm_ioctl as *mut u8;
            slice::from_raw_parts_mut(ptr, len)
        };

        v.extend_from_slice(hdr_slc);
        if let Some(in_data) = in_data {
            v.extend(in_data.iter().cloned());
        }

        // zero out the rest
        let cap = v.capacity();
        v.resize(cap, 0);

        let op = iorw!(DM_IOCTL, ioctl, size_of::<dmi::Struct_dm_ioctl>()) as c_ulong;
        loop {
            if unsafe { convert_ioctl_res!(nix_ioctl(self.file.as_raw_fd(), op, v.as_mut_ptr())) }
                   .is_err() {
                return Err(DmError::Io(Error::last_os_error()));
            }

            let hdr = unsafe {
                (v.as_mut_ptr() as *mut dmi::Struct_dm_ioctl)
                    .as_mut()
                    .unwrap()
            };

            if (hdr.flags & DM_BUFFER_FULL.bits()) == 0 {
                break;
            }

            let len = v.len();
            v.resize(len * 2, 0);
            hdr.data_size = v.len() as u32;
        }

        let hdr = unsafe {
            (v.as_mut_ptr() as *mut dmi::Struct_dm_ioctl)
                .as_mut()
                .unwrap()
        };

        // hdr possibly modified so copy back
        hdr_slc.clone_from_slice(&v[..hdr.data_start as usize]);

        // Maybe we got some add'l data back?
        let new_data_off = cmp::max(hdr.data_start, hdr.data_size);
        Ok(v[hdr.data_start as usize..new_data_off as usize].to_vec())
    }

    /// Devicemapper version information: Major, Minor, and patchlevel versions.
    pub fn version(&self) -> DmResult<(u32, u32, u32)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());

        self.do_ioctl(dmi::DM_VERSION_CMD as u8, &mut hdr, None)?;

        Ok((hdr.version[0], hdr.version[1], hdr.version[2]))
    }

    /// Remove all DM devices and tables. Use discouraged other than
    /// for debugging.
    ///
    /// If DM_DEFERRED_REMOVE is set, the request will succeed for
    /// in-use devices, and they will be removed when released.
    ///
    /// Valid flags: DM_DEFERRED_REMOVE
    pub fn remove_all(&self, flags: DmFlags) -> DmResult<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_DEFERRED_REMOVE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);

        self.do_ioctl(dmi::DM_REMOVE_ALL_CMD as u8, &mut hdr, None)?;

        Ok(())
    }

    /// Returns a list of tuples containing DM device names and a
    /// Device, which holds their major and minor device numbers.
    pub fn list_devices(&self) -> DmResult<Vec<(String, Device)>> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());

        let data_out = self.do_ioctl(dmi::DM_LIST_DEVICES_CMD as u8, &mut hdr, None)?;

        let mut devs = Vec::new();
        if !data_out.is_empty() {
            let mut result = &data_out[..];

            loop {
                let device = unsafe {
                    (result.as_ptr() as *const dmi::Struct_dm_name_list)
                        .as_ref()
                        .unwrap()
                };

                let slc = slice_to_null(&result[size_of::<dmi::Struct_dm_name_list>()..])
                    .expect("Bad data from ioctl");
                let dm_name = String::from_utf8_lossy(slc).into_owned();
                devs.push((dm_name, device.dev.into()));

                if device.next == 0 {
                    break;
                }

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
    /// use devicemapper::DM;
    /// use devicemapper::consts::DmFlags;
    /// let dm = DM::new().unwrap();
    ///
    /// // Setting a devicemapper uuid is optional
    /// let dev = dm.device_create("example-dev", None, DmFlags::empty()).unwrap();
    /// ```
    pub fn device_create(&self,
                         name: &str,
                         uuid: Option<&str>,
                         flags: DmFlags)
                         -> DmResult<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = (DM_READONLY | DM_PERSISTENT_DEV) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);

        Self::hdr_set_name(&mut hdr, name);
        if let Some(uuid) = uuid {
            Self::hdr_set_uuid(&mut hdr, uuid);
        }

        self.do_ioctl(dmi::DM_DEV_CREATE_CMD as u8, &mut hdr, None)?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Remove a DM device and its mapping tables.
    ///
    /// If DM_DEFERRED_REMOVE is set, the request for an in-use
    /// devices will succeed, and it will be removed when no longer
    /// used.
    ///
    /// Valid flags: DM_DEFERRED_REMOVE
    pub fn device_remove(&self, name: &DevId, flags: DmFlags) -> DmResult<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_DEFERRED_REMOVE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        self.do_ioctl(dmi::DM_DEV_REMOVE_CMD as u8, &mut hdr, None)?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Reset a device's name, or set, for the first and only time its
    /// devicemapper uuid.
    ///
    /// Prerequisite: if new == DevId::Name(new_name), old_name != new_name
    /// Note: Possibly surprisingly, returned DeviceInfo contains the old
    /// data for name or uuid, not the newly set value.
    pub fn device_rename(&self, old_name: &str, new: DevId) -> DmResult<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let mut data_in = match new {
            DevId::Name(new_name) => {
                Self::initialize_hdr(&mut hdr, DmFlags::empty());
                Self::hdr_set_name(&mut hdr, old_name);

                let data_in = new_name.as_bytes();
                if data_in.len() > DM_NAME_LEN - 1 {
                    let err_msg = format!("New value {} too long", new_name);
                    return Err(DmError::Dm(ErrorEnum::Invalid, err_msg.into()));
                }
                data_in.to_vec()
            }
            DevId::Uuid(new_uuid) => {
                Self::initialize_hdr(&mut hdr, DM_UUID);
                Self::hdr_set_name(&mut hdr, old_name);

                let data_in = new_uuid.as_bytes();
                if data_in.len() > DM_UUID_LEN - 1 {
                    let err_msg = format!("New value {} too long", new_uuid);
                    return Err(DmError::Dm(ErrorEnum::Invalid, err_msg.into()));
                }
                data_in.to_vec()
            }
        };

        data_in.push(b'\0');

        self.do_ioctl(dmi::DM_DEV_RENAME_CMD as u8, &mut hdr, Some(&data_in))?;

        Ok(DeviceInfo::new(hdr))
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
    /// use devicemapper::{DM, DevId};
    /// use devicemapper::consts::{DmFlags, DM_SUSPEND};

    /// let dm = DM::new().unwrap();
    ///
    /// dm.device_suspend(&DevId::Name("example-dev"), DM_SUSPEND).unwrap();
    /// ```
    pub fn device_suspend(&self, name: &DevId, flags: DmFlags) -> DmResult<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = (DM_SUSPEND | DM_NOFLUSH | DM_SKIP_LOCKFS) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        self.do_ioctl(dmi::DM_DEV_SUSPEND_CMD as u8, &mut hdr, None)?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Get DeviceInfo for a device. This is also returned by other
    /// methods, but if just the DeviceInfo is desired then this just
    /// gets it.
    pub fn device_status(&self, name: &DevId) -> DmResult<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        self.do_ioctl(dmi::DM_DEV_STATUS_CMD as u8, &mut hdr, None)?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Wait for a device to report an event.
    ///
    /// Once an event occurs, this function behaves just like
    /// `table_status`, see that function for more details.
    ///
    /// This interface is not very friendly to monitoring multiple devices.
    /// Events are also exported via uevents, that method may be preferable.
    pub fn device_wait(&self,
                       name: &DevId,
                       flags: DmFlags)
                       -> DmResult<(DeviceInfo, Vec<TargetLine>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_QUERY_INACTIVE_TABLE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        let data_out = self.do_ioctl(dmi::DM_DEV_WAIT_CMD as u8, &mut hdr, None)?;

        let status = DM::parse_table_status(hdr.target_count, &data_out);

        Ok((DeviceInfo::new(hdr), status))

    }

    /// Load targets for a device into its inactive table slot.
    ///
    /// `targets` is an array of (sector_start, sector_length, type, params).
    ///
    /// `params` are target-specific, please see [Linux kernel documentation]
    /// https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/tree/ ->
    /// Documentation/device-mapper
    /// for more.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use devicemapper::{DM, DevId, Sectors};
    /// use devicemapper::consts::DmFlags;
    /// let dm = DM::new().unwrap();
    ///
    /// // Create a 16MiB device (32768 512-byte sectors) that maps to /dev/sdb1
    /// // starting 1MiB into sdb1
    /// let table = vec![(Sectors(0),
    ///                   Sectors(32768),
    ///                   "linear",
    ///                   "/dev/sdb1 2048")];
    ///
    /// dm.table_load(&DevId::Name("example-dev"), &table).unwrap();
    /// ```
    pub fn table_load<T1, T2>(&self,
                              name: &DevId,
                              targets: &[TargetLineArg<T1, T2>])
                              -> DmResult<DeviceInfo>
        where T1: AsRef<str>,
              T2: AsRef<str>
    {
        let mut targs = Vec::new();

        // Construct targets first, since we need to know how many & size
        // before initializing the header.
        for t in targets {
            let mut targ: dmi::Struct_dm_target_spec = Default::default();
            targ.sector_start = *t.0;
            targ.length = *t.1;
            targ.status = 0;

            let dst: &mut [u8] = unsafe { transmute(&mut targ.target_type[..]) };
            let ttyp = t.2.as_ref();
            let ttyp_len = ttyp.len();
            if ttyp_len > dst.len() {
                return Err(DmError::Dm(ErrorEnum::Invalid, "target type too long".into()));
            }
            dst[..ttyp_len].clone_from_slice(ttyp.as_bytes());

            let mut params = t.3.as_ref().to_owned();
            let params_len = params.len();
            let pad_bytes = align_to(params_len + 1usize, 8usize) - params_len;
            params.extend(vec!["\0"; pad_bytes]);

            targ.next = (size_of::<dmi::Struct_dm_target_spec>() + params.len()) as u32;

            targs.push((targ, params));
        }

        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        // io_ioctl() will set hdr.data_size but we must set target_count
        hdr.target_count = targs.len() as u32;

        // Flatten targets into a buf
        let mut data_in = Vec::new();

        for (targ, param) in targs {
            unsafe {
                let ptr = &targ as *const dmi::Struct_dm_target_spec as *mut u8;
                let slc = slice::from_raw_parts(ptr, size_of::<dmi::Struct_dm_target_spec>());
                data_in.extend_from_slice(slc);
            }

            data_in.extend(param.as_bytes());
        }

        self.do_ioctl(dmi::DM_TABLE_LOAD_CMD as u8, &mut hdr, Some(&data_in))?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Clear the "inactive" table for a device.
    pub fn table_clear(&self, name: &DevId) -> DmResult<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        self.do_ioctl(dmi::DM_TABLE_CLEAR_CMD as u8, &mut hdr, None)?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Query DM for which devices are referenced by the "active"
    /// table for this device.
    ///
    /// If DM_QUERY_INACTIVE_TABLE is set, instead return for the
    /// inactive table.
    ///
    /// Valid flags: DM_QUERY_INACTIVE_TABLE
    pub fn table_deps(&self, dev: Device, flags: DmFlags) -> DmResult<Vec<Device>> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_QUERY_INACTIVE_TABLE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        hdr.dev = dev.into();

        let data_out = self.do_ioctl(dmi::DM_TABLE_DEPS_CMD as u8, &mut hdr, None)?;

        if data_out.is_empty() {
            Ok(vec![])
        } else {
            let result = &data_out[..];
            let target_deps = unsafe {
                (result.as_ptr() as *const dmi::Struct_dm_target_deps)
                    .as_ref()
                    .unwrap()
            };

            let dev_slc = unsafe {
                slice::from_raw_parts(result[size_of::<dmi::Struct_dm_target_deps>()..].as_ptr() as
                                      *const u64,
                                      target_deps.count as usize)
            };

            Ok(dev_slc.iter().map(|d| Device::from(*d)).collect())
        }
    }

    /// Parse a device's table. The table value is in buf, count indicates the
    /// expected number of lines.
    /// Panics if there is an error parsing the table.
    /// Trims trailing white space off final entry on each line. This
    /// canonicalization makes checking identity of tables easier.
    // The justification for this is that if there was no error in obtaining
    // the table the data is correct and complete. Therefore, an error in
    // parsing can only result from a change in the kernel. We assume that
    // Stratis will know in advance about such changes.
    fn parse_table_status(count: u32, buf: &[u8]) -> Vec<TargetLine> {
        let mut targets = Vec::new();
        if !buf.is_empty() {
            let mut next_off = 0;
            let mut result = &buf[..];

            for _ in 0..count {
                result = &result[next_off..];
                let targ = unsafe {
                    (result.as_ptr() as *const dmi::Struct_dm_target_spec)
                        .as_ref()
                        .expect("assume all parsing succeeds")
                };

                let target_type = unsafe {
                    let cast: &[u8; 16] = transmute(&targ.target_type);
                    let slc = slice_to_null(cast).expect("assume all parsing succeeds");
                    String::from_utf8_lossy(slc).into_owned()
                };

                let params = {
                    let slc = slice_to_null(&buf[size_of::<dmi::Struct_dm_target_spec>()..])
                        .expect("assume all parsing succeeds");
                    String::from_utf8_lossy(slc).trim_right().to_owned()
                };

                targets.push((Sectors(targ.sector_start),
                              Sectors(targ.length),
                              target_type,
                              params));

                next_off = targ.next as usize;
            }
        }
        targets
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
    /// use devicemapper::{DM, DevId};
    /// use devicemapper::consts::{DM_STATUS_TABLE, DmFlags};
    /// let dm = DM::new().unwrap();
    ///
    /// let res = dm.table_status(&DevId::Name("example-dev"), DM_STATUS_TABLE).unwrap();
    /// println!("{} {:?}", res.0.name(), res.1);
    /// ```
    pub fn table_status(&self,
                        name: &DevId,
                        flags: DmFlags)
                        -> DmResult<(DeviceInfo, Vec<TargetLine>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = (DM_NOFLUSH | DM_STATUS_TABLE | DM_QUERY_INACTIVE_TABLE) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        let data_out = self.do_ioctl(dmi::DM_TABLE_STATUS_CMD as u8, &mut hdr, None)?;

        let status = DM::parse_table_status(hdr.target_count, &data_out);

        Ok((DeviceInfo::new(hdr), status))
    }

    /// Returns a list of each loaded target type with its name, and
    /// version broken into major, minor, and patchlevel.
    pub fn list_versions(&self) -> DmResult<Vec<(String, u32, u32, u32)>> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());

        let data_out = self.do_ioctl(dmi::DM_LIST_VERSIONS_CMD as u8, &mut hdr, None)?;

        let mut targets = Vec::new();
        if !data_out.is_empty() {
            let mut result = &data_out[..];

            loop {
                let tver = unsafe {
                    (result.as_ptr() as *const dmi::Struct_dm_target_versions)
                        .as_ref()
                        .unwrap()
                };

                let name_slc = slice_to_null(&result
                                                  [size_of::<dmi::Struct_dm_target_versions>()..])
                        .expect("bad data from ioctl");
                let name = String::from_utf8_lossy(name_slc).into_owned();
                targets.push((name, tver.version[0], tver.version[1], tver.version[2]));

                if tver.next == 0 {
                    break;
                }

                result = &result[tver.next as usize..];
            }
        }

        Ok(targets)
    }

    /// Send a message to the target at a given sector. If sector is
    /// not needed use 0.  DM-wide messages start with '@', and may
    /// return a string; targets do not.
    pub fn target_msg(&self,
                      name: &DevId,
                      sector: Sectors,
                      msg: &str)
                      -> DmResult<(DeviceInfo, Option<String>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match *name {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        let mut msg_struct: dmi::Struct_dm_target_msg = Default::default();
        msg_struct.sector = *sector;
        let mut data_in = unsafe {
            let ptr = &msg_struct as *const dmi::Struct_dm_target_msg as *mut u8;
            slice::from_raw_parts(ptr, size_of::<dmi::Struct_dm_target_msg>()).to_vec()
        };

        data_in.extend(msg.as_bytes());
        data_in.push(b'\0');

        let data_out = self.do_ioctl(dmi::DM_TARGET_MSG_CMD as u8, &mut hdr, Some(&data_in))?;

        Ok((DeviceInfo::new(hdr),
            if (hdr.flags & DM_DATA_OUT.bits()) > 0 {
                Some(String::from_utf8_lossy(&data_out[..data_out.len() - 1]).into_owned())
            } else {
                None
            }))
    }

    /// Recursively walk DM deps to see if `dev` might be its own dependency.
    pub fn depends_on(&self, dev: Device, dm_majors: &BTreeSet<u32>) -> DmResult<bool> {
        if !dm_majors.contains(&dev.major) {
            return Ok(false);
        }

        for d in self.table_deps(dev, DmFlags::empty())? {
            if d == dev || self.depends_on(d, dm_majors)? {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use {DevId, DM};
    use consts::{DmFlags, DM_STATUS_TABLE};

    #[test]
    /// Test that some version can be obtained.
    fn sudo_test_version() {
        assert!(DM::new().unwrap().version().is_ok());
    }

    #[test]
    /// Test that versions for some targets can be obtained.
    fn sudo_test_versions() {
        assert!(!DM::new().unwrap().list_versions().unwrap().is_empty());
    }

    #[test]
    /// Verify that if no devices have been created the list is empty.
    fn sudo_test_list_devices_empty() {
        assert!(DM::new().unwrap().list_devices().unwrap().is_empty());
    }

    #[test]
    /// Verify that if one device has been created, it will be the only device
    /// listed.
    fn sudo_test_list_devices() {
        let dm = DM::new().unwrap();
        let name = "example-dev";
        dm.device_create(name, None, DmFlags::empty()).unwrap();

        let devices;
        loop {
            if let Ok(list) = dm.list_devices() {
                devices = list;
                break;
            }
        }

        assert_eq!(devices.len(), 1);
        assert!(devices[0].0 == name);
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Test that device creation gives a device with the expected name.
    fn sudo_test_create() {
        let dm = DM::new().unwrap();
        let name = "example-dev";
        let result = dm.device_create(name, None, DmFlags::empty()).unwrap();
        assert!(result.name() == name);
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Test that device rename to same name fails.
    /// This is unfortunate, but appears to be true.
    fn sudo_test_rename_id() {
        let dm = DM::new().unwrap();
        let name = "example-dev";
        dm.device_create(name, None, DmFlags::empty()).unwrap();
        DM::wait_for_dm();
        assert!(dm.device_rename(name, DevId::Name(name)).is_err());
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Test that device rename to different name works.
    /// Verify that the only device in the list of devices is a device with
    /// the new name.
    fn sudo_test_rename() {
        let dm = DM::new().unwrap();
        let name = "example-dev";
        dm.device_create(name, None, DmFlags::empty()).unwrap();

        let new_name = "example-dev-2";
        loop {
            if dm.device_rename(name, DevId::Name(new_name)).is_ok() {
                break;
            }
        }

        assert!(dm.device_status(&DevId::Name(name)).is_err());
        assert!(dm.device_status(&DevId::Name(new_name)).is_ok());

        let devices = dm.list_devices().unwrap();
        assert_eq!(devices.len(), 1);
        assert_eq!(devices[0].0, new_name);

        dm.device_remove(&DevId::Name(new_name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Renaming a device that does not exist yields an error.
    fn sudo_test_rename_non_existant() {
        assert!(DM::new()
                    .unwrap()
                    .device_rename("old_name", DevId::Name("new_name"))
                    .is_err());
    }

    #[test]
    /// Removing a device that does not exist yields an error, unfortunately.
    fn sudo_test_remove_non_existant() {
        assert!(DM::new()
                    .unwrap()
                    .device_remove(&DevId::Name("junk"), DmFlags::empty())
                    .is_err());
    }

    #[test]
    /// A newly created device has no deps.
    fn sudo_test_empty_deps() {
        let dm = DM::new().unwrap();
        let name = "example-dev";
        let result = dm.device_create(name, None, DmFlags::empty()).unwrap();
        let device = result.device();

        let deps;
        loop {
            if let Ok(list) = dm.table_deps(device, DmFlags::empty()) {
                deps = list;
                break;
            }
        }

        assert!(deps.is_empty());
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Table status on a non-existant name should return an error.
    fn sudo_test_table_status_non_existant() {
        assert!(DM::new()
                    .unwrap()
                    .table_status(&DevId::Name("junk"), DmFlags::empty())
                    .is_err());
    }

    #[test]
    /// Table status on a non-existant name with TABLE_STATUS flag errors.
    fn sudo_test_table_status_non_existant_table() {
        assert!(DM::new()
                    .unwrap()
                    .table_status(&DevId::Name("junk"), DM_STATUS_TABLE)
                    .is_err());
    }

    #[test]
    /// The table should have an entry for a newly created device.
    /// The device has no segments, so the second part of the info should
    /// be empty.
    fn sudo_test_table_status() {
        let dm = DM::new().unwrap();
        let name = "example-dev";
        dm.device_create(name, None, DmFlags::empty()).unwrap();

        let status;
        loop {
            if let Ok(info) = dm.table_status(&DevId::Name(name), DmFlags::empty()) {
                status = info;
                break;
            }
        }

        assert!(status.1.is_empty());
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Verify that getting the status of a non-existant device specified
    /// by name returns an error.
    fn sudo_status_no_name() {
        assert!(DM::new()
                    .unwrap()
                    .device_status(&DevId::Name("example_dev"))
                    .is_err());
    }

    #[test]
    /// Verify that creating a device with the same name twice fails.
    fn sudo_test_double_creation() {
        let dm = DM::new().unwrap();
        let name = "example-dev";
        dm.device_create(name, None, DmFlags::empty()).unwrap();
        assert!(dm.device_create(name, None, DmFlags::empty()).is_err());
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }
}
