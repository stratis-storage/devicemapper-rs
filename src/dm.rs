// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::borrow::Borrow;
use std::fmt;
use std::fs::File;
use std::io;
use std::ops::Deref;
use std::os::unix::io::AsRawFd;
use std::mem::{size_of, transmute};
use std::slice;
use std::cmp;
use std::thread;
use std::time::Duration;

use nix::libc::ioctl as nix_ioctl;
use nix::libc::c_ulong;

use super::device::Device;
use super::deviceinfo::{DM_NAME_LEN, DM_UUID_LEN, DeviceInfo};
use super::dm_ioctl as dmi;
use super::errors::Result;
use super::errors::dm::{Error, ErrorKind};
use super::types::{Sectors, TargetLine, TargetLineArg};
use super::util::{align_to, slice_to_null};

/// Indicator to send IOCTL to DM
const DM_IOCTL: u8 = 0xfd;
/// Control path for user space to pass IOCTL to kernel DM
const DM_CTL_PATH: &'static str = "/dev/mapper/control";
/// Major version
const DM_VERSION_MAJOR: u32 = 4;
/// Minor version
const DM_VERSION_MINOR: u32 = 30;
/// Patch level
const DM_VERSION_PATCHLEVEL: u32 = 0;

/// Start with a large buffer to make BUFFER_FULL rare. Libdm does this too.
const MIN_BUF_SIZE: usize = 16 * 1024;

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

/// Returns InvalidArgument if value is unsuitable.
fn dev_id_check(value: &str, max_allowed_chars: usize) -> Result<()> {
    if !value.is_ascii() {
        let err_msg = format!("value {} has some non-ascii characters", value);
        return Err(Error::from_kind(ErrorKind::InvalidArgument(err_msg.into())).into());
    }
    let num_chars = value.len();
    if num_chars == 0 {
        return Err(Error::from_kind(ErrorKind::InvalidArgument("value has zero characters"
                                                                   .into()))
                           .into());
    }
    if num_chars > max_allowed_chars {
        let err_msg = format!("value {} has {} chars which is greater than maximum allowed {}",
                              value,
                              num_chars,
                              max_allowed_chars);
        return Err(Error::from_kind(ErrorKind::InvalidArgument(err_msg.into())).into());
    }
    Ok(())
}

/// Define borrowed and owned versions of string types that guarantee
/// conformance to DM restrictions, such as maximum length.
// This implementation follows the example of Path/PathBuf as closely as
// possible.
macro_rules! dev_id {
    ($B: ident, $O: ident, $MAX: ident) => {
        /// The borrowed version of the DM identifier.
        #[derive(Debug, PartialEq, Eq)]
        pub struct $B {
            inner: str,
        }

        /// The owned version of the DM identifier.
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $O {
            inner: String,
        }

        impl $B {
            /// Create a new borrowed identifier from a `&str`.
            /// Returns InvalidArgument if value is not valid.
            pub fn new(value: &str) -> Result<&$B> {
                dev_id_check(value, $MAX - 1)?;
                Ok(unsafe { transmute(value) })
            }
        }

        impl ToOwned for $B {
            type Owned = $O;
            fn to_owned(&self) -> $O {
                $O { inner: self.inner.to_owned() }
            }
        }

        impl AsRef<str> for $B {
            fn as_ref(&self) -> &str {
                &self.inner
            }
        }

        impl Deref for $B {
            type Target = str;
            fn deref(&self) -> &str {
                &self.inner
            }
        }

        impl fmt::Display for $B {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", &self.inner)
            }
        }

        impl $O {
            /// Construct a new owned identifier.
            /// Returns InvalidArgument if value is not valid.
            pub fn new(value: String) -> Result<$O> {
                dev_id_check(&value, $MAX - 1)?;
                Ok($O { inner: value })
            }
        }

        impl AsRef<$B> for $O {
            fn as_ref(&self) -> &$B {
                self
            }
        }

        impl Borrow<$B> for $O {
            fn borrow(&self) -> &$B {
                self.deref()
            }
        }

        impl Deref for $O {
            type Target = $B;
            fn deref(&self) -> &$B {
                $B::new(&self.inner).expect("inner satisfies all correctness criteria for $B::new")
            }
        }
    }
}

/// A devicemapper name. Really just a string, but also the argument type of
/// DevId::Name. Used in function arguments to indicate that the function
/// takes only a name, not a devicemapper uuid.
dev_id!(DmName, DmNameBuf, DM_NAME_LEN);

/// A devicemapper uuid. A devicemapper uuid has a devicemapper-specific
/// format.
dev_id!(DmUuid, DmUuidBuf, DM_UUID_LEN);

/// Used as a parameter for functions that take either a Device name
/// or a Device UUID.
#[derive(Debug, PartialEq, Eq)]
pub enum DevId<'a> {
    /// The parameter is the device's name
    Name(&'a DmName),
    /// The parameter is the device's devicemapper uuid
    Uuid(&'a DmUuid),
}

impl<'a> fmt::Display for DevId<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DevId::Name(name) => write!(f, "{}", name),
            DevId::Uuid(uuid) => write!(f, "{}", uuid),
        }
    }
}

/// Context needed for communicating with devicemapper.
pub struct DM {
    file: File,
}

impl DM {
    /// Create a new context for communicating with DM.
    /// Returns ContextInitError if context can not be initialized.
    pub fn new() -> Result<DM> {
        Ok(DM {
               file: File::open(DM_CTL_PATH)
                   .map_err(|e| Error::with_chain(e, ErrorKind::ContextInitError))?,
           })
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

    fn hdr_set_name(hdr: &mut dmi::Struct_dm_ioctl, name: &DmName) -> () {
        let name_dest: &mut [u8; DM_NAME_LEN] = unsafe { transmute(&mut hdr.name) };
        let bytes = name.as_bytes();
        name_dest[..bytes.len()].clone_from_slice(bytes);
    }

    fn hdr_set_uuid(hdr: &mut dmi::Struct_dm_ioctl, uuid: &DmUuid) -> () {
        let uuid_dest: &mut [u8; DM_UUID_LEN] = unsafe { transmute(&mut hdr.uuid) };
        let bytes = uuid.as_bytes();
        uuid_dest[..bytes.len()].clone_from_slice(bytes);
    }

    // Give this a filled-in header and optionally add'l stuff.
    // Does the ioctl and maybe returns stuff. Handles BUFFER_FULL flag.
    // Returns IoctlError on failure.
    fn do_ioctl(&self,
                ioctl: u8,
                hdr: &mut dmi::Struct_dm_ioctl,
                in_data: Option<&[u8]>)
                -> Result<Vec<u8>> {
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
                return Err(Error::with_chain(io::Error::last_os_error(), ErrorKind::IoctlError)
                               .into());
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
    pub fn version(&self) -> Result<(u32, u32, u32)> {
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
    pub fn remove_all(&self, flags: DmFlags) -> Result<()> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_DEFERRED_REMOVE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);

        self.do_ioctl(dmi::DM_REMOVE_ALL_CMD as u8, &mut hdr, None)?;

        Ok(())
    }

    /// Returns a list of tuples containing DM device names and a
    /// Device, which holds their major and minor device numbers.
    pub fn list_devices(&self) -> Result<Vec<(DmNameBuf, Device)>> {
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
                devs.push((DmNameBuf::new(dm_name).expect("name obtained from kernel"),
                           device.dev.into()));

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
    /// use devicemapper::{DM, DmFlags, DmName};
    ///
    /// let dm = DM::new().unwrap();
    ///
    /// // Setting a uuid is optional
    /// let name = DmName::new("example-dev").expect("is valid DM name");
    /// let dev = dm.device_create(name, None, DmFlags::empty()).unwrap();
    /// ```
    pub fn device_create(&self,
                         name: &DmName,
                         uuid: Option<&DmUuid>,
                         flags: DmFlags)
                         -> Result<DeviceInfo> {
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
    pub fn device_remove(&self, id: &DevId, flags: DmFlags) -> Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_DEFERRED_REMOVE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *id {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        self.do_ioctl(dmi::DM_DEV_REMOVE_CMD as u8, &mut hdr, None)?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Change a DM device's name OR set the device's uuid for the first time.
    ///
    /// Prerequisite: if new == DevId::Name(new_name), old_name != new_name
    /// Prerequisite: if new == DevId::Uuid(uuid), device's current uuid
    /// must be "".
    /// Note: Possibly surprisingly, returned DeviceInfo's uuid or name field
    /// contains the previous value, not the newly set value.
    pub fn device_rename(&self, old_name: &DmName, new: &DevId) -> Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();
        let mut data_in = match *new {
            DevId::Name(name) => {
                Self::initialize_hdr(&mut hdr, DmFlags::empty());
                name.as_bytes().to_vec()
            }
            DevId::Uuid(uuid) => {
                Self::initialize_hdr(&mut hdr, DM_UUID);
                uuid.as_bytes().to_vec()
            }
        };
        data_in.push(b'\0');

        Self::hdr_set_name(&mut hdr, old_name);

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
    /// use devicemapper::{DM, DM_SUSPEND, DevId, DmFlags, DmName};

    /// let dm = DM::new().unwrap();
    ///
    /// let name = DmName::new("example-dev").expect("is valid DM name");
    /// let id = DevId::Name(name);
    /// dm.device_suspend(&id, DM_SUSPEND).unwrap();
    /// ```
    pub fn device_suspend(&self, id: &DevId, flags: DmFlags) -> Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = (DM_SUSPEND | DM_NOFLUSH | DM_SKIP_LOCKFS) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *id {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        self.do_ioctl(dmi::DM_DEV_SUSPEND_CMD as u8, &mut hdr, None)?;

        Ok(DeviceInfo::new(hdr))
    }

    /// Get DeviceInfo for a device. This is also returned by other
    /// methods, but if just the DeviceInfo is desired then this just
    /// gets it.
    pub fn device_status(&self, id: &DevId) -> Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match *id {
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
    pub fn device_wait(&self, id: &DevId, flags: DmFlags) -> Result<(DeviceInfo, Vec<TargetLine>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_QUERY_INACTIVE_TABLE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *id {
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
    /// use devicemapper::{DM, DevId, DmName, Sectors};
    /// let dm = DM::new().unwrap();
    ///
    /// // Create a 16MiB device (32768 512-byte sectors) that maps to /dev/sdb1
    /// // starting 1MiB into sdb1
    /// let table = vec![(Sectors(0),
    ///                   Sectors(32768),
    ///                   "linear",
    ///                   "/dev/sdb1 2048")];
    ///
    /// let name = DmName::new("example-dev").expect("is valid DM name");
    /// let id = DevId::Name(name);
    /// dm.table_load(&id, &table).unwrap();
    /// ```
    pub fn table_load<T1, T2>(&self,
                              id: &DevId,
                              targets: &[TargetLineArg<T1, T2>])
                              -> Result<DeviceInfo>
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
                let err_msg = "target type too long";
                return Err(Error::from_kind(ErrorKind::InvalidArgument(err_msg.into())).into());
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
        match *id {
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
    pub fn table_clear(&self, id: &DevId) -> Result<DeviceInfo> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match *id {
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
    pub fn table_deps(&self, id: &DevId, flags: DmFlags) -> Result<Vec<Device>> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = DM_QUERY_INACTIVE_TABLE & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *id {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

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
    // Justification: If the ioctl succeeded, the data is correct and
    // complete. An error in parsing can only result from a change in the
    // kernel. We rely on DM's interface versioning system. Kernel changes
    // will either be backwards-compatible, or will increment
    // DM_VERSION_MAJOR.  Since calls made with a non-matching major version
    // will fail, this protects against callers parsing unknown formats.
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
    /// use devicemapper::{DM, DM_STATUS_TABLE, DevId, DmFlags, DmName};
    /// let dm = DM::new().unwrap();
    ///
    /// let name = DmName::new("example-dev").expect("is valid DM name");
    /// let id = DevId::Name(name);
    /// let res = dm.table_status(&id, DM_STATUS_TABLE).unwrap();
    /// println!("{} {:?}", res.0.name(), res.1);
    /// ```
    pub fn table_status(&self,
                        id: &DevId,
                        flags: DmFlags)
                        -> Result<(DeviceInfo, Vec<TargetLine>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        let clean_flags = (DM_NOFLUSH | DM_STATUS_TABLE | DM_QUERY_INACTIVE_TABLE) & flags;

        Self::initialize_hdr(&mut hdr, clean_flags);
        match *id {
            DevId::Name(name) => Self::hdr_set_name(&mut hdr, name),
            DevId::Uuid(uuid) => Self::hdr_set_uuid(&mut hdr, uuid),
        };

        let data_out = self.do_ioctl(dmi::DM_TABLE_STATUS_CMD as u8, &mut hdr, None)?;

        let status = DM::parse_table_status(hdr.target_count, &data_out);

        Ok((DeviceInfo::new(hdr), status))
    }

    /// Returns a list of each loaded target type with its name, and
    /// version broken into major, minor, and patchlevel.
    pub fn list_versions(&self) -> Result<Vec<(String, u32, u32, u32)>> {
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
                      id: &DevId,
                      sector: Sectors,
                      msg: &str)
                      -> Result<(DeviceInfo, Option<String>)> {
        let mut hdr: dmi::Struct_dm_ioctl = Default::default();

        // No flags checked so don't pass any
        Self::initialize_hdr(&mut hdr, DmFlags::empty());
        match *id {
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
}

#[cfg(test)]
mod tests {

    use super::super::errors;

    use super::*;

    #[test]
    /// Verify that creating an empty DmName is an error.
    pub fn test_empty_name() {
        assert!(match *DmName::new("").unwrap_err().kind() {
                    errors::ErrorKind::Dm(ErrorKind::InvalidArgument(_)) => true,
                    _ => false,
                })
    }

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
        let name = DmName::new("example-dev").expect("is valid DM name");
        dm.device_create(name, None, DmFlags::empty()).unwrap();

        let devices;
        loop {
            if let Ok(list) = dm.list_devices() {
                devices = list;
                break;
            }
        }

        assert_eq!(devices.len(), 1);
        assert_eq!(devices[0].0.as_ref(), name);
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Test that device creation gives a device with the expected name.
    fn sudo_test_create() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        let result = dm.device_create(name, None, DmFlags::empty()).unwrap();
        assert!(result.name() == name);
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Verify that creation with a UUID results in correct name and UUID.
    fn sudo_test_create_uuid() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        let uuid = DmUuid::new("example-363333333333333").expect("is valid DM uuid");
        let result = dm.device_create(name, Some(uuid), DmFlags::empty())
            .unwrap();
        assert_eq!(result.name(), name);
        assert_eq!(result.uuid().unwrap(), uuid);
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Verify that resetting uuid fails.
    fn sudo_test_rename_uuid() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        let uuid = DmUuid::new("example-363333333333333").expect("is valid DM uuid");
        dm.device_create(name, Some(uuid), DmFlags::empty())
            .unwrap();

        let new_uuid = DmUuid::new("example-9999999999").expect("is valid DM uuid");
        assert!(match *dm.device_rename(name, &DevId::Uuid(new_uuid))
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });

        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Verify that resetting uuid to same uuid fails.
    fn sudo_test_rename_uuid_id() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        let uuid = DmUuid::new("example-363333333333333").expect("is valid DM uuid");
        dm.device_create(name, Some(uuid), DmFlags::empty())
            .unwrap();
        assert!(match *dm.device_rename(name, &DevId::Uuid(uuid))
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Verify that setting a new uuid succeeds.
    /// Note that the uuid is not set in the returned dev_info.
    fn sudo_test_set_uuid() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        dm.device_create(name, None, DmFlags::empty()).unwrap();

        let uuid = DmUuid::new("example-363333333333333").expect("is valid DM uuid");
        let result = dm.device_rename(name, &DevId::Uuid(uuid)).unwrap();
        assert_eq!(result.uuid(), None);
        assert_eq!(dm.device_status(&DevId::Name(name))
                       .unwrap()
                       .uuid()
                       .unwrap(),
                   uuid);
        assert!(dm.device_status(&DevId::Uuid(uuid)).is_ok());
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Test that device rename to same name fails.
    /// This is unfortunate, but appears to be true.
    fn sudo_test_rename_id() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        dm.device_create(name, None, DmFlags::empty()).unwrap();
        DM::wait_for_dm();
        assert!(match *dm.device_rename(name, &DevId::Name(name))
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Test that device rename to different name works.
    /// Verify that the only device in the list of devices is a device with
    /// the new name.
    fn sudo_test_rename() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        dm.device_create(name, None, DmFlags::empty()).unwrap();

        let new_name = DmName::new("example-dev-2").expect("is valid DM name");
        loop {
            if dm.device_rename(name, &DevId::Name(new_name)).is_ok() {
                break;
            }
        }

        assert!(match *dm.device_status(&DevId::Name(name)).unwrap_err().kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
        assert!(dm.device_status(&DevId::Name(new_name)).is_ok());

        let devices = dm.list_devices().unwrap();
        assert_eq!(devices.len(), 1);
        assert_eq!(devices[0].0.as_ref(), new_name);

        let third_name = DmName::new("example-dev-3").expect("is valid DM name");
        dm.device_create(third_name, None, DmFlags::empty())
            .unwrap();
        assert!(match *dm.device_rename(new_name, &DevId::Name(third_name))
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });

        dm.device_remove(&DevId::Name(third_name), DmFlags::empty())
            .unwrap();
        dm.device_remove(&DevId::Name(new_name), DmFlags::empty())
            .unwrap();
    }

    #[test]
    /// Renaming a device that does not exist yields an error.
    fn sudo_test_rename_non_existant() {
        assert!(match *DM::new()
                .unwrap()
                .device_rename(DmName::new("old_name").expect("is valid Dm name"),
                               &DevId::Name(DmName::new("new_name").expect("is valid DM name")))
                          .unwrap_err()
                          .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
    }

    #[test]
    /// Removing a device that does not exist yields an error, unfortunately.
    fn sudo_test_remove_non_existant() {
        assert!(match *DM::new()
                .unwrap()
                .device_remove(&DevId::Name(DmName::new("junk").expect("is valid Dm name")),
                DmFlags::empty())
                          .unwrap_err()
                          .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
    }

    #[test]
    /// A newly created device has no deps.
    fn sudo_test_empty_deps() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        dm.device_create(name, None, DmFlags::empty()).unwrap();

        let deps;
        loop {
            if let Ok(list) = dm.table_deps(&DevId::Name(name), DmFlags::empty()) {
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
        assert!(match *DM::new()
                .unwrap()
                .table_status(&DevId::Name(DmName::new("junk").expect("is valid Dm name")),
                DmFlags::empty())
                          .unwrap_err()
                          .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
    }

    #[test]
    /// Table status on a non-existant name with TABLE_STATUS flag errors.
    fn sudo_test_table_status_non_existant_table() {
        let name = DmName::new("junk").expect("is valid DM name");
        assert!(match *DM::new()
                           .unwrap()
                           .table_status(&DevId::Name(name), DM_STATUS_TABLE)
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
    }

    #[test]
    /// The table should have an entry for a newly created device.
    /// The device has no segments, so the second part of the info should
    /// be empty.
    fn sudo_test_table_status() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
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
        let name = DmName::new("example_dev").expect("is valid DM name");
        assert!(match *DM::new()
                           .unwrap()
                           .device_status(&DevId::Name(name))
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
    }

    #[test]
    /// Verify that creating a device with the same name twice fails.
    /// Verify that creating a device with the same uuid twice fails.
    fn sudo_test_double_creation() {
        let dm = DM::new().unwrap();
        let name = DmName::new("example-dev").expect("is valid DM name");
        let uuid = DmUuid::new("uuid").expect("is valid DM UUID");

        let name_alt = DmName::new("name-alt").expect("is valid DM name");
        let uuid_alt = DmUuid::new("uuid-alt").expect("is valid DM UUID");

        dm.device_create(name, Some(uuid), DmFlags::empty())
            .unwrap();
        assert!(match *dm.device_create(name, Some(uuid), DmFlags::empty())
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
        assert!(match *dm.device_create(name, None, DmFlags::empty())
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
        assert!(match *dm.device_create(name, Some(uuid_alt), DmFlags::empty())
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
        assert!(match *dm.device_create(name_alt, Some(uuid), DmFlags::empty())
                           .unwrap_err()
                           .kind() {
                    errors::ErrorKind::Dm(ErrorKind::IoctlError) => true,
                    _ => false,
                });
        dm.device_remove(&DevId::Name(name), DmFlags::empty())
            .unwrap();
    }
}
