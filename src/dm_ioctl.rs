/* automatically generated by rust-bindgen */

pub type __s8 = ::libc::c_char;
pub type __u8 = ::libc::c_uchar;
pub type __s16 = ::libc::c_short;
pub type __u16 = ::libc::c_ushort;
pub type __s32 = ::libc::c_int;
pub type __u32 = ::libc::c_uint;
pub type __s64 = ::libc::c_longlong;
pub type __u64 = ::libc::c_ulonglong;
#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct Struct_Unnamed1 {
    pub fds_bits: [::libc::c_ulong; 16usize],
}
impl ::std::default::Default for Struct_Unnamed1 {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
pub type __kernel_fd_set = Struct_Unnamed1;
pub type __kernel_sighandler_t = ::std::option::Option<extern "C" fn(arg1: ::libc::c_int) -> ()>;
pub type __kernel_key_t = ::libc::c_int;
pub type __kernel_mqd_t = ::libc::c_int;
pub type __kernel_old_uid_t = ::libc::c_ushort;
pub type __kernel_old_gid_t = ::libc::c_ushort;
pub type __kernel_old_dev_t = ::libc::c_ulong;
pub type __kernel_long_t = ::libc::c_long;
pub type __kernel_ulong_t = ::libc::c_ulong;
pub type __kernel_ino_t = __kernel_ulong_t;
pub type __kernel_mode_t = ::libc::c_uint;
pub type __kernel_pid_t = ::libc::c_int;
pub type __kernel_ipc_pid_t = ::libc::c_int;
pub type __kernel_uid_t = ::libc::c_uint;
pub type __kernel_gid_t = ::libc::c_uint;
pub type __kernel_suseconds_t = __kernel_long_t;
pub type __kernel_daddr_t = ::libc::c_int;
pub type __kernel_uid32_t = ::libc::c_uint;
pub type __kernel_gid32_t = ::libc::c_uint;
pub type __kernel_size_t = __kernel_ulong_t;
pub type __kernel_ssize_t = __kernel_long_t;
pub type __kernel_ptrdiff_t = __kernel_long_t;
#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct Struct_Unnamed2 {
    pub val: [::libc::c_int; 2usize],
}
impl ::std::default::Default for Struct_Unnamed2 {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
pub type __kernel_fsid_t = Struct_Unnamed2;
pub type __kernel_off_t = __kernel_long_t;
pub type __kernel_loff_t = ::libc::c_longlong;
pub type __kernel_time_t = __kernel_long_t;
pub type __kernel_clock_t = __kernel_long_t;
pub type __kernel_timer_t = ::libc::c_int;
pub type __kernel_clockid_t = ::libc::c_int;
pub type __kernel_caddr_t = *mut ::libc::c_char;
pub type __kernel_uid16_t = ::libc::c_ushort;
pub type __kernel_gid16_t = ::libc::c_ushort;
pub type __le16 = __u16;
pub type __be16 = __u16;
pub type __le32 = __u32;
pub type __be32 = __u32;
pub type __le64 = __u64;
pub type __be64 = __u64;
pub type __sum16 = __u16;
pub type __wsum = __u32;
#[repr(C)]
#[derive(Copy)]
pub struct Struct_dm_ioctl {
    pub version: [__u32; 3usize],
    pub data_size: __u32,
    pub data_start: __u32,
    pub target_count: __u32,
    pub open_count: __s32,
    pub flags: __u32,
    pub event_nr: __u32,
    pub padding: __u32,
    pub dev: __u64,
    pub name: [::libc::c_char; 128usize],
    pub uuid: [::libc::c_char; 129usize],
    pub data: [::libc::c_char; 7usize],
}
impl ::std::clone::Clone for Struct_dm_ioctl {
    fn clone(&self) -> Self {
        *self
    }
}
impl ::std::default::Default for Struct_dm_ioctl {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
#[repr(C)]
#[derive(Copy, Clone)]
pub struct Struct_dm_target_spec {
    pub sector_start: __u64,
    pub length: __u64,
    pub status: __s32,
    pub next: __u32,
    pub target_type: [::libc::c_char; 16usize],
}
impl ::std::default::Default for Struct_dm_target_spec {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct Struct_dm_target_deps {
    pub count: __u32,
    pub padding: __u32,
    pub dev: [__u64; 0usize],
}
impl ::std::default::Default for Struct_dm_target_deps {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct Struct_dm_name_list {
    pub dev: __u64,
    pub next: __u32,
    pub name: [::libc::c_char; 0usize],
}
impl ::std::default::Default for Struct_dm_name_list {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct Struct_dm_target_versions {
    pub next: __u32,
    pub version: [__u32; 3usize],
    pub name: [::libc::c_char; 0usize],
}
impl ::std::default::Default for Struct_dm_target_versions {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct Struct_dm_target_msg {
    pub sector: __u64,
    pub message: [::libc::c_char; 0usize],
}
impl ::std::default::Default for Struct_dm_target_msg {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
pub type Enum_Unnamed3 = ::libc::c_uint;
pub const DM_VERSION_CMD: ::libc::c_uint = 0;
pub const DM_REMOVE_ALL_CMD: ::libc::c_uint = 1;
pub const DM_LIST_DEVICES_CMD: ::libc::c_uint = 2;
pub const DM_DEV_CREATE_CMD: ::libc::c_uint = 3;
pub const DM_DEV_REMOVE_CMD: ::libc::c_uint = 4;
pub const DM_DEV_RENAME_CMD: ::libc::c_uint = 5;
pub const DM_DEV_SUSPEND_CMD: ::libc::c_uint = 6;
pub const DM_DEV_STATUS_CMD: ::libc::c_uint = 7;
pub const DM_DEV_WAIT_CMD: ::libc::c_uint = 8;
pub const DM_TABLE_LOAD_CMD: ::libc::c_uint = 9;
pub const DM_TABLE_CLEAR_CMD: ::libc::c_uint = 10;
pub const DM_TABLE_DEPS_CMD: ::libc::c_uint = 11;
pub const DM_TABLE_STATUS_CMD: ::libc::c_uint = 12;
pub const DM_LIST_VERSIONS_CMD: ::libc::c_uint = 13;
pub const DM_TARGET_MSG_CMD: ::libc::c_uint = 14;
pub const DM_DEV_SET_GEOMETRY_CMD: ::libc::c_uint = 15;
