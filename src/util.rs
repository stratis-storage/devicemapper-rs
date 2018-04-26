// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// The smallest number divisible by `align_to` and at least `num`.
/// Precondition: `align_to` is a power of 2.
/// Precondition: `num` + `align_to` < usize::MAX + 1.
pub fn align_to(num: usize, align_to: usize) -> usize {
    let agn = align_to - 1;

    (num + agn) & !agn
}

/// Return slc up to the first \0, or None
pub fn slice_to_null(slc: &[u8]) -> Option<&[u8]> {
    slc.iter().position(|c| *c == b'\0').map(|i| &slc[..i])
}

#[cfg(test)]
use std::path::PathBuf;
#[cfg(test)]
use std::process::Command;

#[cfg(test)]
use mnt::get_submounts;
#[cfg(test)]
use nix::mount::{MntFlags, umount2};


#[cfg(test)]
use super::DM;
#[cfg(test)]
use super::dm_flags::DmFlags;
#[cfg(test)]
use super::result::{DmError, DmResult, ErrorEnum};
#[cfg(test)]
use super::types::{DevId, DmNameBuf, DmUuidBuf};

#[cfg(test)]
mod cleanup_errors {
    use mnt;
    use nix;

    error_chain!{
        foreign_links {
            Mnt(mnt::ParseError);
            Nix(nix::Error);
        }
    }
}

#[cfg(test)]
use self::cleanup_errors::{Error, Result};

#[cfg(test)]
static DM_TEST_ID: &str = "_dm-rs_ut_delme";

#[cfg(test)]
pub fn test_name(name: &str) -> DmResult<DmNameBuf> {
    let mut namestr = String::from(name);
    namestr.push_str(DM_TEST_ID);
    DmNameBuf::new(namestr)
}

#[cfg(test)]
pub fn test_uuid(name: &str) -> DmResult<DmUuidBuf> {
    let mut namestr = String::from(name);
    namestr.push_str(DM_TEST_ID);
    DmUuidBuf::new(namestr)
}

/// Common function to call a command line utility, returning a Result with an error message which
/// also includes stdout & stderr if it fails.
#[cfg(test)]
pub fn execute_cmd(cmd: &mut Command, error_msg: &str) -> DmResult<()> {
    let result = cmd.output().unwrap();
    if result.status.success() {
        Ok(())
    } else {
        let std_out_txt = String::from_utf8_lossy(&result.stdout);
        let std_err_txt = String::from_utf8_lossy(&result.stderr);
        let err_msg = format!("{} stdout: {} stderr: {}",
                              error_msg,
                              std_out_txt,
                              std_err_txt);
        Err(DmError::Dm(ErrorEnum::Error, err_msg))
    }
}

/// Attempt to remove all device mapper devices which have DM_TEST_ID contained in name.
/// FIXME: Current implementation complicated by https://bugzilla.redhat.com/show_bug.cgi?id=1506287
#[cfg(test)]
fn dm_test_devices_remove() -> Result<()> {

    let dm = DM::new().unwrap();

    /// One iteration of removing devicemapper devices
    fn one_iteration(dm: &DM) -> Result<(bool, Vec<String>)> {
        let mut progress_made = false;
        let mut remain = Vec::new();

        for d in dm.list_devices()
                .unwrap()
                .iter()
                .filter(|d| format!("{}", d.0.as_ref()).contains(DM_TEST_ID)) {

            match dm.device_remove(&DevId::Name(&d.0), DmFlags::empty()) {
                Ok(_) => progress_made = true,
                Err(_) => remain.push(format!("{}", d.0.as_ref())),
            }
        }
        Ok((progress_made, remain))
    }

    loop {
        let (progress_made, remain) = one_iteration(&dm)
            .map_err(|e| {
                         Error::with_chain(e,
                                           "Error while attempting to remove device mapper devices")
                     })?;

        if !progress_made {
            if remain.len() != 0 {
                bail!("We were unable to remove all stratis device mapper devices {:?}",
                      remain);
            }
            break;
        }
    }

    Ok(())
}

/// Try and un-mount any filesystems that contain DM_TEST_ID in the mount point, returning
/// immediately on the first one we are unable to unmount.
#[cfg(test)]
fn dm_test_fs_unmount() -> Result<()> {
    || -> Result<()> {
        let mounts = get_submounts(&PathBuf::from("/"))?;
        for m in mounts
                .iter()
                .filter(|m| {
                            m.file
                                .to_str()
                                .map_or(false, |s| s.contains(DM_TEST_ID))
                        }) {
            umount2(&m.file, MntFlags::MNT_DETACH)?;
        }
        Ok(())
    }()
            .map_err(|e| {
                         Error::with_chain(e, "unable to unmount all devicemapper test filesystems")
                     })
}

/// When a unit test panics we can leave the system in an inconsistent state.  This function
/// tries to clean up by un-mounting any mounted file systems, device mapper devices which contain
/// DM_TEST_ID in the path or name.
#[cfg(test)]
pub fn clean_up() -> Result<()> {
    dm_test_fs_unmount()?;
    dm_test_devices_remove()?;
    Ok(())
}
