// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::path::PathBuf;

use mnt::get_submounts;
use nix::mount::{MntFlags, umount2};

use super::DM;
use super::dm_flags::DmFlags;
use super::result::DmResult;
use super::types::{DevId, DmNameBuf, DmUuidBuf};

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

use self::cleanup_errors::{Error, Result};

/// String that is to be concatenated with test supplied name, so that we can easily identify and
/// remove.
static DM_TEST_ID: &str = "_dm-rs_test_delme";

/// Generate the test path string given the test supplied name.
pub fn test_path(name: &str) -> String {
    let mut namestr = String::from(name);
    namestr.push_str(DM_TEST_ID);
    namestr
}

/// Generate the test name given the test supplied name.
pub fn test_name(name: &str) -> DmResult<DmNameBuf> {
    let mut namestr = String::from(name);
    namestr.push_str(DM_TEST_ID);
    DmNameBuf::new(namestr)
}

/// Generate the test uuid given the test supplied name.
pub fn test_uuid(name: &str) -> DmResult<DmUuidBuf> {
    let mut namestr = String::from(name);
    namestr.push_str(DM_TEST_ID);
    DmUuidBuf::new(namestr)
}

/// Attempt to remove all device mapper devices which have DM_TEST_ID contained in name.
/// FIXME: Current implementation complicated by https://bugzilla.redhat.com/show_bug.cgi?id=1506287
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
pub fn clean_up() -> Result<()> {
    dm_test_fs_unmount()?;
    dm_test_devices_remove()?;
    Ok(())
}
