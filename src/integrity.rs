// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::fs::OpenOptions;
use std::io::{BufWriter, Seek, SeekFrom, Write};
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;
use std::string::ToString;

use crate::consts::IEC;
use crate::core::{devnode_to_devno, DevId, Device, DeviceInfo, DmName, DmOptions, DmUuid, DM};
use crate::result::{DmError, DmResult, ErrorEnum};
use crate::shared::{
    device_create, device_exists, parse_device, parse_value, DmDevice, TargetLine, TargetParams,
    TargetTable, TargetTypeBuf,
};
use crate::units::{Sectors, SECTOR_SIZE};

const TARGET_NAME: &str = "integrity";

const SUPERBLOCK_SECTORS: Sectors = Sectors(8); // 4 KiB

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Mode {
    Direct,
    Journaled,
    Recovery,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            Mode::Direct => "D",
            Mode::Journaled => "J",
            Mode::Recovery => "R",
        };
        writeln!(f, "{}", val)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegrityTargetParams {
    pub base_dev: Device,
    pub reserved: Sectors,
    pub tag_size: Option<u32>,
    pub mode: Mode,
}

impl IntegrityTargetParams {
    pub fn new(
        base_dev: Device,
        reserved: Sectors,
        tag_size: Option<u32>,
        mode: Mode,
    ) -> IntegrityTargetParams {
        IntegrityTargetParams {
            base_dev,
            reserved,
            tag_size,
            mode,
        }
    }
}

impl fmt::Display for IntegrityTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", TARGET_NAME, self.param_str())
    }
}

impl FromStr for IntegrityTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<IntegrityTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();
        let len = vals.len();
        if len < 5 {
            let err_msg = format!(
                "expected 5+ values in params string \"{}\", found {}",
                s, len
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        if vals[0] != TARGET_NAME {
            let err_msg = format!(
                "Expected an integrity target entry but found target type {}",
                vals[0]
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let mode = match vals[4] {
            "D" => Mode::Direct,
            "J" => Mode::Journaled,
            "R" => Mode::Recovery,
            _ => {
                let err_msg = format!("Unknown mode type: {}", vals[4]);
                return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
            }
        };

        Ok(IntegrityTargetParams::new(
            parse_device(vals[1], "base dev")?,
            Sectors(parse_value(vals[2], "reserved sectors")?),
            parse_value(vals[3], "tag size").ok(),
            mode,
        ))
    }
}

impl TargetParams for IntegrityTargetParams {
    fn param_str(&self) -> String {
        format!(
            "{} {} {} {} {} {}",
            self.base_dev,
            *self.reserved,
            self.tag_size
                .map(|x| x.to_string())
                .unwrap_or_else(|| "-".into()),
            self.mode,
            1,
            "internal_hash:crc32",
        )
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(TARGET_NAME.into()).expect("TARGET_NAME is valid")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntegrityDevTargetTable {
    pub table: TargetLine<IntegrityTargetParams>,
}

impl IntegrityDevTargetTable {
    pub fn new(
        start: Sectors,
        length: Sectors,
        params: IntegrityTargetParams,
    ) -> IntegrityDevTargetTable {
        IntegrityDevTargetTable {
            table: TargetLine::new(start, length, params),
        }
    }
}

impl fmt::Display for IntegrityDevTargetTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let table = &self.table;
        writeln!(f, "{} {} {}", *table.start, *table.length, table.params)
    }
}

impl TargetTable for IntegrityDevTargetTable {
    fn from_raw_table(table: &[(u64, u64, String, String)]) -> DmResult<IntegrityDevTargetTable> {
        if table.len() != 1 {
            let err_msg = format!(
                "IntegrityDev table should have exactly one line, has {} lines",
                table.len()
            );
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }
        let line = table.first().expect("table.len() == 1");
        Ok(IntegrityDevTargetTable::new(
            Sectors(line.0),
            Sectors(line.1),
            format!("{} {}", line.2.to_string(), line.3).parse::<IntegrityTargetParams>()?,
        ))
    }

    fn to_raw_table(&self) -> Vec<(u64, u64, String, String)> {
        to_raw_table_unique!(self)
    }
}

/// DM construct for an integrity block device
#[derive(Debug)]
pub struct IntegrityDev {
    dev_info: Box<DeviceInfo>,
    table: IntegrityDevTargetTable,
}

impl IntegrityDev {
    /// Create a new IntegrityDev, and optionally zero it.
    /// Reading unwritten sectors will fail; either choose to have this method
    /// zero all (this will take a long time), or the caller needs to write to
    /// the entire device before actual use. Somebody (udev?) tries to read
    /// new blockdevs immediately, so expect status.mismatches to be nonzero
    /// for new devices.
    pub fn initialize(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        devnode: &Path,
        reserved: Sectors,
        format: bool,
    ) -> DmResult<IntegrityDev> {
        IntegrityDev::version_check(dm)?;
        if device_exists(dm, name)? {
            let err_msg = format!("integritydev {} already exists", name);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let base_dev: Device = devnode_to_devno(devnode)?
            .ok_or_else(|| {
                DmError::Dm(
                    ErrorEnum::Invalid,
                    format!(
                        "failed to parse device number from \"{}\"",
                        devnode.display()
                    ),
                )
            })?
            .into();

        // Overwrite the superblock with zeroes
        wipe_sectors(devnode, Sectors(0), SUPERBLOCK_SECTORS)?;

        // Load the dm-integrity target with one-sector size, the kernel
        // driver will write the superblock and allocate space for tags
        let table = IntegrityDev::gen_default_table(base_dev, Sectors(1), reserved);
        let dev_info = device_create(dm, name, uuid, &table, &DmOptions::new())?;

        let mut idev = IntegrityDev {
            dev_info: Box::new(dev_info),
            table,
        };

        // Read status to get provided sectors
        let provided_sectors = idev.status(dm)?.provided_sectors;

        // Load the dm-integrity target with the the target size
        // "provided_data_sectors"
        let table = IntegrityDev::gen_default_table(base_dev, provided_sectors, reserved);
        idev.table_load(dm, &table)?;
        idev.resume(dm)?;
        idev.table = table;

        // All integrity blocks must be written, or reads to uninitialized
        // sectors will return an integrity error. This will take a long time,
        // so maybe the caller wants to do it themselves.
        if format {
            wipe_sectors(idev.devnode(), Sectors(0), provided_sectors)?;
        }

        Ok(idev)
    }

    /// Set up a previously-initialized integrity device.
    pub fn setup(
        dm: &DM,
        name: &DmName,
        uuid: Option<&DmUuid>,
        devnode: &Path,
        reserved: Sectors,
        size: Sectors,
    ) -> DmResult<IntegrityDev> {
        IntegrityDev::version_check(dm)?;
        if device_exists(dm, name)? {
            let err_msg = format!("integritydev {} already exists", name);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let base_dev: Device = devnode_to_devno(devnode)?
            .ok_or_else(|| {
                DmError::Dm(
                    ErrorEnum::Invalid,
                    format!(
                        "failed to parse device number from \"{}\"",
                        devnode.display()
                    ),
                )
            })?
            .into();

        let table = IntegrityDev::gen_default_table(base_dev, size, reserved);
        let dev_info = device_create(dm, name, uuid, &table, &DmOptions::new())?;

        Ok(IntegrityDev {
            dev_info: Box::new(dev_info),
            table,
        })
    }

    fn version_check(dm: &DM) -> DmResult<()> {
        let version = dm.list_versions()?;
        match version.into_iter().find(|x| x.0 == TARGET_NAME) {
            None => Err(DmError::Dm(
                ErrorEnum::Invalid,
                "dm-integrity not supported".into(),
            )),
            Some((_name, maj, min, extra)) => {
                if !(maj == 1 && min >= 2) {
                    Err(DmError::Dm(
                        ErrorEnum::Invalid,
                        format!(
                            "integrity version not 1.2.x or later: {}.{}.{}",
                            maj, min, extra
                        ),
                    ))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn gen_default_table(
        base_dev: Device,
        size: Sectors,
        reserved: Sectors,
    ) -> IntegrityDevTargetTable {
        IntegrityDevTargetTable::new(
            Sectors::default(),
            size,
            IntegrityTargetParams::new(base_dev, reserved, None, Mode::Journaled),
        )
    }

    /// Get the current status of the integrity device.
    pub fn status(&self, dm: &DM) -> DmResult<IntegrityDevStatus> {
        let (_, status) = dm.table_status(&DevId::Name(self.name()), &DmOptions::new())?;

        if status.len() != 1 {
            return Err(DmError::Dm(
                ErrorEnum::Invalid,
                "Kernel must return 1 line from integrity dev status".into(),
            ));
        }

        let status_line = &status.first().expect("assertion above holds").3;
        let status_vals = status_line.split(' ').collect::<Vec<_>>();

        // With dm-integrity 1.2+ this should never happen
        assert!(
            status_vals.len() < 3,
            "Status returned less that 3 values. Kernels before 4.19 not supported"
        );

        let mismatches = parse_value(status_vals[0], "mismatches")?;
        let provided_sectors = Sectors(parse_value(status_vals[1], "provided sectors")?);

        let recalc_sector = {
            match status_vals[2] {
                "-" => None,
                x => Some(Sectors(parse_value(x, "recalc sector")?)),
            }
        };

        Ok(IntegrityDevStatus {
            mismatches,
            provided_sectors,
            recalc_sector,
        })
    }
}

impl DmDevice<IntegrityDevTargetTable> for IntegrityDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    // This method is incomplete. It is expected that it will be refined so
    // that it will return true in more cases, i.e., to be less stringent.
    fn equivalent_tables(
        left: &IntegrityDevTargetTable,
        right: &IntegrityDevTargetTable,
    ) -> DmResult<bool> {
        Ok(left == right)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.table.table.length
    }

    fn table(&self) -> &IntegrityDevTargetTable {
        table!(self)
    }

    fn teardown(&mut self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), &DmOptions::new())?;
        Ok(())
    }

    fn uuid(&self) -> Option<&DmUuid> {
        uuid!(self)
    }
}

#[derive(Clone, Debug)]
/// Integrity device status.
pub struct IntegrityDevStatus {
    mismatches: u64,
    provided_sectors: Sectors,
    recalc_sector: Option<Sectors>,
}

/// Write buf at offset length times.
fn write_sectors<P: AsRef<Path>>(
    path: P,
    offset: Sectors,
    length: Sectors,
    buf: &[u8; SECTOR_SIZE],
) -> DmResult<()> {
    let mut f =
        BufWriter::with_capacity(IEC::Mi as usize, OpenOptions::new().write(true).open(path)?);

    f.seek(SeekFrom::Start(*offset.bytes()))?;
    for _ in 0..*length {
        f.write_all(buf)?;
    }

    f.flush()?;
    f.get_mut().sync_all()?;
    Ok(())
}

/// Zero sectors at the given offset for length sectors.
fn wipe_sectors<P: AsRef<Path>>(path: P, offset: Sectors, length: Sectors) -> DmResult<()> {
    write_sectors(path, offset, length, &[0u8; SECTOR_SIZE])
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::loopbacked::{blkdev_size, test_with_spec};

    use crate::test_lib::{test_name, xfs_create_fs};

    use super::*;

    fn test_integrity_dev(paths: &[&Path]) {
        assert!(!paths.is_empty());
        let dm = DM::new().unwrap();

        let base_dev_size =
            blkdev_size(&OpenOptions::new().read(true).open(&paths[0]).unwrap()).sectors();

        let int_dev_name = test_name("integrity").expect("valid format");

        let mut int_dev =
            IntegrityDev::initialize(&dm, &int_dev_name, None, &paths[0], Sectors(0), true)
                .unwrap();
        let size = int_dev.size();

        int_dev.teardown(&dm).unwrap();

        let int_dev =
            IntegrityDev::setup(&dm, &int_dev_name, None, &paths[0], Sectors(0), size).unwrap();

        xfs_create_fs(&int_dev.devnode()).unwrap();

        // TODO: mount and write some stuff to the fs and make sure no errors
        // happen

        let status = int_dev.status(&dm).unwrap();
        assert_eq!(status.mismatches, 0);
        assert!(status.provided_sectors < base_dev_size);
        assert_eq!(status.recalc_sector, None);
    }

    #[test]
    fn loop_test_integrity_dev() {
        test_with_spec(1, test_integrity_dev);
    }
}
