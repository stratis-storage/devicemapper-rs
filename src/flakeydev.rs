// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::hash_set::HashSet;
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::lineardev::LinearDev;
use super::result::{DmResult, DmError, ErrorEnum};
use super::shared::{DmDevice, device_create, device_exists, parse_device, TargetLine, TargetParams};
use super::types::{DevId, DmName, DmUuid, Sectors, TargetTypeBuf};


#[derive(Debug, Eq, PartialEq)]
pub struct FlakeyDevTargetParams {
    linear_dev: Device,
    start_offset: Sectors,
    up_interval: u64,
    down_interval: u64,
    pub feature_args: HashSet<String>,
}

impl FlakeyDevTargetParams {
    pub fn new(device: Device,
               start_offset: Sectors,
               up_interval: u64,
               down_interval: u64,
               feature_args: Vec<String>)
               -> FlakeyDevTargetParams {
        FlakeyDevTargetParams {
            linear_dev: device,
            start_offset: start_offset,
            up_interval: up_interval,
            down_interval: down_interval,
            feature_args: feature_args.into_iter().collect::<HashSet<_>>(),
        }
    }
}

impl fmt::Display for FlakeyDevTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let feature_args = if self.feature_args.is_empty() {
            "0".to_owned()
        } else {
            format!("{} {}",
                    self.feature_args.len(),
                    self.feature_args
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>()
                        .join(" "))
        };

        write!(f,
               "{} {} {} {} {}",
               self.linear_dev,
               *self.start_offset,
               self.up_interval,
               self.down_interval,
               feature_args)
    }
}

impl FromStr for FlakeyDevTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<FlakeyDevTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();

        if vals.len() < 4 {
            let err_msg = format!("expected at least five values in params string \"{}\", found {}",
                                  s,
                                  vals.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let linear_dev = parse_device(vals[0])?;

        let start_offset = vals[1]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for start_offset from \"{}\"",
                                             vals[1]))
                     })?;

        let up_interval = vals[1]
            .parse::<u64>()
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for up_interval from \"{}\"",
                                             vals[1]))
                     })?;

        let down_interval = vals[2]
            .parse::<u64>()
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for down_interval from \"{}\"",
                                             vals[2]))
                     })?;

        let num_feature_args = vals[4]
            .parse::<usize>()
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for number of feature args from \"{}\"",
                                    vals[4]))})?;

        let feature_args: Vec<String> = vals[5..5 + num_feature_args]
            .iter()
            .map(|x| x.to_string())
            .collect();

        Ok(FlakeyDevTargetParams::new(linear_dev,
                                      start_offset,
                                      up_interval,
                                      down_interval,
                                      feature_args))
    }
}

impl TargetParams for FlakeyDevTargetParams {}

/// A DM construct of combined Segments
#[derive(Debug)]
pub struct FlakeyDev {
    /// Data about the device
    dev_info: Box<DeviceInfo>,
    start_offset: Sectors,
    linear_dev: LinearDev,
}

impl DmDevice<FlakeyDevTargetParams> for FlakeyDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    fn equivalent_tables(left: &[TargetLine<FlakeyDevTargetParams>],
                         right: &[TargetLine<FlakeyDevTargetParams>])
                         -> DmResult<bool> {
        Ok(left == right)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.linear_dev.size() - self.start_offset
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        Ok(())
    }

    fn uuid(&self) -> Option<&DmUuid> {
        uuid!(self)
    }
}


impl FlakeyDev {
    /// new
    pub fn new(dm: &DM,
               name: &DmName,
               linear_dev: LinearDev,
               offset: Sectors,
               up_interval: u64,
               down_interval: u64,
               feature_args: Vec<String>)
               -> DmResult<FlakeyDev> {
        if device_exists(dm, name)? {
            let err_msg = format!("flakeydev {} already exists", name);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let table = FlakeyDev::dm_table(&linear_dev,
                                        offset,
                                        up_interval,
                                        down_interval,
                                        feature_args);
        let dev_info = device_create(dm, name, None, &table)?;

        Ok(FlakeyDev {
               dev_info: Box::new(dev_info),
               linear_dev: linear_dev,
               start_offset: offset,
           })
    }
    /// setup
    pub fn setup(dm: &DM,
                 name: &DmName,
                 linear_dev: LinearDev,
                 offset: Sectors,
                 up_interval: u64,
                 down_interval: u64,
                 feature_args: Vec<String>)
                 -> DmResult<FlakeyDev> {

        let table = FlakeyDev::dm_table(&linear_dev,
                                        offset,
                                        up_interval,
                                        down_interval,
                                        feature_args);
        let dev = if device_exists(dm, name)? {
            return Err(DmError::Dm(ErrorEnum::Invalid, "device already exists".into()));
        } else {
            let dev_info = device_create(dm, name, None, &table)?;
            FlakeyDev {
                dev_info: Box::new(dev_info),
                linear_dev: linear_dev,
                start_offset: offset,
            }
        };
        Ok(dev)
    }

    /// Generate a table to be passed to DM.  The format of the table entries
    /// is:
    /// <dev path> <offset> <up interval> <down interval> \
    ///   [<num_features> [<feature arguments>]]
    ///
    /// Table parameters
    /// ----------------
    ///  <dev path> <offset> <up interval> <down interval> \
    ///    [<num_features> [<feature arguments>]]
    ///
    /// Mandatory parameters:
    ///    <dev path>: Full pathname to the underlying block-device, or a
    ///                "major:minor" device-number.
    ///    <offset>: Starting sector within the device.
    ///    <up interval>: Number of seconds device is available.
    ///    <down interval>: Number of seconds device returns errors.
    ///
    /// Optional feature parameters:
    ///  If no feature parameters are present, during the periods of
    ///  unreliability, all I/O returns errors.
    ///
    /// drop_writes:
    ///
    ///	All write I/O is silently ignored.
    ///	Read I/O is handled correctly.
    ///
    /// corrupt_bio_byte <Nth_byte> <direction> <value> <flags>:
    ///
    ///	During <down interval>, replace <Nth_byte> of the data of
    ///	each matching bio with <value>.
    ///
    ///    <Nth_byte>: The offset of the byte to replace.
    ///		Counting starts at 1, to replace the first byte.
    ///    <direction>: Either 'r' to corrupt reads or 'w' to corrupt writes.
    ///		 'w' is incompatible with drop_writes.
    ///    <value>: The value (from 0-255) to write.
    ///    <flags>: Perform the replacement only if bio->bi_opf has all the
    ///	     selected flags set.
    fn dm_table(linear_dev: &LinearDev,
                start_offset: Sectors,
                up_interval: u64,
                down_interval: u64,
                feature_args: Vec<String>)
                -> Vec<TargetLine<FlakeyDevTargetParams>> {
        vec![TargetLine {
                 start: start_offset,
                 length: linear_dev.size() - start_offset,
                 target_type: TargetTypeBuf::new("flakey".into()).expect("< length limit"),
                 params: FlakeyDevTargetParams::new(linear_dev.device(),
                                                    start_offset,
                                                    up_interval,
                                                    down_interval,
                                                    feature_args),
             }]
    }



    /// Set the name for this FlakeyDev.
    pub fn set_name(&mut self, dm: &DM, name: &DmName) -> DmResult<()> {
        if self.name() == name {
            return Ok(());
        }
        dm.device_rename(self.name(), &DevId::Name(name))?;
        self.dev_info = Box::new(dm.device_info(&DevId::Name(name))?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use std::path::Path;

    use super::super::consts::{IEC, SECTOR_SIZE};
    use super::super::device::{Device, devnode_to_devno};
    use super::super::loopbacked::{test_with_spec, write_sectors};
    use super::super::segment::Segment;
    use super::super::types::Bytes;

    use super::*;

    fn test_flakey_dev(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);
        let dm = DM::new().unwrap();
        let name = "linear-dev";
        let length = Bytes(IEC::Mi).sectors();
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let segments = &[Segment::new(dev, Sectors(0), length)];
        let linear_dev = LinearDev::setup(&dm,
                                          DmName::new(name).expect("valid format"),
                                          None,
                                          segments)
                .unwrap();
        let flakey_dev = FlakeyDev::setup(&dm,
                                          DmName::new("flakey-test-dev").expect("valid format"),
                                          linear_dev,
                                          Sectors(0),
                                          0,
                                          1,
                                          vec![])
                .unwrap();

        assert!(write_sectors(flakey_dev.devnode(),
                              Sectors(0),
                              Sectors(1),
                              &[1u8; SECTOR_SIZE])
                        .is_err());
    }

    #[test]
    fn loop_test_flakey_dev() {
        test_with_spec(1, test_flakey_dev);
    }

}
