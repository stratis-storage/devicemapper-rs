// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashSet;
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::DM;
use super::dm_flags::DmFlags;
use super::result::{DmError, DmResult, ErrorEnum};
use super::shared::{DmDevice, TargetLine, TargetParams, TargetTable, device_create, device_exists,
                    device_match, parse_device};
use super::types::{DevId, DmName, DmUuid, Sectors, TargetTypeBuf};


const FLAKEY_TARGET_NAME: &str = "flakey";
const LINEAR_TARGET_NAME: &str = "linear";


/// Struct representing params for a linear target
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LinearTargetParams {
    /// Device on which this segment resides.
    pub device: Device,
    /// Start offset in device on which this segment resides.
    pub start_offset: Sectors,
}

impl LinearTargetParams {
    /// Create a new LinearTargetParams struct
    pub fn new(device: Device, start_offset: Sectors) -> LinearTargetParams {
        LinearTargetParams {
            device,
            start_offset,
        }
    }
}

impl fmt::Display for LinearTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", LINEAR_TARGET_NAME, self.param_str())
    }
}

impl FromStr for LinearTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<LinearTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();
        if vals.len() != 3 {
            let err_msg = format!("expected 3 values in params string \"{}\", found {}",
                                  s,
                                  vals.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        if vals[0] != LINEAR_TARGET_NAME {
            let err_msg = format!("Expected a linear target entry but found target type {}",
                                  vals[0]);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let device = parse_device(vals[1])?;

        let start = vals[2]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for physical start offset \"{}\"",
                                    vals[2]))})?;

        Ok(LinearTargetParams::new(device, start))
    }
}

impl TargetParams for LinearTargetParams {
    fn param_str(&self) -> String {
        format!("{} {}", self.device, *self.start_offset)
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(LINEAR_TARGET_NAME.into()).expect("LINEAR_TARGET_NAME is valid")
    }
}

/// Target params for flakey target
// FIXME: Refine feature args handling. Reading the docs indicates that flakey
// feature args are unlike the one word feature args of cachedev or thinpooldev
// and will require more complicated management.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FlakeyTargetParams {
    /// The device on which this segment resides
    pub device: Device,
    /// The starting offset of this segments in the device.
    pub start_offset: Sectors,
    /// Interval during which flakey target is up, in seconds
    /// DM source type is unsigned, so restrict to u32.
    pub up_interval: u32,
    /// Interval during which flakey target is down, in seconds
    /// DM source type is unsigned, so restrict to u32.
    pub down_interval: u32,
    /// Optional feature arguments
    pub feature_args: HashSet<String>,
}

impl FlakeyTargetParams {
    /// Create a new flakey target param struct.
    pub fn new(device: Device,
               start_offset: Sectors,
               up_interval: u32,
               down_interval: u32,
               feature_args: Vec<String>)
               -> FlakeyTargetParams {
        FlakeyTargetParams {
            device,
            start_offset,
            up_interval,
            down_interval,
            feature_args: feature_args.into_iter().collect::<HashSet<_>>(),
        }
    }
}

impl fmt::Display for FlakeyTargetParams {
    /// Generate params to be passed to DM.  The format of the params is:
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", FLAKEY_TARGET_NAME, self.param_str())
    }
}

impl FromStr for FlakeyTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<FlakeyTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();

        if vals.len() < 5 {
            let err_msg = format!("expected at least five values in params string \"{}\", found {}",
                                  s,
                                  vals.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        if vals[0] != FLAKEY_TARGET_NAME {
            let err_msg = format!("Expected a flakey target entry but found target type {}",
                                  vals[0]);
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let device = parse_device(vals[1])?;

        let start_offset = vals[2]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for start_offset from \"{}\"",
                                             vals[2]))
                     })?;

        let up_interval = vals[3]
            .parse::<u32>()
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for up_interval from \"{}\"",
                                             vals[3]))
                     })?;

        let down_interval = vals[4]
            .parse::<u32>()
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for down_interval from \"{}\"",
                                             vals[4]))
                     })?;


        let num_feature_args = vals[5]
            .parse::<usize>()
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for number of feature args from \"{}\"",
                                    vals[5]))})?;

        let feature_args: Vec<String> = vals[6..6 + num_feature_args]
            .iter()
            .map(|x| x.to_string())
            .collect();

        Ok(FlakeyTargetParams::new(device,
                                   start_offset,
                                   up_interval,
                                   down_interval,
                                   feature_args))
    }
}

impl TargetParams for FlakeyTargetParams {
    fn param_str(&self) -> String {
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

        format!("{} {} {} {} {}",
                self.device,
                *self.start_offset,
                self.up_interval,
                self.down_interval,
                feature_args)
    }

    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(FLAKEY_TARGET_NAME.into()).expect("FLAKEY_TARGET_NAME is valid")
    }
}


/// Target params for linear dev. These are either flakey or linear.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LinearDevTargetParams {
    /// A flakey target
    Flakey(FlakeyTargetParams),
    /// A linear target
    Linear(LinearTargetParams),
}

impl fmt::Display for LinearDevTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LinearDevTargetParams::Flakey(ref flakey) => flakey.fmt(f),
            LinearDevTargetParams::Linear(ref linear) => linear.fmt(f),
        }
    }
}

impl FromStr for LinearDevTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<LinearDevTargetParams> {
        let target_type = s.splitn(2, ' ').next().ok_or_else(|| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("target line string \"{}\" did not contain any values", s))
        })?;
        if target_type == FLAKEY_TARGET_NAME {
            Ok(LinearDevTargetParams::Flakey(s.parse::<FlakeyTargetParams>()?))
        } else if target_type == LINEAR_TARGET_NAME {
            Ok(LinearDevTargetParams::Linear(s.parse::<LinearTargetParams>()?))
        } else {
            Err(DmError::Dm(ErrorEnum::Invalid,
                            format!("unexpected target type \"{}\"", target_type)))
        }
    }
}

impl TargetParams for LinearDevTargetParams {
    fn param_str(&self) -> String {
        match *self {
            LinearDevTargetParams::Flakey(ref flakey) => flakey.param_str(),
            LinearDevTargetParams::Linear(ref linear) => linear.param_str(),
        }
    }

    fn target_type(&self) -> TargetTypeBuf {
        match *self {
            LinearDevTargetParams::Flakey(ref flakey) => flakey.target_type(),
            LinearDevTargetParams::Linear(ref linear) => linear.target_type(),
        }
    }
}


/// A target table for a linear device. Such a table allows flakey targets
/// as well as linear targets.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LinearDevTargetTable {
    /// The device's table
    pub table: Vec<TargetLine<LinearDevTargetParams>>,
}

impl LinearDevTargetTable {
    /// Make a new LinearDevTargetTable from a suitable vec
    pub fn new(table: Vec<TargetLine<LinearDevTargetParams>>) -> LinearDevTargetTable {
        LinearDevTargetTable { table }
    }
}

impl fmt::Display for LinearDevTargetTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in &self.table {
            writeln!(f, "{} {} {}", *line.start, *line.length, line.params)?;
        }
        Ok(())
    }
}

impl TargetTable for LinearDevTargetTable {
    fn from_raw_table(table: &[(Sectors, Sectors, TargetTypeBuf, String)])
                      -> DmResult<LinearDevTargetTable> {
        Ok(LinearDevTargetTable {
               table: table
                   .into_iter()
                   .map(|x| -> DmResult<TargetLine<LinearDevTargetParams>> {
                            Ok(TargetLine::new(x.0,
                                               x.1,
                                               format!("{} {}", x.2.to_string(), x.3)
                                                   .parse::<LinearDevTargetParams>()?))
                        })
                   .collect::<DmResult<Vec<_>>>()?,
           })
    }

    fn to_raw_table(&self) -> Vec<(Sectors, Sectors, TargetTypeBuf, String)> {
        self.table
            .iter()
            .map(|x| (x.start, x.length, x.params.target_type(), x.params.param_str()))
            .collect::<Vec<_>>()
    }
}


/// A DM construct of combined Segments
#[derive(Debug)]
pub struct LinearDev {
    /// Data about the device
    dev_info: Box<DeviceInfo>,
    table: LinearDevTargetTable,
}

impl DmDevice<LinearDevTargetTable> for LinearDev {
    fn device(&self) -> Device {
        device!(self)
    }

    fn devnode(&self) -> PathBuf {
        devnode!(self)
    }

    // Since linear devices have no default or configuration parameters,
    // and the ordering of segments matters, two linear devices represent
    // the same linear device only if their tables match exactly.
    fn equivalent_tables(left: &LinearDevTargetTable,
                         right: &LinearDevTargetTable)
                         -> DmResult<bool> {
        Ok(left == right)
    }

    fn name(&self) -> &DmName {
        name!(self)
    }

    fn size(&self) -> Sectors {
        self.table.table.iter().map(|l| l.length).sum()
    }

    fn table(&self) -> &LinearDevTargetTable {
        table!(self)
    }

    fn teardown(self, dm: &DM) -> DmResult<()> {
        dm.device_remove(&DevId::Name(self.name()), DmFlags::empty())?;
        Ok(())
    }

    fn uuid(&self) -> Option<&DmUuid> {
        uuid!(self)
    }
}

/// Use DM to concatenate a list of segments together into a
/// linear block device of continuous sectors.
impl LinearDev {
    /// Construct a new block device by concatenating the given segments
    /// into linear space.
    /// If the device is already known to the kernel, just verifies that the
    /// segments argument passed exactly matches the kernel data.
    ///
    /// Warning: If the segments overlap, this method will succeed. However,
    /// the behavior of the linear device in that case should be treated as
    /// undefined.
    ///
    /// Note: A linear device is just a mapping in the kernel from locations
    /// in that device to locations on other devices which are specified by
    /// their device number. There is usually a device node so that data can
    /// be read from and written to the device. Other than that, it really
    /// has no existence. Consequently, there is no conflict in overloading
    /// this method to mean both "make a wholly new device" and "establish
    /// the existence of the requested device". Of course, a linear device
    /// is usually expected to hold data, so it is important to get the
    /// mapping just right.
    pub fn setup(dm: &DM,
                 name: &DmName,
                 uuid: Option<&DmUuid>,
                 table: Vec<TargetLine<LinearDevTargetParams>>)
                 -> DmResult<LinearDev> {
        let table = LinearDevTargetTable::new(table);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = LinearDev {
                dev_info: Box::new(dev_info),
                table,
            };
            device_match(dm, &dev, uuid)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table)?;
            LinearDev {
                dev_info: Box::new(dev_info),
                table,
            }
        };
        Ok(dev)
    }

    /// Set the segments for this linear device.
    /// This action puts the device in a state where it is ready to be resumed.
    /// Warning: It is the client's responsibility to make sure the designated
    /// segments are compatible with the device's existing segments.
    /// If they are not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_table(&mut self,
                     dm: &DM,
                     table: Vec<TargetLine<LinearDevTargetParams>>)
                     -> DmResult<()> {
        let table = LinearDevTargetTable::new(table);
        self.suspend(dm, false)?;
        self.table_load(dm, &table)?;
        self.table = table;
        Ok(())
    }

    /// Set the name for this LinearDev.
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
    use std::clone::Clone;
    use std::fs::OpenOptions;
    use std::path::Path;

    use super::super::device::{Device, devnode_to_devno};
    use super::super::loopbacked::{blkdev_size, test_with_spec};
    use super::super::util::test_name;

    use super::*;

    /// Verify that a new linear dev with 0 segments fails.
    fn test_empty(_paths: &[&Path]) -> () {
        assert!(LinearDev::setup(&DM::new().unwrap(),
                                 &test_name("new").expect("valid format"),
                                 None,
                                 vec![])
                        .is_err());
    }

    /// Verify that setting an empty table on an existing DM device fails.
    fn test_empty_table_set(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let params = LinearTargetParams::new(dev, Sectors(0));
        let table = vec![TargetLine::new(Sectors(0),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params))];
        let mut ld = LinearDev::setup(&dm, &name, None, table).unwrap();

        assert!(ld.set_table(&dm, vec![]).is_err());
        ld.resume(&dm).unwrap();
        ld.teardown(&dm).unwrap();
    }

    /// Verify that id rename succeeds.
    fn test_rename_id(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let params = LinearTargetParams::new(dev, Sectors(0));
        let table = vec![TargetLine::new(Sectors(0),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params))];
        let mut ld = LinearDev::setup(&dm, &name, None, table).unwrap();

        ld.set_name(&dm, &name).unwrap();
        assert_eq!(ld.name(), &*name);

        ld.teardown(&dm).unwrap();
    }

    /// Verify that after a rename, the device has the new name.
    fn test_rename(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let params = LinearTargetParams::new(dev, Sectors(0));
        let table = vec![TargetLine::new(Sectors(0),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params))];
        let mut ld = LinearDev::setup(&dm, &name, None, table).unwrap();

        let new_name = test_name("new_name").expect("valid format");
        ld.set_name(&dm, &new_name).unwrap();
        assert_eq!(ld.name(), &*new_name);

        ld.teardown(&dm).unwrap();
    }

    /// Verify that passing the same segments two times gets two segments.
    /// Verify that the size of the devnode is the size of the sum of the
    /// ranges of the segments. Verify that the table contains entries for both
    /// segments.
    fn test_duplicate_segments(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let params = LinearTargetParams::new(dev, Sectors(0));
        let table = vec![TargetLine::new(Sectors(0),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params.clone())),
                         TargetLine::new(Sectors(1),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params))];
        let range: Sectors = table.iter().map(|s| s.length).sum();
        let count = table.len();
        let ld = LinearDev::setup(&dm, &name, None, table).unwrap();

        let table = LinearDev::read_kernel_table(&dm, &DevId::Name(ld.name()))
            .unwrap()
            .table;
        assert_eq!(table.len(), count);
        match table[0].params {
            LinearDevTargetParams::Linear(ref device) => assert_eq!(device.device, dev),
            _ => panic!("unexpected param type"),
        }
        match table[1].params {
            LinearDevTargetParams::Linear(ref device) => assert_eq!(device.device, dev),
            _ => panic!("unexpected param type"),
        }

        assert_eq!(blkdev_size(&OpenOptions::new()
                                    .read(true)
                                    .open(ld.devnode())
                                    .unwrap())
                           .sectors(),
                   range);

        ld.teardown(&dm).unwrap();
    }

    /// Use five segments, each distinct. If parsing works correctly,
    /// default table should match extracted table.
    fn test_several_segments(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let table = (0..5)
            .map(|n| {
                     TargetLine::new(Sectors(n),
                                Sectors(1),
                                LinearDevTargetParams::Linear(LinearTargetParams::new(dev,
                                                                                      Sectors(n))))
                 })
            .collect::<Vec<_>>();
        let ld = LinearDev::setup(&dm, &name, None, table.clone()).unwrap();

        let loaded_table = LinearDev::read_kernel_table(&dm, &DevId::Name(ld.name())).unwrap();
        assert!(LinearDev::equivalent_tables(&LinearDevTargetTable::new(table), &loaded_table)
                    .unwrap());

        ld.teardown(&dm).unwrap();
    }

    /// Verify that constructing a second dev with the same name succeeds
    /// only if it has the same list of segments.
    fn test_same_name(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let params = LinearTargetParams::new(dev, Sectors(0));
        let table = vec![TargetLine::new(Sectors(0),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params))];
        let ld = LinearDev::setup(&dm, &name, None, table.clone()).unwrap();
        let params2 = LinearTargetParams::new(dev, Sectors(1));
        let table2 = vec![TargetLine::new(Sectors(0),
                                          Sectors(1),
                                          LinearDevTargetParams::Linear(params2))];
        assert!(LinearDev::setup(&dm, &name, None, table2).is_err());
        assert!(LinearDev::setup(&dm, &name, None, table).is_ok());
        ld.teardown(&dm).unwrap();
    }

    /// Verify constructing a second linear dev with the same segment succeeds.
    fn test_same_segment(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let ersatz = test_name("ersatz").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let params = LinearTargetParams::new(dev, Sectors(0));
        let table = vec![TargetLine::new(Sectors(0),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params))];
        let ld = LinearDev::setup(&dm, &name, None, table.clone()).unwrap();
        let ld2 = LinearDev::setup(&dm, &ersatz, None, table);
        assert!(ld2.is_ok());

        ld2.unwrap().teardown(&dm).unwrap();
        ld.teardown(&dm).unwrap();
    }

    /// Verify that suspending and immediately resuming doesn't fail.
    fn test_suspend(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = test_name("name").expect("valid format");
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let params = LinearTargetParams::new(dev, Sectors(0));
        let table = vec![TargetLine::new(Sectors(0),
                                         Sectors(1),
                                         LinearDevTargetParams::Linear(params))];
        let mut ld = LinearDev::setup(&dm, &name, None, table).unwrap();

        ld.suspend(&dm, false).unwrap();
        ld.suspend(&dm, false).unwrap();
        ld.resume(&dm).unwrap();
        ld.resume(&dm).unwrap();

        ld.teardown(&dm).unwrap();
    }

    #[test]
    fn loop_test_duplicate_segments() {
        test_with_spec(1, test_duplicate_segments);
    }

    #[test]
    fn loop_test_empty() {
        test_with_spec(0, test_empty);
    }

    #[test]
    fn loop_test_empty_table_set() {
        test_with_spec(1, test_empty_table_set);
    }

    #[test]
    fn loop_test_rename() {
        test_with_spec(1, test_rename);
    }

    #[test]
    fn loop_test_rename_id() {
        test_with_spec(1, test_rename_id);
    }

    #[test]
    fn loop_test_same_name() {
        test_with_spec(1, test_same_name);
    }

    #[test]
    fn loop_test_segment() {
        test_with_spec(1, test_same_segment);
    }

    #[test]
    fn loop_test_several_segments() {
        test_with_spec(1, test_several_segments);
    }

    #[test]
    fn loop_test_suspend() {
        test_with_spec(1, test_suspend);
    }
}
