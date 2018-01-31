// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashSet;
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use either::{Either, Left, Right};

use super::device::Device;
use super::deviceinfo::DeviceInfo;
use super::dm::{DM, DmFlags};
use super::result::{DmResult, DmError, ErrorEnum};
use super::segment::Segment;
use super::shared::{DmDevice, TargetLine, TargetParams, TargetTable, device_create, device_exists,
                    device_match, parse_device, table_reload};
use super::types::{DevId, DmName, DmUuid, Sectors, TargetTypeBuf};


const FLAKEY_TARGET_NAME: &str = "flakey";
const LINEAR_TARGET_NAME: &str = "linear";

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LinearTargetParams {
    pub device: Device,
    pub physical_start_offset: Sectors,
}

impl LinearTargetParams {
    pub fn new(device: Device, physical_start_offset: Sectors) -> LinearTargetParams {
        LinearTargetParams {
            device: device,
            physical_start_offset: physical_start_offset,
        }
    }
}

impl fmt::Display for LinearTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.device, *self.physical_start_offset)
    }
}

impl FromStr for LinearTargetParams {
    type Err = DmError;

    fn from_str(s: &str) -> DmResult<LinearTargetParams> {
        let vals = s.split(' ').collect::<Vec<_>>();
        if vals.len() != 2 {
            let err_msg = format!("expected two values in params string \"{}\", found {}",
                                  s,
                                  vals.len());
            return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
        }

        let device = parse_device(vals[0])?;

        let start = vals[1]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|_| {
                DmError::Dm(ErrorEnum::Invalid,
                            format!("failed to parse value for physical start offset \"{}\"",
                                    vals[1]))})?;

        Ok(LinearTargetParams::new(device, start))
    }
}

impl TargetParams for LinearTargetParams {
    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(LINEAR_TARGET_NAME.into()).expect("< max length")
    }
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FlakeyTargetParams {
    pub linear_dev: Device,
    pub start_offset: Sectors,
    pub up_interval: u64,
    pub down_interval: u64,
    pub feature_args: HashSet<String>,
}

impl FlakeyTargetParams {
    pub fn new(device: Device,
               start_offset: Sectors,
               up_interval: u64,
               down_interval: u64,
               feature_args: Vec<String>)
               -> FlakeyTargetParams {
        FlakeyTargetParams {
            linear_dev: device,
            start_offset: start_offset,
            up_interval: up_interval,
            down_interval: down_interval,
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

        let device = parse_device(vals[0])?;

        let start_offset = vals[1]
            .parse::<u64>()
            .map(Sectors)
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for start_offset from \"{}\"",
                                             vals[1]))
                     })?;

        let up_interval = vals[2]
            .parse::<u64>()
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for up_interval from \"{}\"",
                                             vals[2]))
                     })?;

        let down_interval = vals[3]
            .parse::<u64>()
            .map_err(|_| {
                         DmError::Dm(ErrorEnum::Invalid,
                                     format!("failed to parse value for down_interval from \"{}\"",
                                             vals[3]))
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

        Ok(FlakeyTargetParams::new(device,
                                   start_offset,
                                   up_interval,
                                   down_interval,
                                   feature_args))
    }
}

impl TargetParams for FlakeyTargetParams {
    fn target_type(&self) -> TargetTypeBuf {
        TargetTypeBuf::new(FLAKEY_TARGET_NAME.into()).expect("< max length")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// The union of the two possible params which are linear and flakey.
/// Wraps either, but does not use it directly, since rules of trait
/// implementation make this impossible.
struct LinearDevTargetParams {
    value: Either<LinearTargetParams, FlakeyTargetParams>,
}

impl LinearDevTargetParams {
    fn new_linear(params: LinearTargetParams) -> LinearDevTargetParams {
        LinearDevTargetParams { value: Left(params) }
    }

    fn new_flakey(params: FlakeyTargetParams) -> LinearDevTargetParams {
        LinearDevTargetParams { value: Right(params) }
    }
}

impl fmt::Display for LinearDevTargetParams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            Left(ref linear) => linear.fmt(f),
            Right(ref flakey) => flakey.fmt(f),
        }
    }
}

impl TargetParams for LinearDevTargetParams {
    fn target_type(&self) -> TargetTypeBuf {
        self.value
            .as_ref()
            .either(|x| x.target_type(), |x| x.target_type())
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct LinearDevTargetTable {
    table: Vec<TargetLine<LinearDevTargetParams>>,
}

impl LinearDevTargetTable {
    /// Generate a table to be passed to DM.  The format of the table entries
    /// is:
    /// <logical start offset> <length> "linear" <linear-specific string>
    /// where the linear-specific string has the format:
    /// <maj:min> <physical start offset>
    pub fn new(segments: &[Segment]) -> LinearDevTargetTable {
        let mut table = Vec::new();
        let mut logical_start_offset = Sectors(0);
        for segment in segments {
            let (physical_start_offset, length) = (segment.start, segment.length);
            let line = TargetLine {
                start: logical_start_offset,
                length: length,
                params: LinearDevTargetParams::new_linear(
                    LinearTargetParams::new(segment.device,
                                            physical_start_offset)),
            };
            table.push(line);
            logical_start_offset += length;
        }

        LinearDevTargetTable { table: table }
    }
}

impl TargetTable for LinearDevTargetTable {
    // Since linear devices have no default or configuration parameters,
    // and the ordering of segments matters, two linear devices represent
    // the same linear device only if their tables match exactly.
    fn equivalent_devices(left: &LinearDevTargetTable, right: &LinearDevTargetTable) -> bool {
        left == right
    }

    fn read(table: &[(Sectors, Sectors, TargetTypeBuf, String)]) -> DmResult<LinearDevTargetTable> {
        Ok(LinearDevTargetTable {
               table: table
                   .into_iter()
                   .map(|x| -> DmResult<TargetLine<LinearDevTargetParams>> {

            let target_type = &x.2.to_string();
            if target_type != LINEAR_TARGET_NAME && target_type != FLAKEY_TARGET_NAME {
                let err_msg = format!("Parsing a linear table entry but found target type {}",
                                      target_type);
                return Err(DmError::Dm(ErrorEnum::Invalid, err_msg));
            }
            Ok(TargetLine {
                   start: x.0,
                   length: x.1,
                   params: if target_type == LINEAR_TARGET_NAME {
                       LinearDevTargetParams::new_linear(x.3.parse::<LinearTargetParams>()?)
                   } else {
                       LinearDevTargetParams::new_flakey(x.3.parse::<FlakeyTargetParams>()?)
                   },
               })
        })
                   .collect::<DmResult<Vec<_>>>()?,
           })
    }

    fn as_raw_table(&self) -> Vec<(Sectors, Sectors, TargetTypeBuf, String)> {
        self.table
            .iter()
            .map(|x| (x.start, x.length, x.params.target_type(), x.params.to_string()))
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
                 segments: &[Segment])
                 -> DmResult<LinearDev> {
        if segments.is_empty() {
            return Err(DmError::Dm(ErrorEnum::Invalid,
                                   "linear device must have at least one segment".into()));
        }

        let table = LinearDev::gen_table(segments);
        let dev = if device_exists(dm, name)? {
            let dev_info = dm.device_info(&DevId::Name(name))?;
            let dev = LinearDev {
                dev_info: Box::new(dev_info),
                table: table,
            };
            device_match(dm, &dev, uuid)?;
            dev
        } else {
            let dev_info = device_create(dm, name, uuid, &table)?;
            LinearDev {
                dev_info: Box::new(dev_info),
                table: table,
            }
        };
        Ok(dev)
    }

    /// Generate a table to be passed to DM.
    fn gen_table(segments: &[Segment]) -> LinearDevTargetTable {
        assert_ne!(segments.len(), 0);
        LinearDevTargetTable::new(segments)
    }

    /// Set the segments for this linear device.
    /// Warning: It is the client's responsibility to make sure the designated
    /// segments are compatible with the device's existing segments.
    /// If they are not, this function will still succeed, but some kind of
    /// data corruption will be the inevitable result.
    pub fn set_segments(&mut self, dm: &DM, segments: &[Segment]) -> DmResult<()> {
        if segments.is_empty() {
            return Err(DmError::Dm(ErrorEnum::Invalid,
                                   "linear device must have at least one segment".into()));
        }

        let table = LinearDev::gen_table(segments);
        table_reload(dm, &DevId::Name(self.name()), &table)?;
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
    use std::fs::OpenOptions;
    use std::path::Path;

    use super::super::device::{Device, devnode_to_devno};
    use super::super::loopbacked::{blkdev_size, test_with_spec};

    use super::*;

    /// Verify that a new linear dev with 0 segments fails.
    fn test_empty(_paths: &[&Path]) -> () {
        assert!(LinearDev::setup(&DM::new().unwrap(),
                                 DmName::new("new").expect("valid format"),
                                 None,
                                 &[])
                        .is_err());
    }

    /// Verify that id rename succeeds.
    fn test_rename_id(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let mut ld = LinearDev::setup(&dm,
                                      DmName::new(name).expect("valid format"),
                                      None,
                                      &[Segment::new(dev, Sectors(0), Sectors(1))])
                .unwrap();

        ld.set_name(&dm, DmName::new(name).expect("valid format"))
            .unwrap();
        assert_eq!(ld.name(), DmName::new(name).expect("valid format"));

        ld.teardown(&dm).unwrap();
    }

    /// Verify that after a rename, the device has the new name.
    fn test_rename(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let mut ld = LinearDev::setup(&dm,
                                      DmName::new(name).expect("valid format"),
                                      None,
                                      &[Segment::new(dev, Sectors(0), Sectors(1))])
                .unwrap();

        let new_name = "new_name";
        ld.set_name(&dm, DmName::new(new_name).expect("valid format"))
            .unwrap();
        assert_eq!(ld.name(), DmName::new(new_name).expect("valid format"));

        ld.teardown(&dm).unwrap();
    }

    /// Verify that passing the same segments two times gets two segments.
    /// Verify that the size of the devnode is the size of the sum of the
    /// ranges of the segments. Verify that the table contains entries for both
    /// segments.
    fn test_duplicate_segments(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1)),
                         Segment::new(dev, Sectors(0), Sectors(1))];
        let range: Sectors = segments.iter().map(|s| s.length).sum();
        let count = segments.len();
        let ld = LinearDev::setup(&dm,
                                  DmName::new(name).expect("valid format"),
                                  None,
                                  segments)
                .unwrap();

        let table = LinearDev::load_table(&dm, &DevId::Name(ld.name()))
            .unwrap()
            .table;
        assert_eq!(table.len(), count);
        assert_eq!(table[0].params.value.as_ref().left().unwrap().device, dev);
        assert_eq!(table[1].params.value.as_ref().left().unwrap().device, dev);

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
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let segments = (0..5)
            .map(|n| Segment::new(dev, Sectors(n), Sectors(1)))
            .collect::<Vec<Segment>>();

        let ld = LinearDev::setup(&dm,
                                  DmName::new(name).expect("valid format"),
                                  None,
                                  &segments)
                .unwrap();

        let table = LinearDev::load_table(&dm, &DevId::Name(ld.name())).unwrap();
        assert!(LinearDevTargetTable::equivalent_devices(&table, &LinearDev::gen_table(&segments)));

        ld.teardown(&dm).unwrap();
    }

    /// Verify that constructing a second dev with the same name succeeds
    /// only if it has the same list of segments.
    fn test_same_name(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let name = "name";
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1))];
        let ld = LinearDev::setup(&dm,
                                  DmName::new(name).expect("valid format"),
                                  None,
                                  segments)
                .unwrap();
        assert!(LinearDev::setup(&dm,
                                 DmName::new(name).expect("valid format"),
                                 None,
                                 &[Segment::new(dev, Sectors(1), Sectors(1))])
                        .is_err());
        assert!(LinearDev::setup(&dm,
                                 DmName::new(name).expect("valid format"),
                                 None,
                                 segments)
                        .is_ok());
        ld.teardown(&dm).unwrap();
    }

    /// Verify constructing a second linear dev with the same segment succeeds.
    fn test_same_segment(paths: &[&Path]) -> () {
        assert!(paths.len() >= 1);

        let dm = DM::new().unwrap();
        let dev = Device::from(devnode_to_devno(&paths[0]).unwrap().unwrap());
        let segments = &[Segment::new(dev, Sectors(0), Sectors(1))];
        let ld = LinearDev::setup(&dm,
                                  DmName::new("name").expect("valid format"),
                                  None,
                                  segments)
                .unwrap();
        let ld2 = LinearDev::setup(&dm,
                                   DmName::new("ersatz").expect("valid format"),
                                   None,
                                   segments);
        assert!(ld2.is_ok());

        ld2.unwrap().teardown(&dm).unwrap();
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
}
