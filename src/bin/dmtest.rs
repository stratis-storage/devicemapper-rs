extern crate devicemapper;

use devicemapper as dm;

fn main() {
    println!("Hello, world!");

    let dmi = dm::DM::new().unwrap();

    let x = dmi.version().unwrap();
    println!("{:?}", x);

    let x = dmi.list_devices(dm::DmFlags::empty()).unwrap();
    println!("{:?}", x);

    let x = dmi.table_status("zon-swap", dm::DmFlags::empty()).unwrap();
    println!("{:?}", x);

}
