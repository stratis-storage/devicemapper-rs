extern crate devicemapper;

use devicemapper::dm::{DM, DevId};
use devicemapper::consts::{DmFlags, DM_STATUS_TABLE};

fn main() {
    let dmi = DM::new().unwrap();

    println!("Calling version()");
    let x = dmi.version().unwrap();
    println!("{:?}", x);

    println!("Calling list_devices()");
    let x = dmi.list_devices().unwrap();
    println!("{:?}", x);
    let (first_name, first_dev) = x[0].clone();

    println!("Calling list_versions()");
    let x = dmi.list_versions().unwrap();
    println!("{:?}", x);

    println!("Calling table_deps()");
    let x = dmi.table_deps(first_dev, DmFlags::empty()).unwrap();
    println!("{:?}", x);

    println!("Calling table_status() INFO");
    let x = dmi.table_status(&DevId::Name(&first_name), DmFlags::empty()).unwrap();
    println!("{:?}", x.1);

    println!("Calling table_status() TABLE");
    let x = dmi.table_status(&DevId::Name(&first_name), DM_STATUS_TABLE).unwrap();
    println!("{:?}", x.1);
}
