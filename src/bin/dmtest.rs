extern crate devicemapper;

use devicemapper as dm;

fn main() {
    let dmi = dm::DM::new().unwrap();

    println!("Calling version()");
    let x = dmi.version().unwrap();
    println!("{:?}", x);

    println!("Calling list_devices()");
    let x = dmi.list_devices(dm::DmFlags::empty()).unwrap();
    println!("{:?}", x);
    let (first_name, first_dev) = x[0].clone();

    println!("Calling list_versions()");
    let x = dmi.list_versions(dm::DmFlags::empty()).unwrap();
    println!("{:?}", x);

    println!("Calling table_deps()");
    let x = dmi.table_deps(first_dev, dm::DmFlags::empty()).unwrap();
    println!("{:?}", x);

    println!("Calling table_status()");
    let x = dmi.table_status(&first_name, dm::DmFlags::empty()).unwrap();
    println!("{:?}", x);

}
