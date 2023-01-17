extern "C" {
    fn rue_main() -> i32;
}

#[no_mangle]
pub extern "C" fn rue_print(c: i32) {
    print!("{}", char::from(<i32 as TryInto<u8>>::try_into(c).unwrap()));
}

fn main() {
    let retval = unsafe { rue_main() };

    println!("Return value from fake rue main: {}", retval);
}