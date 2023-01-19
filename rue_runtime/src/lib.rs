#[no_mangle]
pub extern "C" fn rue_print(c: i32) {
    print!("{}", char::from(<i32 as TryInto<u8>>::try_into(c).unwrap()));
}