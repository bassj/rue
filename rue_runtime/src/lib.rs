#[no_mangle]
pub unsafe extern "C" fn print(c: *const i32) {
    let c: i32 = *c;
    print!("{}", char::from(<i32 as TryInto<u8>>::try_into(c).unwrap()));
}