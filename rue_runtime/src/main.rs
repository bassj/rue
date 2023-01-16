extern "C" {
    fn rue_main() -> i32;
}

#[no_mangle]
pub extern "C" fn rue_print() {
    println!("Hello World from rue!");
}

fn main() {
    let retval = unsafe { rue_main() };

    println!("Return value from fake rue main: {}", retval);
}