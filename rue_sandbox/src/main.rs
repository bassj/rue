#![allow(unused_imports)]

use rue_runtime;

extern {
    fn _rue_main() -> i32;
}

fn main() {
    unsafe { _rue_main(); }
}