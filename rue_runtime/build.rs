fn main() {
    println!("cargo:rustc-link-search=native=/mnt/d/projects/rue/");
    println!("cargo:rustc-link-lib=static=rueprog");
}