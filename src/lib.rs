use std::fs;
use std::path::Path;
use std::env;

pub mod ast;
pub mod parse;
pub mod codegen;

#[derive(Debug)]
struct ModuleSource {
    file_name: String,
    file_source: String,
}

pub fn autosource() {
    fn _scan_for_sources<P: AsRef<Path>>(path: P) -> Vec<ModuleSource> {
        let mut sources = Vec::new();

        for entry in fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();

            if path.is_dir() {
                sources.extend(_scan_for_sources(path));
            } else if let Some(ext_osstr) = path.extension() {
                let ext = ext_osstr.to_str().unwrap();

                if ext == "rue" {
                    let file_name = path.file_stem().unwrap().to_str().unwrap().to_string();
                    let file_source = fs::read_to_string(path).unwrap();

                    sources.push(ModuleSource {
                        file_name,
                        file_source,
                    });
                }
            }
        }

        sources
    }

    let out_dir = env::var("OUT_DIR").unwrap();
    let obj_dir = format!("{}/objs/", out_dir);

    std::fs::create_dir(obj_dir.clone()).unwrap();

    let sources = _scan_for_sources("./src");
    let mut objs = vec![];

    for source in sources {
        let ast = parse::parse_source(source.file_source.as_str()).unwrap();
        let obj_path = format!("{}{}.o", obj_dir, source.file_name);
        codegen::llvm::generate_binary(ast, obj_path.as_str());
        objs.push(obj_path);
    }

    let archive_path = format!("{}/librueprog.a", out_dir);

    codegen::llvm::link_binaries_into_archive(objs, archive_path);

    println!("cargo:rustc-link-search=native={}", out_dir);
    println!("cargo:rustc-link-lib=static=rueprog");
}