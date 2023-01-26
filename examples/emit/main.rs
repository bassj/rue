use std::path::Path;
use std::fs;

fn main() {
    let rue_source = include_str!("source.rue");

    let ast = match rue::parse::parse_source(rue_source) {
        Ok(ast) => ast,
        Err(e) => {
            panic!("{}", e)
        }
    };

    let output = Path::new("./ruebinary.o");
    let archive_output = Path::new("./librueprog.a");

    rue::codegen::llvm::emit_module(ast, output);

    rue::codegen::llvm::link_binaries_into_archive(vec![output], archive_output);

    fs::remove_file(output).unwrap();
}