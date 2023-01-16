use crate::ast::Expression;
use inkwell::{context::Context, targets::*, OptimizationLevel, module::*, builder::Builder};
use std::path::Path;
use ar;
use std::fs::File;

fn generate_expression(expr: Expression, builder: &Builder, module: &Module) {
    match expr {
        Expression::FunctionInvocation(func_name, func_args) => {
            let func_val = module.get_function(func_name.as_str()).expect(format!("Cannot find function '{}'", func_name).as_str());

            // TODO: Figure out how to handle code generation for the function arguments.
            builder.build_call(
                func_val,
                &[],
                func_name.as_str()
            );
        }, 
        _ => todo!()
    }
}

pub fn generate_binary(ast: Expression, file: &Path) {
    let context = Context::create();
    let module = context.create_module("main"); // TODO: Some way to specify which module this is.
    // Maybe it would make sense to add some modeling for what a module actually is.
    let builder = context.create_builder();

    // Add in the external functions from the runtime.
    // TODO: The rue programming language should probably have some way to import
    // modules, which would determine what external functions we need to be aware of.

    let void_type = context.void_type();
    let i32_type = context.i32_type();

    let print_function_signature = void_type.fn_type(&[], false);
    let external_print_function = module.add_function("rue_print", print_function_signature, Some(Linkage::AvailableExternally));

    // TODO: For now, since we don't have a concept of function declaration, we're just going to
    // stick everything we generate in the body of a "main" function. 

    let main_function_signature = i32_type.fn_type(&[], false);
    let main_function = module.add_function("rue_main", main_function_signature, None);
    let basic_block = context.append_basic_block(main_function, "entry");
    let int_constant = i32_type.const_int(0, false);

    builder.position_at_end(basic_block);

    // Now we're going to generate our code inside the main funcion.

    generate_expression(ast, &builder, &module);

    // To wrap up the function, we're just going to return the value we created in the
    // int_constant variable above

    builder.build_return(Some(&int_constant));

    // Now actually write the binary.
    Target::initialize_all(&InitializationConfig::default());

    // TODO: all of this should probably be configurable.
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-unknown-linux-gnu"),
            "x86-64",
            "+avx2",
            opt,
            reloc,
            model,
        )
        .unwrap();

    target_machine.write_to_file(&module, FileType::Object, file).unwrap();

    module.print_to_stderr();
}

pub fn link_binaries_into_archive(files: Vec<&Path>, output: &Path) {
    {
        let output_file = File::create(output).unwrap();
        let mut builder = ar::Builder::new(output_file);

        for object_file in files {
            builder.append_path(object_file).unwrap();
        }
    }

    // TODO: This isn't really portable, and should really be moved to some rust based implementation at some point.
    // Unfortunately, there doesn't yet exist equivalent functionality to ranlib for the rust programming language.
    std::process::Command::new(
        "ranlib"
    )
    .arg(output)
    .status()
    .expect("Failed to run ranlib on the archive");
}