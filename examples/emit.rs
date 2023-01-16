use inkwell::{context::Context, targets::*, OptimizationLevel, module::Linkage};
use std::path::Path;

fn main() {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let void_type = context.void_type();
    let i32_type = context.i32_type();

    let print_function_signature = void_type.fn_type(&[], false);
    let external_print_function = module.add_function("rue_print", print_function_signature, Some(Linkage::AvailableExternally));

    let main_function_signature = i32_type.fn_type(&[], false);
    let main_function = module.add_function("rue_main", main_function_signature, None);
    let basic_block = context.append_basic_block(main_function, "entry");

    let int_constant = i32_type.const_int(420, false);

    builder.position_at_end(basic_block);
    builder.build_call(external_print_function, &[], "rue_print");
    builder.build_return(Some(&int_constant));

    Target::initialize_all(&InitializationConfig::default());

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

    let file_name = "ruemodule.o";
    let path = Path::new(file_name);

    target_machine.write_to_file(&module, FileType::Object, path).unwrap();

    module.print_to_stderr();
}
