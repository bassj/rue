mod generate;

use generate::*;

use crate::{
    ast::Statement,
    types::RueValue,
};
use ar;
use inkwell::{
    context::{Context, ContextRef},
    module::*,
    targets::*,
    types::{BasicType, BasicTypeEnum},
    values::BasicValueEnum,
    OptimizationLevel,
};
use std::ffi::OsStr;
use std::fs::File;
use std::path::Path;

use super::RueScope;

fn check_type_compatibility(lhs: BasicTypeEnum, rhs: BasicTypeEnum) {
    match (lhs, rhs) {
        (BasicTypeEnum::PointerType(left_ptr_type), BasicTypeEnum::PointerType(right_ptr_type)) => {
            // My intention here is that pointer types must be exact, while scalar types may
            // implicitly expand in size
            assert_eq!(
                left_ptr_type, right_ptr_type,
                "Left hand type is not compatible with right hand type"
            );
        }
        (BasicTypeEnum::IntType(left_int_type), BasicTypeEnum::IntType(right_int_type)) => {
            assert!(
                left_int_type.get_bit_width() >= right_int_type.get_bit_width(),
                "Right hand type cannot be contained in left hand type"
            )
        }
        (lhs, rhs) => panic!("Incompatible types: {:?} and {:?}", lhs, rhs),
    };
}

fn llvm_type_from_type_string(type_string: String, context: ContextRef) -> BasicTypeEnum {
    match type_string.as_str() {
        "i8" => context.i8_type(),
        "i16" => context.i16_type(),
        "i32" => context.i32_type(),
        "i64" => context.i64_type(),
        "i128" => context.i128_type(),
        "u8" => context.i8_type(),
        "u16" => context.i16_type(),
        "u32" => context.i32_type(),
        "u64" => context.i64_type(),
        "u128" => context.i128_type(),
        _ => panic!("No such type"),
    }
    .as_basic_type_enum()
}

pub fn generate_binary<P: AsRef<Path>>(ast: Vec<Statement>, file: P) {
    let context = Context::create();
    let module = context.create_module("main"); // TODO: Some way to specify which module this is.
                                                // Maybe it would make sense to add some modeling for what a module actually is.
    let builder = context.create_builder();

    let mut global_scope = RueScope::global();

    // Add in the external functions from the runtime.
    // TODO: The rue programming language should probably have some way to import
    // modules, which would determine what external functions we need to be aware of.

    let void_type = context.void_type();
    let i32_type = context.i32_type();

    let print_function_signature = void_type.fn_type(&[i32_type.into()], false);
    let _external_print_function = module.add_function(
        "print",
        print_function_signature,
        Some(Linkage::AvailableExternally),
    );

    // TODO: For now, since we don't have a concept of function declaration, we're just going to
    // stick everything we generate in the body of a "main" function.

    let main_function_signature = i32_type.fn_type(&[], false);
    let main_function = module.add_function("_rue_main", main_function_signature, None);
    let basic_block = context.append_basic_block(main_function, "entry");
    let int_constant = i32_type.const_int(0, false);

    builder.position_at_end(basic_block);

    // Now we're going to generate our code inside the main funcion.
    for stmt in ast {
        generate_statement(stmt, &mut global_scope, &builder, &module);
    }

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

    target_machine
        .write_to_file(&module, FileType::Object, file.as_ref())
        .unwrap();

    module.print_to_stderr();
}

pub fn link_binaries_into_archive<P: AsRef<Path> + AsRef<OsStr>>(files: Vec<P>, output: P) {
    {
        let output_file = File::create(output.as_ref() as &Path).unwrap();
        let mut builder = ar::Builder::new(output_file);

        for object_file in files {
            builder.append_path(object_file).unwrap();
        }
    }

    // TODO: This isn't really portable, and should really be moved to some rust based implementation at some point.
    // Unfortunately, there doesn't yet exist equivalent functionality to ranlib for the rust programming language.
    std::process::Command::new("ranlib")
        .arg(output)
        .status()
        .expect("Failed to run ranlib on the archive");
}

trait IntoBasicValue {
    fn into_basic_value<'ctx>(self, module: &Module<'ctx>) -> BasicValueEnum<'ctx>;
}

impl IntoBasicValue for RueValue {
    fn into_basic_value<'ctx>(self, module: &Module<'ctx>) -> BasicValueEnum<'ctx> {
        let context = module.get_context();

        match self {
            RueValue::Integer(int_val) => {
                let const_type = match int_val.bit_width {
                    8 => context.i8_type(),
                    16 => context.i16_type(),
                    32 => context.i32_type(),
                    64 => context.i64_type(),
                    128 => context.i128_type(),
                    _ => unimplemented!(),
                };

                inkwell::values::BasicValueEnum::IntValue(
                    const_type
                        .const_int_from_string(
                            int_val.value.to_string().as_str(),
                            inkwell::types::StringRadix::Decimal,
                        )
                        .unwrap(),
                )
            }
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::types::RueInteger;
    use super::*;

    #[test]
    fn test_into_basic_value_for_rue_value() {
        // Test that the 
        let value = 

    }
}