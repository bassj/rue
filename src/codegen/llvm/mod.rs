mod generate;

use generate::*;

use crate::{ast, types::RueValue};
use ar;
use inkwell::{
    context::Context, module::*, targets::*, types::BasicTypeEnum, values::BasicValueEnum,
    OptimizationLevel,
};
use std::ffi::OsStr;
use std::fs::File;
use std::path::Path;

pub mod scope;

/*
 * TODO: this function should probably be a trait, like CanCoerce or Coercable, something
 * to indicate which types a given Rue Type can be automatically converted into.
 *
 * Ideally this type of logic should happen in the types module. That way typechecking logic can
 * be seperate from the codegen logic. This would enable consistent typechecking rules if some compile target
 * other than llvm is desired.
 *
 * 1/31/2023
 *
 * On second thought, I probably won't remove this type check, but I would like to make it mostly
 * redundant with a semantic analysis step in the rue compiler.
 */
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

/// Emits the ast paramter as an x86 object file
pub fn emit_module<P: AsRef<Path>>(rue_module: ast::Module, file: P) {
    let context = Context::create();
    let module = context.create_module("main"); // TODO: Some way to specify which module this is.
                                                // Maybe it would make sense to add some modeling for what a module actually is.
    let builder = context.create_builder();

    let mut scope_graph = scope::ScopeGraph::new();

    // Add in the external functions from the runtime.
    // TODO: The rue programming language should probably have some way to import
    // modules, which would determine what external functions we need to be aware of.

    let i32_type = context.i32_type();

    // we should now have the ability to do this within rue
    // let print_function_signature = void_type.fn_type(&[i32_type.into()], false);
    // let _external_print_function = module.add_function(
    //     "print",
    //     print_function_signature,
    //     Some(Linkage::AvailableExternally),
    // );

    // TODO: For now, since we don't have a concept of function declaration, we're just going to
    // stick everything we generate in the body of a "main" function.

    let main_function_signature = i32_type.fn_type(&[], false);
    let main_function = module.add_function("_rue_main", main_function_signature, None);
    let basic_block = context.append_basic_block(main_function, "entry");
    let int_constant = i32_type.const_int(0, false);

    builder.position_at_end(basic_block);

    // Now we're going to generate our code inside the main funcion.
    for stmt in rue_module.statements {
        generate_statement(stmt, &mut scope_graph.root_scope, &builder, &module);
    }

    // To wrap up the function, we're just going to return the value we created in the
    // int_constant variable above

    builder.build_return(Some(&int_constant))
        .unwrap();

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

/// Uses the ar crate to stuff any specified binary files into an archive.
/// Subsequently executes the "ranlib" command on the archive, which generates a symbol table.
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

/// Trait for converting some type into a BasicValueEnum. This is how inkwell represents an LLVM value.
trait IntoBasicValue {
    fn into_basic_value<'ctx>(self, module: &Module<'ctx>) -> BasicValueEnum<'ctx>;
}

impl IntoBasicValue for RueValue {
    fn into_basic_value<'ctx>(self, module: &Module<'ctx>) -> BasicValueEnum<'ctx> {
        let context = module.get_context();

        #[allow(unreachable_patterns)]
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
    use super::*;
    use crate::types::RueInteger;

    /// Tests that the implementation of IntoBasicValue for RueValue
    /// assigns the correct bit-width. Currently only checks signed integer types.
    #[test]
    fn test_into_basic_value_for_rue_value_bit_width() {
        let context = Context::create();
        let module = context.create_module("test");

        let value = RueInteger::from(0i8);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.get_type().into_int_type().get_bit_width(),
            8,
            "bit width for i8 is correct"
        );

        let value = RueInteger::from(0i16);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.get_type().into_int_type().get_bit_width(),
            16,
            "bit width for i16 is correct"
        );

        let value = RueInteger::from(0i32);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.get_type().into_int_type().get_bit_width(),
            32,
            "bit width for i32 is correct"
        );

        let value = RueInteger::from(0i64);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.get_type().into_int_type().get_bit_width(),
            64,
            "bit width for i64 is correct"
        );

        let value = RueInteger::from(0i128);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.get_type().into_int_type().get_bit_width(),
            128,
            "bit width for i128 is correct"
        );
    }

    /// Tests that the implementation of IntoBasicValue for RueValue
    /// assigns the correct value. Currently only tests signed integers.
    #[test]
    fn test_into_basic_value_for_rue_value() {
        let context = Context::create();
        let module = context.create_module("test");

        // Signed integer
        let value = RueInteger::from(-12345);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.into_int_value().get_sign_extended_constant(),
            Some(-12345),
            "Correctly stores negative signed value"
        );

        let value = RueInteger::from(12345);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.into_int_value().get_sign_extended_constant(),
            Some(12345),
            "Correctly stores positive signed value"
        );

        let value = RueInteger::from(i64::MAX);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.into_int_value().get_sign_extended_constant(),
            Some(i64::MAX),
            "Correctly stores largest positive signed value"
        );

        let value = RueInteger::from(i64::MIN);
        let basic_value = RueValue::from(value).into_basic_value(&module);
        assert_eq!(
            basic_value.into_int_value().get_sign_extended_constant(),
            Some(i64::MIN),
            "Correctly stores largest negative signed value"
        );

        // TODO: probably should be testing with 128 bit values, but inkwell only returns 64 bit values.
        // Should see if there is some way to test with 128 bit values.
    }
}
