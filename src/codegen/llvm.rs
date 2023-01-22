use crate::{
    ast::{Expression, Operator, Statement},
    types::RueValue,
};
use ar;
use inkwell::{
    builder::Builder,
    context::{Context, ContextRef},
    module::*,
    targets::*,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    OptimizationLevel, types::{BasicType, BasicTypeEnum},
};
use std::ffi::OsStr;
use std::fs::File;
use std::path::Path;

fn build_expr_into_const<'ctx>(
    expr: Expression,
    _builder: &Builder,
    module: &Module<'ctx>,
) -> BasicMetadataValueEnum<'ctx> {
    let context = module.get_context();

    let val = expr.compute_value();

    match val {
        RueValue::Integer(int_val) => {
            let const_type = match int_val.bit_width {
                8 => context.i8_type(),
                16 => context.i16_type(),
                32 => context.i32_type(),
                64 => context.i64_type(),
                128 => context.i128_type(),
                _ => unimplemented!(),
            };

            inkwell::values::BasicMetadataValueEnum::IntValue(
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

fn generate_expression<'ctx>(
    expr: Expression,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
) -> Option<Box<dyn BasicValue<'ctx> + 'ctx>> {
    match expr {
        Expression::FunctionInvocation(func_name, func_args) => {
            let func_val = module
                .get_function(func_name.as_str())
                .expect(format!("Cannot find function '{}'", func_name).as_str());

            let mut arg_values = Vec::with_capacity(func_args.len());

            for arg in func_args {
                if arg.is_constant() {
                    let arg_val = build_expr_into_const(arg, builder, module);
                    arg_values.push(arg_val);
                } else {
                    if let Some(arg_val) = generate_expression(arg, builder, module) {
                        let arg_val = arg_val.as_basic_value_enum();
                        arg_values.push(arg_val.into());
                    } else {
                        panic!("Cannot use void type as function parameter");
                        // TODO: proper error handling for codegen errors.
                    }
                }
            }

            if let Some(left) = builder
                .build_call(func_val, arg_values.as_slice(), func_name.as_str())
                .try_as_basic_value()
                .left()
            {
                let b: Box<dyn BasicValue<'ctx>> = Box::new(left);
                Some(b)
            } else {
                None
            }
        }
        Expression::BinaryOperation(op, lhs, rhs) => {
            let expr = Expression::BinaryOperation(op, lhs, rhs);

            if expr.is_constant() {
                let val = build_expr_into_const(expr, builder, module);
                Some(Box::new(BasicValueEnum::try_from(val).unwrap()))
            } else {
                match expr {
                    Expression::BinaryOperation(op, lhs, rhs) => {
                        let lhs = generate_expression(*lhs, builder, module)
                            .expect("trying to use void type in operation")
                            .as_basic_value_enum()
                            .into_int_value();
                        let rhs = generate_expression(*rhs, builder, module)
                            .expect("trying to use void type in operation")
                            .as_basic_value_enum()
                            .into_int_value();

                        let op_val = match op {
                            Operator::Add => builder.build_int_add(lhs, rhs, "integer addition"),
                            Operator::Subtract => {
                                builder.build_int_sub(lhs, rhs, "integer subtraction")
                            }

                            Operator::Divide => {
                                builder.build_int_signed_div(lhs, rhs, "integer division")
                            }

                            Operator::Multiply => {
                                builder.build_int_mul(lhs, rhs, "integer multiplication")
                            }
                        };

                        Some(Box::new(op_val.as_basic_value_enum()))
                    }
                    _ => unimplemented!(),
                }
            }
        }
        Expression::Variable(var_name) => {
            let variable = module
                .get_global(var_name.as_str())
                .expect("Cannot find variable");
            Some(Box::new(variable))
        }
        e => Some(Box::new(
            BasicValueEnum::try_from(build_expr_into_const(e, builder, module)).unwrap(),
        )),
    }
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
        _ => panic!("No such type")
    }.as_basic_type_enum()
}

fn generate_statement<'ctx>(stmt: Statement, builder: &Builder<'ctx>, module: &Module<'ctx>) {
    match stmt {
        Statement::Expression(expr) => {
            generate_expression(expr, builder, module);
        }
        Statement::VariableDeclaration(var_name, var_type, var_value) => {
            let context = module.get_context();
            let var_value = generate_expression(var_value, builder, module)
                .unwrap()
                .as_basic_value_enum();

            let var_type = if let Some(var_type) = var_type {
                llvm_type_from_type_string(var_type, context)
            } else {
                var_value.get_type()
            };

            let var_global = module.add_global(var_type, None, var_name.as_str());
            var_global.set_linkage(Linkage::External);

            var_global.set_initializer(
                &var_value,
            );
        }
    };
}

pub fn generate_binary<P: AsRef<Path>>(ast: Vec<Statement>, file: P) {
    let context = Context::create();
    let module = context.create_module("main"); // TODO: Some way to specify which module this is.
                                                // Maybe it would make sense to add some modeling for what a module actually is.
    let builder = context.create_builder();

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
        generate_statement(stmt, &builder, &module);
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
