use inkwell::{
    builder::Builder,
    context::ContextRef,
    module::{Linkage, Module},
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue},
};

use crate::{
    ast::{Expression, Operator, Statement}, codegen::llvm::check_type_compatibility, types::RueType
};

use super::{scope, IntoBasicValue};

/// Maps rue types into their corresponding inkwell type
fn llvm_type_from_rue_type<'ctx>(
    rue_type: RueType,
    context: &ContextRef<'ctx>,
) -> AnyTypeEnum<'ctx> {
    match rue_type {
        RueType::Integer { bit_width, signed } => match (bit_width, signed) {
            (8, _) => context.i8_type(),
            (16, _) => context.i16_type(),
            (32, _) => context.i32_type(),
            (64, _) => context.i64_type(),
            (128, _) => context.i128_type(),
            _ => unimplemented!(
                "Unable to convert int into llvm type: signed: {} bit_width: {}",
                signed,
                bit_width
            ),
        }
        .into(),
        RueType::Unit => context.void_type().into(),
        t => todo!("convert {:#?} into llvm type", t),
    }
}

/// Generates the llvm IR representation of the passed rue expression. Places the IR in the module parameter.
/// If the expression has a type other than void, this function will return a generic inkwell type representing that expression's value.
pub fn generate_expression<'ctx>(
    expr: Expression,
    scope: &mut scope::Scope<'ctx>,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
) -> Option<Box<dyn BasicValue<'ctx> + 'ctx>> {
    match expr {
        Expression::FunctionInvocation(func_name, func_args) => {
            let func_val = module
                .get_function(func_name.as_str())
                .expect(format!("Cannot find function '{}'", func_name).as_str());

            let mut arg_values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(func_args.len());

            // Type checking to make sure we're passing in valid parameters.
            let func_params = func_val.get_type().get_param_types();

            assert_eq!(
                func_params.len(),
                func_args.len(),
                "Function invocation has correct number of parameters."
            );

            for (index, arg) in func_args.into_iter().enumerate() {
                let param_type = func_params[index];

                if arg.is_constant() {
                    let arg_val = arg.compute_value().into_basic_value(module);
                    let arg_type = arg_val.get_type().as_basic_type_enum();
                    check_type_compatibility(param_type, arg_type);
                    arg_values.push(arg_val.into());
                } else {
                    if let Some(arg_val) = generate_expression(arg, scope, builder, module) {
                        let arg_val = arg_val.as_basic_value_enum();
                        let arg_type = arg_val.get_type().as_basic_type_enum();
                        check_type_compatibility(param_type, arg_type);
                        arg_values.push(arg_val.into());
                    } else {
                        panic!("Cannot use void type as function parameter");
                        // TODO: proper error handling for codegen errors.
                    }
                }
            }

            if let Some(left) = builder
                .build_call(func_val, arg_values.as_slice(), func_name.as_str())
                .unwrap()
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
                Some(Box::new(expr.compute_value().into_basic_value(module)))
            } else {
                match expr {
                    Expression::BinaryOperation(op, lhs, rhs) => {
                        let lhs = generate_expression(*lhs, scope, builder, module)
                            .expect("trying to use void type in operation")
                            .as_basic_value_enum()
                            .into_int_value();
                        let rhs = generate_expression(*rhs, scope, builder, module)
                            .expect("trying to use void type in operation")
                            .as_basic_value_enum()
                            .into_int_value();

                        let op_val = match op {
                            Operator::Add => {
                                builder.build_int_add(lhs, rhs, "integer addition").unwrap()
                            }
                            Operator::Subtract => builder
                                .build_int_sub(lhs, rhs, "integer subtraction")
                                .unwrap(),

                            Operator::Divide => builder
                                .build_int_signed_div(lhs, rhs, "integer division")
                                .unwrap(),

                            Operator::Multiply => builder
                                .build_int_mul(lhs, rhs, "integer multiplication")
                                .unwrap(),
                        };

                        Some(Box::new(op_val.as_basic_value_enum()))
                    }
                    _ => unimplemented!(),
                }
            }
        }
        Expression::Variable(var_name) => {
            let variable = scope.find_variable(&var_name).expect("Cannot find variable"); // TODO: Proper error system for these type of errors.

            // TODO: At some point, we should support operators for references / dereferencing
            // For now I am just going to automatically dereference the value whenever we access a variable.

            // TODO: There needs to be some way to know what type we are dereferencing into
            let t = module.get_context().i32_type();
            let var_value = builder
                .build_load(t, variable, "temp")
                .expect("Failed to build load instruction");
            Some(Box::new(var_value))
        }
        e => Some(Box::new(e.compute_value().into_basic_value(module))),
    }
}

/// Genereates the llvm IR representation of the passed statment.
/// The IR will be placed in the module parameter.
pub fn generate_statement<'ctx>(
    stmt: Statement,
    scope: &mut scope::Scope<'ctx>,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
) {
    match stmt {
        Statement::Expression(expr) => {
            generate_expression(expr, scope, builder, module);
        }
        Statement::VariableDeclaration(var_name, var_type, var_value) => {
            let context = module.get_context();
            let var_value = generate_expression(var_value, scope, builder, module)
                .unwrap()
                .as_basic_value_enum();

            let var_type = match var_type {
                Some(var_type) => llvm_type_from_rue_type(var_type, &context).try_into().expect("Failed to convert any type into basic type value, probably should replace this with a real error sometime."),
                None => var_value.get_type(),
            };

            let var_local = builder
                .build_alloca(var_type, var_name.as_str())
                    .expect("Failed to build alloca");
            builder.build_store(var_local, var_value).unwrap();
            scope.add_variable(&var_name, var_local);
        }
        Statement::FunctionDeclaration {
            function_name,
            function_parameters,
            function_return_type,
            is_external_function,
        } => {
            let context = module.get_context();

            let param_types: Vec<BasicMetadataTypeEnum> = function_parameters
                .into_iter()
                .map(|(_param_name, param_type)| {
                    llvm_type_from_rue_type(param_type, &context)
                        .try_into()
                        .unwrap()
                })
                .collect();

            let return_type =
                llvm_type_from_rue_type(function_return_type, &context);

            let func_signature = if return_type.is_void_type() {
                let return_type = return_type.into_void_type();
                return_type.fn_type(param_types.as_slice(), false)
            } else {
                let return_type: BasicTypeEnum = return_type.try_into().unwrap();
                return_type.fn_type(param_types.as_slice(), false)
            };

            let func_linkage = if is_external_function {
                Some(Linkage::AvailableExternally)
            } else {
                todo!("Implement non-external function declarations")
            };

            module.add_function(function_name.as_str(), func_signature, func_linkage);
        }
    };
}
