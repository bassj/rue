mod atom;
mod error;
mod expr;
mod func;
mod statement;
mod util;

use nom::Err;

use nom_locate::LocatedSpan;

use crate::ast::Module;

use error::ParseError;

type InputType<'p> = LocatedSpan<&'p str>;
type IResult<'p, T> = nom::IResult<InputType<'p>, T, ParseError<'p>>;

/// The main entry point for parsing a rue program
pub fn parse_source<'s, T: Into<&'s str>>(input: T) -> Result<Module, ParseError<'s>> {
    let src_string = input.into();
    let src = LocatedSpan::new(src_string.as_ref());

    let res = nom::combinator::all_consuming(nom::multi::many0(statement::parse_statement))(src);

    match res {
        Ok((_, stmt)) => Ok(Module { statements: stmt }),
        Err(e) => match e {
            Err::Failure(e) => Err(e),
            Err::Error(e) => Err(e),
            _ => unimplemented!(),
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::{CodeBlock, Expression, Module, Operator, Statement}, parse::parse_source, types::RueType};

    fn _test_parse_source(input: &str, expected_output: Module, message: &str) {
        let parser_res = parse_source(input);

        assert!(
            parser_res.is_ok(),
            "parse_source returns no error - {}:\n{}",
            message,
            parser_res.unwrap_err()
        );

        let module = parser_res.unwrap();

        assert_eq!(
            module, expected_output,
            "parse_source returns correct module - {}",
            message
        );
    }

    #[test]
    fn test_parse_source() {
        _test_parse_source(
            r#"
            extern fn print(i32)

            fn test_func(a: i32, b: i32) -> i32 {
                return a + b
            }
        "#,
            Module {
                statements: vec![
                    Statement::FunctionDeclaration {
                        function_name: "print".to_string(),
                        function_parameters: vec![("".to_string(), RueType::I32)],
                        function_return_type: RueType::Unit,
                        is_external_function: true,
                        body: None,
                    },
                    Statement::FunctionDeclaration {
                        function_name: "test_func".to_string(),
                        function_parameters: vec![
                            ("a".to_string(), RueType::I32),
                            ("b".to_string(), RueType::I32),
                        ],
                        function_return_type: RueType::I32,
                        is_external_function: false,
                        body: Some(CodeBlock {
                            statements: vec![Statement::Return(Expression::BinaryOperation(
                                Operator::Add,
                                Box::new(Expression::Variable("a".to_string())),
                                Box::new(Expression::Variable("b".to_string())),
                            ))],
                        }),
                    },
                ],
            },
            "Parse multiple function declarations",
        );
    }
}
