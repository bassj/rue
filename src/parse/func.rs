use crate::{
    ast::{Expression, Statement},
    types::RueType,
};

use super::{
    error::ErrorStack,
    util::{self, parse_identifier, parse_type, parse_type_tag, ws},
    IResult, InputType,
};

fn parse_function_arguments(input: InputType) -> IResult<Vec<Expression>> {
    nom::sequence::delimited(
        nom::character::complete::char('('),
        nom::multi::separated_list0(
            nom::character::complete::char(','),
            nom_preserve::error::discard(super::expr::parse_expression),
        ),
        nom::character::complete::char(')'),
    )(input)
}

pub fn parse_function_invocation(input: InputType) -> IResult<Expression> {
    nom::combinator::map(
        nom::sequence::tuple((util::parse_identifier, parse_function_arguments)),
        |(func_name, func_args)| Expression::FunctionInvocation(func_name, func_args),
    )(input)
}

pub fn parse_function_declaration(input: InputType) -> IResult<(Statement, ErrorStack)> {
    fn _parse_function_declaration(input: InputType) -> IResult<Statement> {
        let (input, (_, function_name, function_parameters, return_type)) =
            nom::sequence::tuple((
                ws(nom::bytes::complete::tag("fn")),
                ws(parse_identifier),
                nom::sequence::delimited(
                    nom::character::complete::char('('),
                    nom::multi::separated_list0(
                        ws(nom::character::complete::char(',')),
                        nom::sequence::tuple((ws(parse_identifier), parse_type_tag)),
                    ),
                    nom::character::complete::char(')'),
                ),
                nom::combinator::opt(nom::sequence::preceded(
                    // TODO: probably refactor this into a parse function return type tag
                    ws(nom::bytes::complete::tag("->")),
                    parse_type,
                )),
            ))(input)?;

        let function_return_type = match return_type {
            Some(return_type) => return_type,
            None => RueType::Unit,
        };

        let stmt = Statement::FunctionDeclaration {
            function_name,
            function_parameters,
            function_return_type,
            is_external_function: false,
        };

        Ok((input, stmt))
    }

    fn _parse_external_function_declaration(input: InputType) -> IResult<Statement> {
        let (input, (_, function_name, function_parameters, return_type)) =
            nom::sequence::tuple((
                ws(nom::bytes::complete::tag("fn")),
                ws(parse_identifier),
                nom::sequence::delimited(
                    nom::character::complete::char('('),
                    nom::multi::separated_list0(
                        ws(nom::character::complete::char(',')),
                        nom::branch::alt((
                            nom::combinator::map(parse_type, |rue_type| (String::new(), rue_type)),
                        )),
                    ),
                    nom::character::complete::char(')'),
                ),
                nom::combinator::opt(nom::sequence::preceded(
                    // TODO: probably refactor this into a parse function return type tag
                    ws(nom::bytes::complete::tag("->")),
                    parse_type,
                )),
            ))(input)?;

        let function_return_type = match return_type {
            Some(return_type) => return_type,
            None => RueType::Unit,
        };

        let stmt = Statement::FunctionDeclaration {
            function_name,
            function_parameters,
            function_return_type,
            is_external_function: true,
        };

        Ok((input, stmt))
    }

    let (input, is_extern) = nom::combinator::opt(ws(nom::bytes::complete::tag("extern")))(input)?;

    let error_stack = Vec::new();

    let (input, stmt) = if is_extern.is_some() {
        _parse_external_function_declaration(input)?
    } else {
        _parse_function_declaration(input)?
    };

    Ok((input, (stmt, error_stack)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Statement, types::RueInteger};
    use nom_locate::LocatedSpan;

    #[test]
    fn test_parse_function_declaration() {
        fn _test_parse_function_declaration(
            input: &str,
            expected_output_str: &str,
            expected_output: Statement,
            message: &str,
        ) {
            let input = LocatedSpan::new(input);
            let parser_res = parse_function_declaration(input);

            assert!(
                parser_res.is_ok(),
                "parse_function_declaration returns no error - {}:\n{}",
                message,
                parser_res.unwrap_err()
            );

            let (input, (stmt, _error_stack)) = parser_res.unwrap();

            assert_eq!(
                input.fragment(),
                &expected_output_str,
                "parse_function_declaration returns correct input - {}",
                message
            );
            assert_eq!(
                stmt, expected_output,
                "parse_function_declaration returns correct statement - {}",
                message
            );
        }

        _test_parse_function_declaration(
            "fn test()",
            "",
            Statement::FunctionDeclaration {
                function_name: "test".to_string(),
                function_parameters: Vec::new(),
                function_return_type: RueType::Unit,
                is_external_function: false,
            },
            "Test parse simple function",
        );

        _test_parse_function_declaration(
            "extern fn test()",
            "",
            Statement::FunctionDeclaration {
                function_name: "test".to_string(),
                function_parameters: Vec::new(),
                function_return_type: RueType::Unit,
                is_external_function: true,
            },
            "Test parse simple external function",
        );

        _test_parse_function_declaration(
            "extern fn test(i32, i32)",
            "",
            Statement::FunctionDeclaration {
                function_name: "test".to_string(),
                function_parameters: vec![
                    (
                        "".to_string(),
                        RueType::Integer {
                            bit_width: 32,
                            signed: true,
                        },
                    ),
                    (
                        "".to_string(),
                        RueType::Integer {
                            bit_width: 32,
                            signed: true,
                        },
                    ),
                ],
                function_return_type: RueType::Unit,
                is_external_function: true,
            },
            "Test parse external function with parameters",
        );

        _test_parse_function_declaration(
            "fn test(test: i32, test_two: i32)",
            "",
            Statement::FunctionDeclaration {
                function_name: "test".to_string(),
                function_parameters: vec![
                    (
                        "test".to_string(),
                        RueType::Integer {
                            bit_width: 32,
                            signed: true,
                        },
                    ),
                    (
                        "test_two".to_string(),
                        RueType::Integer {
                            bit_width: 32,
                            signed: true,
                        },
                    ),
                ],
                function_return_type: RueType::Unit,
                is_external_function: false,
            },
            "Test parse external function with named parameters",
        );

        _test_parse_function_declaration(
            "fn test(test_param: i32) -> i32",
            "",
            Statement::FunctionDeclaration {
                function_name: "test".to_string(),
                function_parameters: vec![(
                    "test_param".to_string(),
                    RueType::Integer {
                        bit_width: 32,
                        signed: true,
                    },
                )],
                function_return_type: RueType::Integer {
                    bit_width: 32,
                    signed: true,
                },
                is_external_function: false,
            },
            "Test parse function with argument and return type",
        );

        // TODO: later
        // _test_parse_function_declaration(
        //     "fn test(test_param: i32, test_param2: i32)",
        //     "",
        //     Statement::FunctionDeclaration {
        //         function_name: "test".to_string(),
        //         function_parameters: vec![(
        //             "test_param".to_string(),
        //             RueType::Integer {
        //                 bit_width: 32,
        //                 signed: true,
        //             },
        //         )],
        //         function_return_type: RueType::Integer {
        //             bit_width: 32,
        //             signed: true,
        //         },
        //         is_external_function: false,
        //     },
        //     "Test parse function with multiple arguments"
        // );
    }

    #[test]
    fn test_parse_function_arguments() {
        let input = LocatedSpan::new("()");
        let (input, func_args) = parse_function_arguments(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returned correct input");

        assert_eq!(
            func_args,
            Vec::new(),
            "Parser returned the correct function arguments"
        );

        let input = LocatedSpan::new("( 1 , 2 , 3 , 4 )");
        let (input, func_args) = parse_function_arguments(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returned correct input");

        assert_eq!(
            func_args,
            vec![
                Expression::Literal(RueInteger::from(1).into()),
                Expression::Literal(RueInteger::from(2).into()),
                Expression::Literal(RueInteger::from(3).into()),
                Expression::Literal(RueInteger::from(4).into()),
            ],
            "Parser returned the correct function arguments"
        );
    }

    #[test]
    pub fn test_parse_function_invocation() {
        use nom_locate::LocatedSpan;

        let input = LocatedSpan::new("test_function()");
        let (input, func_invoc) = parse_function_invocation(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returned the correct input");

        assert_eq!(
            func_invoc,
            Expression::FunctionInvocation(String::from("test_function"), vec![]),
            "Parser returns the correct function invocation"
        );
    }
}
