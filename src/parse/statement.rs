use crate::ast::Statement;
use crate::parse::error::ErrorStack;
use crate::parse::util;
use crate::parse::IResult;
use crate::parse::InputType;
use crate::types::RueType;

use super::util::parse_identifier;
use super::util::parse_type;
use super::util::parse_type_tag;
use super::util::ws;

pub fn parse_statement(input: InputType) -> IResult<Statement> {
    let t = nom_preserve::sequence::delimited(
        nom::character::complete::multispace0,
        nom_preserve::error::blame(nom::branch::alt((
            nom::combinator::map(super::expr::parse_expression, |(expr, err_stack)| {
                (Statement::Expression(expr), err_stack)
            }),
            parse_variable_declaration,
            parse_function_declaration,
        ))),
        nom::branch::alt((
            nom::sequence::terminated(
                nom::character::complete::line_ending,
                nom::character::complete::multispace0,
            ),
            nom::combinator::eof,
        )),
    )(input);

    t.map(|(i, (expr, _err_stack))| (i, expr))
}

fn parse_variable_declaration(input: InputType) -> IResult<(Statement, ErrorStack)> {
    let (input, ((var_name, var_type), (var_value, error_stack))) = nom::sequence::preceded(
        util::ws(nom::bytes::complete::tag("let")),
        nom::sequence::separated_pair(
            nom::sequence::tuple((
                super::util::parse_identifier,
                nom::combinator::opt(super::util::parse_type_tag),
            )),
            util::ws(nom::bytes::complete::tag("=")),
            super::expr::parse_expression,
        ),
    )(input)?;

    let stmt = Statement::VariableDeclaration(var_name, var_type, var_value);

    Ok((input, (stmt, error_stack)))
}

// TODO: probably move this to the func parse module, then just call it from parse_statement
// TODO #2: when declaring external functions, we probably don't actually need to include the param names
fn parse_function_declaration(input: InputType) -> IResult<(Statement, ErrorStack)> {
    let (input, (is_extern, _, function_name, function_parameters, return_type)) =
        nom::sequence::tuple((
            nom::combinator::opt(ws(nom::bytes::complete::tag("extern"))),
            ws(nom::bytes::complete::tag("fn")),
            ws(parse_identifier),
            nom::sequence::delimited(
                nom::character::complete::char('('),
                nom::multi::many0(nom::sequence::tuple((ws(parse_identifier), parse_type_tag))),
                nom::character::complete::char(')'),
            ),
            nom::combinator::opt(nom::sequence::preceded(
                // TODO: probably refactor this into a parse function return type tag
                ws(nom::bytes::complete::tag("->")),
                parse_type,
            )),
        ))(input)?;

    let is_external_function = is_extern.is_some();

    let function_return_type = match return_type {
        Some(return_type) => return_type,
        None => RueType::Void,
    };

    let stmt = Statement::FunctionDeclaration {
        function_name,
        function_parameters,
        function_return_type,
        is_external_function,
    };

    let error_stack = Vec::new();

    Ok((input, (stmt, error_stack)))
}

#[cfg(test)]
mod tests {
    use crate::types::RueInteger;

    use super::*;
    use crate::ast::Expression;
    use crate::types::RueType;
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
            let (input, (stmt, _error_stack)) = parse_function_declaration(input).unwrap();

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
                function_return_type: RueType::Void,
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
                function_return_type: RueType::Void,
                is_external_function: true,
            },
            "Test parse simple external function",
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
    fn test_parse_variable_declaration() {
        let input = LocatedSpan::new("let test = 10");
        let (input, (stmt, _error_stack)) = parse_variable_declaration(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returns correct input");

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test".to_string(),
                None,
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );
    }

    #[test]
    fn test_parse_variable_declaration_type_tag() {
        use crate::ast::Expression;

        let input = LocatedSpan::new("let test:u64 = 10");
        let (input, (stmt, _error_stack)) = parse_variable_declaration(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returns correct input");

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test".to_string(),
                Some(RueType::Integer {
                    bit_width: 64,
                    signed: false
                }),
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );

        let input = LocatedSpan::new("let test : u64 = 10");
        let (input, (stmt, _error_stack)) = parse_variable_declaration(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returns correct input");

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test".to_string(),
                Some(RueType::Integer {
                    bit_width: 64,
                    signed: false
                }),
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );
    }

    #[test]
    fn test_parse_statement_variable_declaration() {
        let input = LocatedSpan::new("let test = 10\nlet test2 = 10");
        let (input, stmt) = parse_statement(input).unwrap();

        assert_eq!(
            input.fragment(),
            &"let test2 = 10",
            "Parser returns correct input"
        );

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test".to_string(),
                None,
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );

        let (input, stmt) = parse_statement(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returns correct input");

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test2".to_string(),
                None,
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );

        let input = LocatedSpan::new("let test = 10\r\nlet test2 = 10");
        let (input, stmt) = parse_statement(input).unwrap();

        assert_eq!(
            input.fragment(),
            &"let test2 = 10",
            "Parser returns correct input"
        );

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test".to_string(),
                None,
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );

        let (input, stmt) = parse_statement(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returns correct input");

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test2".to_string(),
                None,
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );
    }

    #[test]
    fn test_parse_statement_variable_declaration_with_type_tag() {
        let input = LocatedSpan::new("let test: u64 = 10");
        let (input, stmt) = parse_statement(input).unwrap();

        assert_eq!(input.fragment(), &"", "Parser returns correct input");

        assert_eq!(
            stmt,
            Statement::VariableDeclaration(
                "test".to_string(),
                Some(RueType::Integer {
                    bit_width: 64,
                    signed: false
                }),
                Expression::Literal(RueInteger::from(10).into())
            ),
            "Parser parses correct statement"
        );
    }
}
