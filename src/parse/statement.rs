use crate::ast::Statement;
use crate::parse::error::ErrorStack;
use crate::parse::util;
use crate::parse::IResult;
use crate::parse::InputType;

use super::func::parse_function_declaration;

pub fn parse_statement(input: InputType) -> IResult<Statement> {
    let t = nom_preserve::sequence::delimited(
        nom::character::complete::multispace0,
        nom_preserve::error::blame(nom::branch::alt((
            parse_expression_as_statement,
            parse_variable_declaration,
            parse_function_declaration,
            parse_return_statement,
        ))),
        nom::branch::alt((nom::character::complete::multispace0, nom::combinator::eof)),
    )(input);

    t.map(|(i, (expr, _err_stack))| (i, expr))
}

fn parse_expression_as_statement(input: InputType) -> IResult<(Statement, ErrorStack)> {
    return nom::combinator::map(super::expr::parse_expression, |(expr, err_stack)| {
        (Statement::Expression(expr), err_stack)
    })(input);
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

fn parse_return_statement(input: InputType) -> IResult<(Statement, ErrorStack)> {
    let (input, (returned_expr, error_stack)) = nom::sequence::preceded(
        util::ws(nom::bytes::complete::tag("return")),
        super::expr::parse_expression,
    )(input)?;

    let stmt = Statement::Return(returned_expr);

    Ok((input, (stmt, error_stack)))
}

#[cfg(test)]
mod tests {
    use crate::types::RueInteger;

    use super::*;
    use crate::ast::{CodeBlock, Expression, Operator};
    use crate::types::RueType;
    use nom_locate::LocatedSpan;

    fn _test_parse_statement(
        input: &str,
        expected_output_str: &str,
        expected_output: Statement,
        message: &str,
    ) {
        let input = LocatedSpan::new(input);
        let parser_res = parse_statement(input);

        assert!(
            parser_res.is_ok(),
            "parse_statement returns no error - {}:\n{:?}",
            message,
            parser_res.unwrap_err()
        );

        let (input, stmt) = parser_res.unwrap();

        assert_eq!(
            input.fragment(),
            &expected_output_str,
            "parse_statement returns correct input - {}",
            message
        );
        assert_eq!(
            stmt, expected_output,
            "parse_statement returns correct statement - {}",
            message
        );
    }

    #[test]
    fn test_parse_variable_declaration() {
        _test_parse_statement(
            "let test = 10",
            "",
            Statement::VariableDeclaration(
                "test".to_string(),
                None,
                Expression::Literal(RueInteger::from(10).into()),
            ),
            "Parse simple variable declaration",
        );

        _test_parse_statement(
            "let test:u64 = 10",
            "",
            Statement::VariableDeclaration(
                "test".to_string(),
                Some(RueType::U64),
                Expression::Literal(RueInteger::from(10).into()),
            ),
            "Parse variable declaration with explicit type info",
        );

        _test_parse_statement(
            "let test = 10\nlet test2 = 10",
            "let test2 = 10",
            Statement::VariableDeclaration(
                "test".to_string(),
                None,
                Expression::Literal(RueInteger::from(10).into()),
            ),
            "Parse variable declaration eats line return",
        );
    }

    #[test]
    fn test_parse_function_declaration() {
        _test_parse_statement(r#"
            extern fn print(i32)

            fn test_func(a: i32, b: i32) -> i32 {
                return a + b
            }
        "#,
        "fn test_func(a: i32, b: i32) -> i32 {\n                return a + b\n            }\n        ",
        Statement::FunctionDeclaration {
            function_name: "print".to_string(),
            function_parameters: vec![("".to_string(), RueType::I32)],
            function_return_type: RueType::Unit,
            is_external_function: true,
            body: None,
        },
        "Parse external function declaration");

        _test_parse_statement(
        "fn test_func(a: i32, b: i32) -> i32 {\n                return a + b\n            }\n        ",
        "",
        Statement::FunctionDeclaration {
            function_name: "test_func".to_string(),
            function_parameters: vec![("a".to_string(), RueType::I32), ("b".to_string(), RueType::I32)],
            function_return_type: RueType::I32,
            is_external_function: false,
            body: Some(CodeBlock {
                statements: vec![
                    Statement::Return(Expression::BinaryOperation(Operator::Add, Box::new(Expression::Variable("a".to_string())), Box::new(Expression::Variable("b".to_string()))))
                ]
            }),
        },
        "Parse function declaration");
    }
}
