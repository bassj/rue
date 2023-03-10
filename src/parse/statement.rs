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


#[cfg(test)]
mod tests {
    use crate::types::RueInteger;

    use super::*;
    use crate::ast::Expression;
    use crate::types::RueType;
    use nom_locate::LocatedSpan;

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
