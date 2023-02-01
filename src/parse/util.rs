use crate::types::RueType;

use super::InputType;
use nom::{error::ParseError, IResult, Parser};

/// Eats whitespace before and after a parser.
/// Will eat 0 or more tabs or spaces, does not eat newlines or carriage returns
pub fn ws<'p, O, E: ParseError<InputType<'p>>, F>(
    f: F,
) -> impl FnMut(InputType<'p>) -> IResult<InputType<'p>, O, E>
where
    F: Parser<InputType<'p>, O, E>,
{
    let eat_whitespace = |i: InputType<'p>| {
        nom::multi::many0(nom::branch::alt((
            nom::character::complete::char(' '),
            nom::character::complete::char('\t'),
        )))(i)
    };

    nom::sequence::delimited(eat_whitespace, f, eat_whitespace)
}

// list of keywords
// let, fn, extern

fn parse_keyword(input: InputType) -> crate::parse::IResult<String> {
    nom::combinator::map(
        nom::branch::alt((
            nom::bytes::complete::tag("let"),
            nom::bytes::complete::tag("fn"),
            nom::bytes::complete::tag("extern"),
        )),
        |tag: InputType| tag.fragment().to_string(),
    )(input)
}

pub fn parse_identifier(input: InputType) -> crate::parse::IResult<String> {
    let (input, ((), (first_part, second_part))) = nom::combinator::not(parse_keyword)
        .and(nom::sequence::tuple((
            nom::bytes::complete::take_while1(|c: char| c.is_alphabetic() || c == '_'),
            nom::bytes::complete::take_while(|c: char| c.is_alphanumeric() || c == '_'),
        )))
        .parse(input)?;

    let mut function_name = first_part.to_string();

    function_name.extend(second_part.chars());

    Ok((input, function_name))
}

pub fn parse_type(input: InputType) -> crate::parse::IResult<RueType> {
    nom::combinator::map(ws(parse_identifier), |type_name| {
        RueType::from(type_name.as_str())
    })(input)
}

// TODO: Implement and use a proper type parser.
pub fn parse_type_tag(input: InputType) -> crate::parse::IResult<RueType> {
    let (input, rue_type) =
        nom::sequence::preceded(ws(nom::bytes::complete::tag(":")), parse_type)(input)?;

    Ok((input, rue_type))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom_locate::LocatedSpan;

    #[test]
    fn test_ws_eats() {
        let inner_parser = nom::character::complete::alpha1::<InputType, crate::parse::ParseError>;

        let input = LocatedSpan::new("  test  ");
        let (input, tag) = ws(inner_parser)(input).unwrap();

        assert_eq!(input.fragment(), &"", "parser returns correct input");
        assert_eq!(tag.fragment(), &"test", "parser returns correct tag");

        let input = LocatedSpan::new("      test        ");
        let (input, tag) = ws(inner_parser)(input).unwrap();

        assert_eq!(input.fragment(), &"", "parser returns correct input");
        assert_eq!(tag.fragment(), &"test", "parser returns correct tag");
    }

    #[test]
    fn test_ws_doesnt_eat() {
        let inner_parser = nom::character::complete::alpha1::<InputType, crate::parse::ParseError>;

        let input = LocatedSpan::new("      test        \r\n");
        let (input, tag) = ws(inner_parser)(input).unwrap();

        assert_eq!(input.fragment(), &"\r\n", "parser returns correct input");
        assert_eq!(tag.fragment(), &"test", "parser returns correct tag");

        let input = LocatedSpan::new("      test        \n");
        let (input, tag) = ws(inner_parser)(input).unwrap();

        assert_eq!(input.fragment(), &"\n", "parser returns correct input");
        assert_eq!(tag.fragment(), &"test", "parser returns correct tag");
    }

    #[test]
    fn test_parse_identifier() {
        use nom_locate::LocatedSpan;

        let input = LocatedSpan::new("test_function()");
        let (input, func_name) = parse_identifier(input).unwrap();

        assert_eq!(
            input.fragment(),
            &"()",
            "Correctly parsed the full function name"
        );

        assert_eq!(
            &func_name, &"test_function",
            "Parser returned the correct function name"
        );

        let input = LocatedSpan::new("test_function1()");
        let (input, func_name) = parse_identifier(input).unwrap();

        assert_eq!(
            input.fragment(),
            &"()",
            "Correctly parsed the full function name"
        );

        assert_eq!(
            &func_name, &"test_function1",
            "Parser returned the correct function name"
        );

        let input = LocatedSpan::new("test1_function()");
        let (input, func_name) = parse_identifier(input).unwrap();

        assert_eq!(
            input.fragment(),
            &"()",
            "Correctly parsed the full function name"
        );

        assert_eq!(
            &func_name, &"test1_function",
            "Parser returned the correct function name"
        );

        let input = LocatedSpan::new("_test()");
        let (input, func_name) = parse_identifier(input).unwrap();

        assert_eq!(
            input.fragment(),
            &"()",
            "Correctly parsed the full function name"
        );

        assert_eq!(
            &func_name, &"_test",
            "Parser returned the correct function name"
        );
    }
}
