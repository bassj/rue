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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::ParseError;
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

        assert_eq!(
            input.fragment(),
            &"\r\n",
            "parser returns correct input"
        );
        assert_eq!(tag.fragment(), &"test", "parser returns correct tag");

        let input = LocatedSpan::new("      test        \n");
        let (input, tag) = ws(inner_parser)(input).unwrap();

        assert_eq!(
            input.fragment(),
            &"\n",
            "parser returns correct input"
        );
        assert_eq!(tag.fragment(), &"test", "parser returns correct tag");
    }
}
