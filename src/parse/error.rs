use nom::{Err, IResult};
use nom_locate::LocatedSpan;

use super::InputType;

pub trait Expectable {
    fn expectation_name() -> &'static str;
}

#[derive(Debug)]
pub enum Expectation {
    Eof,
    Custom(&'static str),
}

impl std::fmt::Display for Expectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Expectation::Eof => write!(f, "eof"),
            Expectation::Custom(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug)]
pub enum ErrorContext {
    Expectation { expected: Expectation },
    Kind(nom::error::ErrorKind),
    Context(&'static str),
}

#[derive(Debug)]
pub enum ParseError<'s> {
    Root {
        location: InputType<'s>,
        kind: ErrorContext,
    },
    Stack {
        root: Box<Self>,
        context: Vec<(InputType<'s>, ErrorContext)>,
    },
}

impl<'s> ParseError<'s> {
    pub fn location(&self) -> &InputType<'s> {
        match &self {
            &ParseError::Root { location, kind: _ } => location,
            &ParseError::Stack { root, context: _ } => root.location(),
        }
    }
}

impl<'s> nom::error::ParseError<InputType<'s>> for ParseError<'s> {
    fn from_error_kind(input: InputType<'s>, kind: nom::error::ErrorKind) -> Self {
        Self::Root {
            location: input,
            kind: ErrorContext::Kind(kind),
        }
    }

    fn append(input: InputType<'s>, kind: nom::error::ErrorKind, other: Self) -> Self {
        let ctx = (input, ErrorContext::Kind(kind));

        match other {
            Self::Stack { root, mut context } => Self::Stack {
                root,
                context: {
                    context.push(ctx);
                    context
                },
            },
            root => Self::Stack {
                root: Box::new(root),
                context: vec![ctx],
            },
        }
    }
}

impl<'s> nom::error::ContextError<InputType<'s>> for ParseError<'s> {
    fn add_context(input: InputType<'s>, ctx: &'static str, other: Self) -> Self {
        let ctx = (input, ErrorContext::Context(ctx));

        match other {
            Self::Stack { root, mut context } => Self::Stack {
                root,
                context: {
                    context.push(ctx);
                    context
                },
            },
            root => Self::Stack {
                root: Box::new(root),
                context: vec![ctx],
            },
        }
    }
}

impl<'s> nom::error::FromExternalError<InputType<'s>, Box<dyn std::error::Error>>
    for ParseError<'s>
{
    fn from_external_error(
        input: InputType<'s>,
        kind: nom::error::ErrorKind,
        _e: Box<dyn std::error::Error>,
    ) -> Self {
        Self::Root {
            location: input,
            kind: ErrorContext::Kind(kind),
        }
    }
}

impl<'s> Default for ParseError<'s> {
    fn default() -> Self {
        Self::Root {
            location: LocatedSpan::new(""),
            kind: ErrorContext::Kind(nom::error::ErrorKind::Fail),
        }
    }
}

pub fn convert_error(err: ParseError) -> String {
    match err {
        ParseError::Stack { root, context: _ } => match root.as_ref() {
            ParseError::Root { location, kind } => {
                let expectation = match kind {
                    ErrorContext::Expectation { expected } => format!("expected {}", expected),
                    ErrorContext::Kind(_) => todo!(),
                    ErrorContext::Context(_) => todo!(),
                };

                let found = location.fragment().chars().next();
                let found = format!("`{}`", if found.is_none() { 0 as char } else { found.unwrap() });

                let line_number = location.location_line();
                let column_number = location.get_utf8_column();

                let line_number = format!("{} |", line_number);

                let location = std::str::from_utf8(location.get_line_beginning())
                    .expect("Rue can only parse valid utf8");

                let location = format!("{}\t{}", line_number, location);
                let location = format!("{}\r\n\t{:>offset$} {}", location, "^", expectation, offset = column_number);

                format!("error: {}, found: {}\n{}", expectation, found, location)
            }
            _ => panic!("Stack root should always be of type ParseError::Root"),
        },
        _ => String::new(),
    }
}

pub fn expect<'s, I, O, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, O, ParseError<'s>>
where
    O: Expectable,
    F: nom::Parser<I, O, ParseError<'s>>,
{
    move |input: I| match parser.parse(input) {
        Err(err) => {
            let err: ParseError = match err {
                Err::Error(e) => e,
                Err::Failure(e) => e,
                _ => panic!("Unexpected, should not be incomplete"),
            };

            let kind = ErrorContext::Expectation {
                expected: Expectation::Custom(O::expectation_name()),
            };

            Err(Err::Failure(ParseError::Root {
                location: *err.location(),
                kind,
            }))
        }
        rest => rest,
    }
}

impl Expectable for crate::ast::Expression {
    fn expectation_name() -> &'static str {
        "expression"
    }
}
