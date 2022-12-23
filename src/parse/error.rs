use nom::{Err, IResult};
use nom_locate::LocatedSpan;

use colored::Colorize;

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

    pub fn error_message(&self) -> impl std::fmt::Display {
        let root_err = match self {
            ParseError::Stack { root, context: _ } => root.as_ref(),
            e => e,
        };

        match root_err {
            ParseError::Root { location: _, kind } => match kind {
                ErrorContext::Expectation { expected } => format!("expected {}", expected),
                _ => unimplemented!(),
            },
            _ => panic!("root error should not be anything except ParseError::Root"),
        }
    }

    pub fn error_message_long(&self) -> impl std::fmt::Display {
        let short_msg = self.error_message();

        let root_err = match self {
            ParseError::Stack { root, context: _ } => root.as_ref(),
            e => e,
        };

        match root_err {
            ParseError::Root { location, kind } => match kind {
                ErrorContext::Expectation { expected: _ } => {
                    let found = match location.fragment().chars().next() {
                        Some(c) => format!("`{}`", c),
                        _ => "``".to_string(),
                    };
                    format!("{}, found {}", short_msg, found)
                }
                _ => short_msg.to_string(),
            },
            _ => panic!("root error should not be anything except ParseError::Root"),
        }
    }

    pub fn error_header(&self) -> impl std::fmt::Display {
        format!("{}: {}", "error".red(), self.error_message_long())
            .bold()
            .white()
    }

    pub fn code_highlight(&self) -> String {
        let short_msg = self.error_message();

        let root_err = match self {
            ParseError::Stack { root, context: _ } => root.as_ref(),
            e => e,
        };

        match root_err {
            ParseError::Root { location, kind: _ } => {
                let line_number = location.location_line().to_string();
                let width = line_number.len() + 1;
                let empty_header = format!("{:w$}|", "", w = width).bold().bright_cyan();
                let line_header = format!("{:w$}|", line_number, w = width)
                    .bold()
                    .bright_cyan();
                let newline = "\r\n";

                let code = std::str::from_utf8(location.get_line_beginning())
                    .expect("Source must be utf8 encoded");
                let code = format!("\t{}", code);

                let offset = location.get_utf8_column();
                let error_highlight = format!("\t{:>offset$} {}", "^", short_msg, offset = offset)
                    .bold()
                    .red();

                format!(
                    "{}{}{}{}{}{}{}",
                    empty_header,
                    newline,
                    line_header,
                    code,
                    newline,
                    empty_header,
                    error_highlight
                )
            }
            _ => panic!("root error should not be anything except ParseError::Root"),
        }
    }
}

impl std::fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let newline = "\r\n";
        write!(
            f,
            "{}{}{}",
            self.error_header(),
            newline,
            self.code_highlight()
        )
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
