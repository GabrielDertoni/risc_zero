use std::fmt::Display;

use crate::parser::ParserRule;

#[derive(Debug)]
pub enum Error {
    Parser(pest::error::Error<ParserRule>),
    Io(std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! error {
    ($msg:expr, $span:expr) => {
        Err(
            pest::error::Error::new_from_span(
                pest::error::ErrorVariant::CustomError {
                    message: $msg.into()
                },
                $span
            ).into()
        )
    };
}

impl From<pest::error::Error<ParserRule>> for Error {
    #[inline]
    fn from(err: pest::error::Error<ParserRule>) -> Error {
        Error::Parser(err)
    }
}

impl From<std::io::Error> for Error {
    #[inline]
    fn from(err: std::io::Error) -> Error {
        Error::Io(err)
    }
}

impl Display for Error {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err: &dyn Display = match self {
            Error::Parser(err) => err,
            Error::Io(err)     => err,
        };
        err.fmt(f)
    }
}
