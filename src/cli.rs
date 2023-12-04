use std::{fmt::Display, io, error::Error};

use crate::Config;

pub(crate) struct Input {
    pub config: Config
}

#[derive(Debug)]
pub(crate) enum CliError {
    UnknownDirective(Box<str>),
    MissingFlagArgument,
    IoError(io::Error),
}

impl From<io::Error> for CliError {
    fn from(v: io::Error) -> Self {
        Self::IoError(v)
    }
}

impl Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliError::UnknownDirective(d) => write!(f, "Unknown Directive {d:?}"),
            CliError::MissingFlagArgument => "Missing flag argument".fmt(f),
            CliError::IoError(e) => e.fmt(f),
        }
    }
}

impl Error for CliError { }

fn parse_flag(arg: Box<str>, config: &mut Config) -> Result<(), CliError> {
    match &*arg {
        "-g" => config.emit_debug_syms = true,
        "-v" => config.emit_comments = true,
        _ => return Err(CliError::UnknownDirective(arg))
    };
    Ok(())
}

pub(crate) fn read_args() -> Result<Input, CliError> {
    let mut args_iter = std::env::args();
    // exec name
    args_iter.next();

    let mut config = Config::new();
    while let Some(arg) = args_iter.next() {
        if arg.starts_with("-") {
            parse_flag(arg.into(), &mut config)?;
        } else {
            config.input_files.push(arg.into())
        }
    }

    Ok(Input { config })
}
