use thiserror::Error;
use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, Error)]
pub enum AsmErr<'a> {
    #[error("bad opcode: {0}")]
    BadOpcode(&'a str),
    #[error("bad operand")]
    BadOperand,
    #[error("got {got}, expected {expected}")]
    BadOperandNum{ got: usize, expected: usize },
    #[error("symbol {0:?} defined multiple times")]
    DoubleLabel(&'a str),
    #[error("bad integer literal: {0}")]
    BadInt(&'a str),
    #[error("bad register name: {0}")]
    BadReg(&'a str),
    #[error("bad ident name: {0}")]
    BadIdent(&'a str),
    #[error("relative jump too long")]
    JumpTooLong,
    #[error("undefined symbol: {0}")]
    UndefinedSymbol(&'a str),
    #[error("unaligned jump")]
    UnalignedJump
}
pub type Result<'a, T> = std::result::Result<T, AsmErr<'a>>;

pub fn parse_int_literal<'a>(lit: &'a str) -> Result<'a, u16> {
    if let Ok(i) = lit.parse::<u16>() {
        Ok(i)
    }
    else {
        if lit.len() < 3 {
            Err(AsmErr::BadInt(lit))
        }
        else {
            let radix = match &lit[0..2] {
                "0x" => 16,
                "0d" => 10,
                "0o" => 8,
                "0b" => 2,
                _ => return Err(AsmErr::BadInt(lit))
            };
            if let Ok(i) = u16::from_str_radix(&lit[2..], radix) {
                Ok(i)
            }
            else {
                Err(AsmErr::BadInt(lit))
            }
        }
    }
}

lazy_static! {
    static ref REGISTER_NAMES: HashMap<&'static str, u8> = {
        let mut h = HashMap::new();
        h.insert("pc", 15);
        h.insert("link", 14);
        h.insert("sp", 13);
        h.insert("0", 0);
        h
    };
}

pub fn parse_reg<'a>(r: &'a str) -> Result<'a, u8> {
    if r.starts_with('r') {
        if let Ok(i) = r[1..].parse::<u8>() {
            if i > 15 {
                Err(AsmErr::BadReg(r))
            }
            else {
                Ok(i)
            }
        }
        else {
            Err(AsmErr::BadReg(r))
        }
    }
    else {
        REGISTER_NAMES.get(r).ok_or(AsmErr::BadReg(r)).map(|i| *i)
    }
}

pub struct ExportOptions {
    format: ExportFormat
}
impl ExportOptions {
    pub fn new() -> ExportOptions {
        ExportOptions {
            format: ExportFormat::BareMetal
        }
    }

    pub fn format(mut self, f: ExportFormat) -> Self {
        self.format = f;
        self
    }
}

pub enum ExportFormat {
    BareMetal
}
