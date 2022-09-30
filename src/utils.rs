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
    UnalignedJump,
    #[error("bad directive: {0}")]
    BadDirective(&'a str),
    #[error("org not at start of program")]
    OrgNotAtAtStart,
    #[error("org/spaceto not word-aligned")]
    UnalignedOrg,
    #[error("spaceto to previous address")]
    BackwardsSpaceto
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


pub trait CollectUntil<T, F> {
    fn collect_until(&mut self, predicate: F) -> Vec<T>;
}
impl<T, F: FnMut(&T) -> bool, I: Iterator<Item = T>> CollectUntil<T, F> for I {
    fn collect_until(&mut self, predicate: F) -> Vec<T> {
        let mut predicate = predicate;
        let mut ret = Vec::new();
        while let Some(item) = self.next() {
            if predicate(&item) {
                break
            }
            else {
                ret.push(item)
            }
        }
        ret
    }
}

pub struct InsertableIter<I> {
    iters: std::collections::VecDeque<I>
}
impl<I: Iterator> InsertableIter<I> {
    pub fn new() -> Self {
        Self {
            iters: std::collections::VecDeque::new()
        }
    }
    pub fn insert(&mut self, new: I) {
        self.iters.push_front(new)
    }
    pub fn append(&mut self, new: I) {
        self.iters.push_back(new)
    }
}
impl<I: Iterator<Item = T>, T> Iterator for InsertableIter<I> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let current = self.iters.get_mut(0)?;
            let next = current.next();
            if next.is_some() {
                break next
            }
            else {
                self.iters.pop_front();
            }
        }
    }
}

pub enum LineIter<'a> {
    FromSource(std::iter::Enumerate<std::str::Lines<'a>>),
    FromMacro(usize, std::str::Lines<'a>)
}
impl<'a> Iterator for LineIter<'a> {
    type Item = (usize, &'a str);
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::FromMacro(line, iter) => iter.next().map(|i| (*line, i)),
            Self::FromSource(iter) => iter.next()
        }
    }
}
pub type CodeIter<'a> = InsertableIter<LineIter<'a>>;
