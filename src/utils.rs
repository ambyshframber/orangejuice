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
    BackwardsSpaceto,
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


// PREPROCESSOR UTILS START HERE

pub type PreResult<T> = std::result::Result<T, MacErr>;

#[derive(Debug, PartialEq, Error)]
pub enum MacErr {
    #[error("bad arguments")]
    BadArgs,
    #[error("invalid replacement escape")]
    InvalidRepEscape,
    #[error("invalid macro decl: {0}")]
    InvalidDecl(String)
}

pub struct SkipUntil<I, F> {
    inner: I,
    finished_skip: bool,
    f: F
}
impl<Iter, Item, F> Iterator for SkipUntil<Iter, F>
where Iter: Iterator<Item = Item>, F: FnMut(&Item) -> bool {
    type Item = Item;
    fn next(&mut self) -> Option<Item> {
        loop {
            let next = self.inner.next()?;
            if self.finished_skip {
                break Some(next)
            }
            else {
                if (self.f)(&next) {
                    self.finished_skip = true;
                    break Some(next)
                }
            }
        }
    }
}
pub trait SkipUntilT<I, F> {
    fn skip_until(self, f: F) -> SkipUntil<I, F>;
}
impl<Iter, Item, F> SkipUntilT<Iter, F> for Iter
where Iter: Iterator<Item = Item>, F: FnMut(&Item) -> bool {
    fn skip_until(self, f: F) -> SkipUntil<Iter, F> {
        SkipUntil { inner: self, finished_skip: false, f }
    }
}

pub fn scrub_until_at_start<'a>(mut haystack: &'a str, pat: &str) -> Option<(usize, &'a str)> {
    let mut idx;
    let haystack_original = haystack;
    
    loop {
        let mut chars = haystack.char_indices().skip_until(|c| !c.1.is_whitespace()).peekable();
        idx = chars.peek()?.0;
        // skip until first non-whitespace character
        'char_match: for (hay, needle) in chars.zip(pat.chars()) {
            if hay.1 == '\n' {
                break 'char_match
            }
            if hay.1 == needle {
                if (hay.0 - idx + 1) == pat.len() {
                    let ret = &haystack[idx..];
                    let idx = unsafe {
                        index_of_slice(haystack_original, ret)
                    };
                    return Some((idx, ret))
                }
            }
            else { // strings don't match, bail out
                break 'char_match
            }
        }


        let next_line_start = haystack.char_indices().skip_until(|c| c.1 == '\n').next()?.0 + 1;
        haystack = &haystack[next_line_start..]
    }
}

/// SAFETY: needle must be a slice into haystack. if it's not, the best outcome is a panic, and the worst is a totally nonsensical value
pub unsafe fn index_of_slice(haystack: &str, needle: &str) -> usize {
    let haystack_start = haystack.as_ptr() as usize;
    let needle_start = needle.as_ptr() as usize;

    needle_start - haystack_start
}

pub struct ScrubAnyResult<'a> {
    pub needle: usize,
    pub idx: usize,
    pub remainder: &'a str
}

pub fn scrub_until_any_at_start<'a>(haystack: &'a str, needles: &[&str]) -> Option<ScrubAnyResult<'a>> {
    // naive impl for now

    let mut found_match = false;
    let mut earliest_needle = 0;
    let mut idx = 0;

    for hay_line in haystack.lines() {
        if hay_line.trim().is_empty() {
            continue
        }
        for (i, n) in needles.iter().enumerate() {
            if let Some(idx_inner) = hay_line.find(n) {
                let hs_index = unsafe { index_of_slice(haystack, &hay_line[idx_inner..]) };
                if !found_match {
                    idx = hs_index;
                    found_match = true;
                    earliest_needle = i
                }
                else if hs_index < idx {
                    idx = hs_index;
                    earliest_needle = i
                }
            }
        }
    }
    
    if found_match {
        Some(ScrubAnyResult {
            needle: earliest_needle,
            idx,
            remainder: &haystack[idx..]
        })
    }
    else { None }
}

pub fn cut_first_line(s: &str) -> Option<&str> {
    s.split_once('\n').map(|spl| spl.1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn skip_until() {
        let mut chars = "   abc".chars().skip_until(|c| !c.is_whitespace());
        assert_eq!(chars.next(), Some('a'))
    }

    #[test]
    fn scrub() {
        let haystack = "abc\n123\n   .macro\naaa";
        let mat = scrub_until_at_start(haystack, ".macro");
        assert!(mat.is_some());
        let (idx, mat) = mat.unwrap();
        assert_eq!(
            mat,
            ".macro\naaa"
        );
        assert_eq!(
            &haystack[idx..],
            ".macro\naaa"
        )
    }
}

