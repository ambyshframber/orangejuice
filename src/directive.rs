use super::Value;

#[derive(Debug, Copy, Clone)]
pub enum Directive<'a> {
    Set{ src: Value<'a>, dest: &'a str },
    Org(u16),
    SpaceTo(u16),
    Space(u16),
    Data(Value<'a>),
    Ascii(&'a str),
}
