use super::Value;

#[derive(Debug, Copy, Clone)]
pub enum Directive<'a> {
    Store{ src: Value<'a>, dest: &'a str }
}
