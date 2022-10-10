use crate::utils::*;
use std::collections::HashMap;

pub fn preprocess(code: &str) -> PreResult<String> {
    let mut mm = MacroManager::new(&code);
    mm.go()?;
    Ok(mm.output)
}

struct MacroManager<'a> {
    source: &'a str,
    macros: HashMap<&'a str, OjMacro<'a>>,
    lookup_names: Vec<&'a str>,
    output: String,
    ctr: usize
}
impl<'a> MacroManager<'a> {
    pub fn new(text: &'a str) -> MacroManager {
        MacroManager {
            source: text,
            macros: HashMap::new(),
            output: String::new(),
            lookup_names: vec![".macro"],
            ctr: 0
        }
    }

    pub fn go(&mut self) -> PreResult<()> {
        loop {
            if let Some(res) = scrub_until_any_at_start(self.source, &self.lookup_names) {
                let code = &self.source[..res.idx];
                self.output.push_str(code);
                if res.needle == 0 { // macro decl
                    let rem_source = res.remainder;
    
                    if let Some((idx, new_source)) = scrub_until_at_start(rem_source, ".endm") {
                        let text_start = rem_source.char_indices().position(|c| c.1 == '\n').unwrap(); // unwrap ok, must exist
                        let macro_text = &rem_source[text_start + 1..idx];
                        let macro_decl_line_full = rem_source.lines().next().unwrap();
                        let macro_decl_line = macro_decl_line_full.split_once(';').map(|s| s.0).unwrap_or(macro_decl_line_full);
                        let mut macro_decl = macro_decl_line.split_ascii_whitespace().skip(1);
                        let macro_name = macro_decl.next().ok_or_else(|| MacErr::InvalidDecl(String::from(macro_decl_line)))?;
                        let macro_args = macro_decl.collect();
                        let mac = OjMacro {
                            text: macro_text,
                            args: macro_args,
                        };
                        self.macros.insert(macro_name, mac);
                        self.lookup_names.push(macro_name);
                        
                        if let Some(cut) = cut_first_line(new_source) {
                            self.source = cut
                        }
                        else { break } // no more code after .endm
                    }
                    else { break } // no .endm
                }
                else { // macro invoc
                    let name = self.lookup_names[res.needle];
                    let macr = &self.macros[name]; // will always exist
                    let invoc_full = res.remainder.lines().next().unwrap();
                    let invoc = invoc_full.split_once(';').map(|s| s.0).unwrap_or(invoc_full);
                    let args: Vec<&str> = invoc.split_ascii_whitespace().skip(1).collect();
                    macr.expand(args, self.ctr, &mut self.output)?;
                    self.ctr += 1;

                    if let Some(cut) = cut_first_line(res.remainder) {
                        self.source = cut
                    }
                    else { break } // no more code after macro
                }
            }
            else {
                self.output.push_str(self.source);
                break
            } // no more macros
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
struct OjMacro<'a> {
    text: &'a str,
    args: Vec<&'a str>
}
impl OjMacro<'_> {
    fn expand(&self, args: Vec<&str>, mac_ctr: usize, ret: &mut String) -> PreResult<()> {
        if args.len() != self.args.len() {
            return Err(MacErr::BadArgs)
        }

        let mut chars = self.text.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\\' { // escape
                if let Some(c2) = chars.peek() {
                    match c2 {
                        '\\' => ret.push(chars.next().unwrap()), // literal backslash
                        '@' => { // macro index
                            let _ = chars.next();
                            ret.push_str(&mac_ctr.to_string())
                        }
                        _ => { // arg replacement
                            let mut buf = String::new();
                            let mut found_arg = false;
                            while let Some(c) = chars.next() {
                                if c == ' ' {
                                    break
                                }
                                buf.push(c);
                                if let Some(idx) = self.args.iter().position(|a| a == &buf) {
                                    ret.push_str(args[idx]);
                                    found_arg = true;
                                    break
                                }
                            }
                            if !found_arg {
                                return Err(MacErr::InvalidRepEscape)
                            }
                        }
                    }
                }
            }
            else {
                ret.push(c)
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expansion() {
        let mac = OjMacro {
            text: "abc \\abcde aaa \\\\ \\@",
            args: vec!["abc"]
        };
        let mut buf = String::new();
        mac.expand(vec!["123"], 0, &mut buf).unwrap();
        assert_eq!(
            buf,
            String::from("abc 123de aaa \\ 0")
        )
    }
}
