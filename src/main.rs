use instruction::*;
use directive::*;
use std::collections::HashMap;
use utils::*;
use std::fs;
use std::env::args;
use std::io::{stdin, Read};
use preprocessor::preprocess;

mod instruction;
mod directive;
mod utils;
mod preprocessor;

fn main() {
    let mut args: Vec<String> = args().skip(1).collect(); // skip command name
    if args.is_empty() {
        args.push(String::from("-"))
    }
    let code_read = if args[0] == "-" {
        let mut buf = String::new();
        stdin().read_to_string(&mut buf).map(|_| buf)
    }
    else {
        fs::read_to_string(&args[0])
    };
    if let Ok(code) = code_read {
        match preprocess(&code) {
            Ok(code) => {
                let mut a = Assembler::new(&code);
                match a.run() {
                    Ok(_) => {
                        match a.export(&ExportOptions::new()) {
                            Ok(b) => {
                                if let Err(_) = fs::write(args.get(1).unwrap_or(&"a.out".into()), &b) {
                                    eprintln!("failed to write output file")
                                }
                            }
                            Err((e, line)) => {
                                eprintln!("error on line {line}: {e}")
                            }
                        }
                    }
                    Err((e, line)) => {
                        eprintln!("error on line {line}: {e}")
                    }
                }
            }
            Err(e) => {
                eprintln!("preprocessor error: {}", e)
            }
        }
    }
    else {
        eprintln!("failed to read input file {}", &args[0])
    }
}

/*
    assembler goes through each line in turn and tries to parse it into a Line
    if the line starts with . it's a directive, else it's an instruction
*/
struct Assembler<'a> {
    program: Vec<LineOuter<'a>>,
    idents: HashMap<&'a str, (u16, bool)>, // true if the ident is a label
    ctr: u16,
    lines: CodeIter<'a>
}
impl<'a> Assembler<'a> { // housekeeping
    pub fn new(code: &'a str) -> Self {
        let mut lines = InsertableIter::new();
        lines.insert(LineIter::FromSource(code.lines().enumerate()));
        Assembler {
            program: Vec::new(),
            idents: HashMap::new(),
            ctr: 0,
            lines,
        }
    }
    pub fn run(&mut self) -> std::result::Result<(), (AsmErr<'a>, usize)> {
        while let Some((idx, line)) = self.lines.next() {
            self.do_line(line, idx + 1).map_err(|e| (e, idx + 1))?
        }
        
        Ok(())
    }
    pub fn export(&mut self, _e: &ExportOptions) -> std::result::Result<Vec<u8>, (AsmErr<'a>, usize)> {
        self.ctr = 0;
        let mut bytes = Vec::new();

        let mut program = Vec::new();
        std::mem::swap(&mut program, &mut self.program); // eat shit borrow checker

        for line in program {
            let mut line_bytes = self.export_line(_e, line.inner).map_err(|e| (e, line.position))?;
            self.ctr = self.ctr.wrapping_add(line_bytes.len() as u16);
            bytes.append(&mut line_bytes)
        }
        

        Ok(bytes)
    }
}

impl<'a> Assembler<'a> { // export
    fn export_line(&mut self, _e: &ExportOptions, line: Line<'a>) -> Result<'a, Vec<u8>> {
        match line {
            Line::Instruction(i) => {
                self.export_instruction(_e, i)
            }
            Line::Directive(d) => {
                self.export_directive(_e, d)
            }
        }
    }

    fn export_directive(&mut self, _e: &ExportOptions, line: Directive<'a>) -> Result<'a, Vec<u8>> {
        match line {
            Directive::Org(dest) => {
                self.ctr = dest;
                Ok(vec![])
            }
            Directive::Space(amt) => {
                let mut ret = Vec::new();
                ret.resize(amt as usize, 0);
                Ok(ret)
            }
            Directive::SpaceTo(dest) => {
                let fill = dest.checked_sub(self.ctr).ok_or(AsmErr::BackwardsSpaceto)? as usize;
                let mut ret = Vec::new();
                ret.resize(fill, 0);
                Ok(ret)
            }
            Directive::Data(v) => {
                let [lb, hb] = self.value(v)?.to_le_bytes();
                Ok(vec![lb, hb])
            }
            Directive::Set { src, dest } => {
                let v = self.value(src)?;
                if let Some((_, true)) = self.idents.insert(dest, (v, false)) {
                    Err(AsmErr::DoubleLabel(dest))
                }
                else {
                    Ok(vec![])
                }

            }
        }
    }

    fn export_instruction(&mut self, _e: &ExportOptions, line: Instruction<'a>) -> Result<'a, Vec<u8>> {
        let mut ret = Vec::new();
        let bytes = match line {
            Instruction::I { opcode, rd, imm } => {
                let mut i = opcode.binary();
                i |= (rd as u16) << 4;
                i |= self.value(imm)? << 8;
                i
            }
            Instruction::M { opcode, ra, ro, rd } => {
                let mut i = opcode.binary();
                i |= (rd as u16) << 4;
                i |= (ro as u16) << 8;
                i |= (ra as u16) << 12;
                i
            }
            Instruction::J { opcode, addr } => {
                let dest = self.value(addr)?; // excess k
                if dest & 1 != 0 {
                    return Err(AsmErr::UnalignedJump)
                }
                let from = self.ctr;
                let offset = dest.wrapping_add(4096).wrapping_sub(from);
                //eprintln!("{} {} {}", dest, from, offset);
                if offset > 4096 * 2 {
                    return Err(AsmErr::JumpTooLong)
                }
                let mut i = opcode.binary();
                i |= offset << 3;
                i
            }
            Instruction::R { opcode, rs, rd } => {
                let mut i = opcode.binary();
                i |= (rd as u16) << 8;
                i |= (rs as u16) << 12;
                i
            }
        }.to_be_bytes();
        ret.extend_from_slice(&bytes);
        Ok(ret)
    }

    fn value(&self, v: Value<'a>) -> Result<'a, u16> {
        match v {
            Value::Literal(i) => Ok(i),
            Value::Ident(name, sel) => {
                let i = self.idents.get(name).ok_or(AsmErr::UndefinedSymbol(name))?.0;
                Ok(match sel {
                    ByteSelection::All => i,
                    ByteSelection::High => (i & 0xff00) >> 8,
                    ByteSelection::Low => i & 0xff
                })
            }
        }
    }
}

impl<'a> Assembler<'a> { // parsing
    fn add_line(&mut self, l: Line<'a>, pos: usize) {
        let l = LineOuter { inner: l, position: pos };
        self.program.push(l);
        self.ctr += 2;
    }
    fn add_label(&mut self, l: &'a str) -> Result<'a, ()> {
        if l.contains(['/']) {
            Err(AsmErr::BadIdent(l))
        }
        else {
            if let Some(_) = self.idents.insert(l, (self.ctr, true)) {
                Err(AsmErr::DoubleLabel(l))
            }
            else {
                Ok(())
            }
        }
    }

    fn do_line(&mut self, line: &'a str, pos: usize) -> Result<'a, ()> {
        let line = line.trim();
        if line.is_empty() { // ignore empty line
            return Ok(())
        }
        let line = if let Some((new_line, _comment)) = line.split_once(';') { new_line } else { line }; // remove ; comments
        
        let line = if let Some((lbl, new_line)) = line.split_once(':') { // TODO: reject labels containing spaces
            self.add_label(lbl.trim())?;
            new_line
        }
        else { line }.trim();

        if line.is_empty() { // return when line is only label
            return Ok(())
        }

        if line.starts_with('.') {
            // directive
            self.parse_directive(&line[1..], pos)
        }
        else {
            // instruction
            self.parse_instruction(line, pos)
        }
    }

    fn parse_directive(&mut self, line: &'a str, pos: usize) -> Result<'a, ()> {
        if line.trim().is_empty() {
            return Err(AsmErr::BadDirective(line))
        }
        let mut parts = line.split_ascii_whitespace();
        let directive_name = parts.next().unwrap(); // will always exist, line will never be empty
        let inner = Line::Directive(match directive_name {
            "org" => {
                let dest = parse_int_literal(parts.next().ok_or(AsmErr::BadDirective(line))?)?;
                if dest & 1 != 0 {
                    return Err(AsmErr::UnalignedOrg)
                }
                if self.ctr != 0 {
                    return Err(AsmErr::OrgNotAtAtStart)
                }
                self.ctr = dest;
                Directive::Org(dest)
            }
            "spaceto" => {
                let dest = parse_int_literal(parts.next().ok_or(AsmErr::BadDirective(line))?)?;
                if dest & 1 != 0 {
                    return Err(AsmErr::UnalignedOrg)
                }
                if dest.checked_sub(self.ctr).is_none() {
                    return Err(AsmErr::BackwardsSpaceto)
                }
                self.ctr = dest;
                Directive::SpaceTo(dest)
            }
            "space" => {
                let mut amt = parse_int_literal(parts.next().ok_or(AsmErr::BadDirective(line))?)?;
                if amt & 1 != 0 {
                    amt = amt.checked_add(1).ok_or(AsmErr::BadDirective(line))?
                }
                self.ctr = self.ctr.wrapping_add(amt);
                Directive::Space(amt)
            }
            "data" => {
                let value = Value::from_str(parts.next().ok_or(AsmErr::BadDirective(line))?)?;
                Directive::Data(value)
            }
            "set" => {
                let name = parts.next().ok_or(AsmErr::BadDirective(line))?;
                let value = Value::from_str(parts.next().ok_or(AsmErr::BadDirective(line))?)?;
                Directive::Set{ src: value, dest: name }
            }
            /*"macro" => {
                let macro_contents = self.lines.collect_until(|l| l.1.trim() == ".endm");
                eprintln!("{:?}", macro_contents);

                todo!()
            }*/
            _ => return Err(AsmErr::BadDirective(line))
        });
        let line = LineOuter { inner, position: pos };
        self.program.push(line);
        Ok(())
    }

    fn parse_instruction(&mut self, line: &'a str, pos: usize) -> Result<'a, ()> {
        let mut parts = line.split_ascii_whitespace();
        let opcode_str = parts.next().unwrap(); // will always exist, line will never be empty
        let operands: Vec<&str> = parts.collect();
        let opcode = Opcode::from_str(opcode_str)?;
        match opcode {
            Opcode::I(opcode) => {
                self.parse_i_format(opcode, operands, pos)
            }
            Opcode::M(opcode) => {
                self.parse_m_format(opcode, operands, pos)
            }
            Opcode::J(opcode) => {
                self.parse_j_format(opcode, operands, pos)
            }
            Opcode::R(opcode) => {
                self.parse_r_format(opcode, operands, pos)
            }
        }
    }

    fn parse_i_format(&mut self, opcode: IFormat, operands: Vec<&'a str>, pos: usize) -> Result<'a, ()> {
        if operands.len() != 2 {
            Err(AsmErr::BadOperandNum { got: operands.len(), expected: 2 })
        }
        else {
            let value = Value::from_str(operands[0])?;
            let reg = parse_reg(operands[1])?;
            if let IFormat::Ldi = opcode { // one of 5 special cases in the entire assembler
                let (vall, valh) = match value {
                    Value::Ident(name, sel) => {
                        if let ByteSelection::All = sel {
                            (Value::Ident(name, ByteSelection::Low), Value::Ident(name, ByteSelection::High))
                        }
                        else {
                            todo!() // some kind of error idk
                        }
                    }
                    Value::Literal(i) => {
                        let [lo, hi] = i.to_le_bytes();
                        (Value::Literal(lo as u16), Value::Literal(hi as u16))
                    }
                };
                let i_low = Line::Instruction(Instruction::I {
                    opcode: IFormat::Ldl,
                    imm: vall,
                    rd: reg
                });
                let i_hi = Line::Instruction(Instruction::I {
                    opcode: IFormat::Ldh,
                    imm: valh,
                    rd: reg
                });
                self.add_line(i_low, pos);
                self.add_line(i_hi, pos);
            }
            else {
                let i = Line::Instruction(Instruction::I {
                    opcode, rd: reg, imm: value
                });
                self.add_line(i, pos)
            }

            Ok(())
        }
    }

    fn parse_j_format(&mut self, opcode: JFormat, operands: Vec<&'a str>, pos: usize) -> Result<'a, ()> {
        if operands.len() != 1 {
            Err(AsmErr::BadOperandNum { got: operands.len(), expected: 1 })
        }
        else {
            let dest = Value::from_str(operands[0])?;
            let i = Line::Instruction(Instruction::J {
                opcode, addr: dest
            });
            self.add_line(i, pos);
            
            Ok(())
        }
    }

    fn parse_m_format(&mut self, opcode: MFormat, operands: Vec<&'a str>, pos: usize) -> Result<'a, ()> {
        if operands.len() != 3 {
            Err(AsmErr::BadOperandNum { got: operands.len(), expected: 3 })
        }
        else {
            let ra = parse_reg(operands[0])?;
            let ro = parse_reg(operands[1])?;
            let rd = parse_reg(operands[2])?;
            let i = Line::Instruction(Instruction::M {
                opcode, ra, ro, rd
            });
            self.add_line(i, pos);

            Ok(())
        }
    }

    fn parse_r_format(&mut self, opcode: RFormat, operands: Vec<&'a str>, pos: usize) -> Result<'a, ()> {
        let i = match opcode {
            RFormat::Int => {
                if operands.len() != 0 {
                    Err(AsmErr::BadOperandNum { got: operands.len(), expected: 0 })
                }
                else {
                    Ok(Line::Instruction(Instruction::R { opcode, rs: 0, rd: 0 }))
                }
            }
            RFormat::Sf | RFormat::Psr | RFormat::Iret => {
                if operands.len() != 1 {
                    Err(AsmErr::BadOperandNum { got: operands.len(), expected: 1 })
                }
                else {
                    let rs = parse_reg(operands[0])?;
                    Ok(Line::Instruction(Instruction::R { opcode, rs, rd: 0 }))
                }
            }
            RFormat::Gf | RFormat::Not => {
                if operands.len() != 1 {
                    Err(AsmErr::BadOperandNum { got: operands.len(), expected: 1 })
                }
                else {
                    let rd = parse_reg(operands[0])?;
                    Ok(Line::Instruction(Instruction::R { opcode, rs: 0, rd }))
                }
            }
            _ => {
                if operands.len() != 2 {
                    Err(AsmErr::BadOperandNum { got: operands.len(), expected: 2 })
                }
                else {
                    let rs = parse_reg(operands[0])?;
                    let rd = parse_reg(operands[1])?;
                    Ok(Line::Instruction(Instruction::R { opcode, rs, rd }))
                }
            }
        }?;
        self.add_line(i, pos);
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
enum Line<'a> {
    Instruction(Instruction<'a>),
    Directive(Directive<'a>)
}
#[derive(Debug, Copy, Clone)]
struct LineOuter<'a> {
    inner: Line<'a>,
    position: usize
}

#[derive(Debug, Copy, Clone)]
pub enum Value<'a> {
    Ident(&'a str, ByteSelection),
    Literal(u16)
}
#[derive(Debug, Copy, Clone)]
pub enum ByteSelection {
    High, Low, All
}
impl<'a> Value<'a> {
    pub fn from_str(s: &'a str) -> Result<'a ,Value<'a>> {
        if s.chars().next().unwrap().is_ascii_digit() {
            parse_int_literal(s).map(|i| Value::Literal(i))
        }
        else {
            let (name, sel) = if let Some((name, sel)) = s.split_once('/') {
                let sel = match sel {
                    "l" => Ok(ByteSelection::Low),
                    "h" => Ok(ByteSelection::High),
                    _ => Err(AsmErr::BadIdent(s))
                }?;
                (name, sel)
            }
            else {
                (s, ByteSelection::All)
            };
            Ok(Value::Ident(name, sel))
        }
    }
}

