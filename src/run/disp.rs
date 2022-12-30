use crate::codepage::tochar;
use crate::prelude::*;
use alloc::fmt::{Write, Formatter, Result as FResult};

use super::{Val, Lis, Num, Int};

impl alloc::fmt::Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { self.format(f, &[]) }
}

impl Val {
pub fn format(&self, f: &mut impl Write, slice: &[Val]) -> FResult {
    let rest  = slice.get(1..).unwrap_or(&[]);
    match slice.first().and_then(|x| x.try_int()) {
        Some(0) => return write!(f, "{}", self.display_string()),
        Some(1) => if self.is_scalar() { return write!(f, "{self}"); }
        Some(2) => if let Some(n) = self.try_int() { return write!(f, "{n}") }
        Some(3) => if let Some(c) = self.try_int() { return match char::from_u32(c as u32) {
            Some('\x00'..='\x1F' | '\x7F'..='\u{9F}' | '\\') =>
                write!(f, "\\{}", tochar(c as u8)),
            Some(c) => write!(f, "{c}"), None => write!(f, "\u{FFFD}"),
        }},
        Some(4) => if let Lis{l, ..} = self { 'give_up: {
            let mut string = String::with_capacity(l.len());
            for x in l.iter() { match x.try_int().map(|x| char::from_u32(x as u32)) {
                Some(Some(c @ ('\x00'..='\x1F' | '\x7F'..='\u{9F}' | '\\' | '\"'))) =>
                    write!(string, "\\{}", tochar(c as u8))?,
                Some(Some(c)) => string.push(c), Some(None) => string.push('\u{FFFD}'),
                None => break 'give_up,
            }}
            return write!(f, "\"{string}\"");
        }}
        Some(5) => if let Lis{l, ..} = self { 'give_up: {
            let mut string = String::with_capacity(l.len());
            for x in l.iter() { if let Some(c) = x.try_int() { 
                string.push(c.try_into().map_or('\u{FFFD}', tochar));
            } else { break 'give_up; } }
            return write!(f, "\"{string}\"");
        }}
        Some(7) => if let Lis{l, fill} = self { 
            let mut indent = Indent(f, 1);
            let mut iter = l.iter();
            write!(indent, "(")?;
            if let Some(i) = iter.next() { i.format(&mut indent, rest)?; }
            for i in iter { writeln!(indent)?; i.format(&mut indent, rest)?; }
            write!(indent, ")")?;
            if !fill.is_nan() { write!(indent, "▐")?; fill.format(&mut indent, rest)?; }
            return Ok(());
        }
        Some(align @ (8|9)) => if let Lis{l, fill} = self {
            let mut col_lens = Vec::new();
            let list: Vec<Result<(Vec<String>, &Val), &Val>> = l.iter().map(|i| { match i {
                Lis{l, fill} => Ok((l.iter().enumerate().map(|(n, j)| {
                    let mut s = String::new();
                    j.format(&mut Indent(&mut s, 1), rest).unwrap();
                    if l.len() + 1 > col_lens.len() { col_lens.resize(l.len() + 1, 0); }
                    if col_lens[n] < s.chars().count() { col_lens[n] = s.chars().count(); };
                s }).collect(), fill.as_ref())),
                _ => Err(i)
            }}).collect();
            f.write_char('(')?;
            for (n, i) in list.iter().enumerate() { 
                match i {
                    Ok((l, fill)) => {
                        f.write_char('(')?;
                        for (n, j) in l.iter().enumerate() {
                            _ = if align == 8 {
                                write!(f, "{: <width$}", j, width=col_lens[n])
                            } else {
                                write!(f, "{: >width$}", j, width=col_lens[n])
                            };
                            if n != l.len()-1 {f.write_char(' ')?;}
                        }; f.write_char(')')?;
                        if !fill.is_nan() { write!(f, "▐")?; fill.format(f, rest)?; }
                    }
                    Err(v) => v.format(&mut Indent(f, 1), rest)?,
                }
                if n != l.len()-1 {f.write_str("\n ")?;}
            }
            f.write_char(')')?;
            if !fill.is_nan() { write!(f, "▐")?; fill.format(f, rest)?; }
            return Ok(());
        }
        _ => ()
    };
    match self {
        Num(n) => {
            if n.is_nan() { return write!(f, "█"); }
            write!(f, "{}", n.re)?;
            if n.im != 0. { write!(f, "{:+}i", n.im)? };
        Ok(()) },
        Int(n) => write!(f, "{n}"),
        Lis { l, fill } => {
            let mut iter = l.iter();
            write!(f, "(")?;
            if let Some(i) = iter.next() { i.format(f, slice)?; }
            for i in iter { write!(f, " ")?; i.format(f, slice)?; }
            write!(f, ")")?;
            if !fill.is_nan() { write!(f, "▐{fill}")?; }
        Ok(()) },
        Val::FSet(x) => write!(f, "→{}", crate::codepage::tochars(x)),
        Val::Err(x) => write!(f, "ERROR ERROR {x}"),
        _ => write!(f, "<function>"),
    }
}}

struct Indent<'a>(&'a mut dyn Write, u32);
impl<'a> alloc::fmt::Write for Indent<'a> {
    fn write_str(&mut self, mut s: &str) -> FResult {
        let mut i = 0;
        loop { match s.as_bytes().get(i) {
            Some(b'\n') => {
                self.0.write_str(&s[0..i+1])?;
                for _ in 0..self.1 { self.0.write_char(' ')?; }
                s = &s[i+1..]; i = 0;
            }
            Some(_) => i += 1,
            None => return self.0.write_str(s),
        }}
    }
    fn write_char(&mut self, c: char) -> FResult {
        self.0.write_char(c)?;
        if c == '\n' { for _ in 0..self.1 { self.0.write_char(' ')?; } }
    Ok(()) }
}

impl Val {
    pub fn display_string(&self) -> String {
        if let Some(n) = self.try_c() {
            if n.is_nan() { String::new() }
            else if n.im != 0. { format!("{}{:+}i", n.re, n.im) }
            else { format!("{}", n.re) }
        } else if let Lis { l, .. } = self {
            l.iter().enumerate().map( |(i,x)| x.try_int().map_or_else(
                | | x.display_string() + if i == l.len()-1 {""} else {"\n"},
                |n| char::from_u32(n as u32).into_iter().collect::<String>()
            )).collect()
        } else { format!("{self}") }
    }
}