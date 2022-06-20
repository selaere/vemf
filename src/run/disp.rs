use crate::codepage::tochar;
use std::fmt::Write;

use super::{Val, Lis, Num, Int};


impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Num(n) => {
                if n.is_nan() { return write!(f, "NaN"); }
                write!(f, "{}", n.re)?;
                if n.im != 0. { write!(f, "{:+}i", n.im)? };
                Ok(())
            },
            Int(n) => write!(f, "{}", n),
            Lis { l, .. } => {
                let mut iter = l.iter();
                write!(f, "(")?;
                if let Some(i) = iter.next() { write!(f, "{}", i)?; }
                for i in iter { write!(f, " {}", i)?; }
                write!(f, ")")?;
                Ok(())
            },
            Val::FSet(x) => write!(f, "→{}", crate::codepage::tochars(x)),
            z => write!(f, "<function {:?}>", z),
        }
    }
}

pub fn format(val: &Val, slice: &[Val]) -> String {
    let Some(first) = slice.first() else {return format!("{}", val)};
    let rest = &slice[1..];
    first.try_int().map_or_else(|| format(val, rest), |n| match n as isize {
        1 => if val.is_scalar() { format!("{val}")    } else { format(val, rest) }
        2 => if let Some(n) = val.try_int() { format!("{}", n) } else { format(val, rest) }
        3 => val.try_int().map_or_else(|| format(val, rest),
            |n| match char::from_u32(n as u32) {
                Some('\x00'..='\x19' | '\x7F'..='\u{9F}' | '\\') =>
                    format!("\\{}", tochar(n as u8)),
                Some(c) => c.to_string(), None => '\u{FFFD}'.to_string(),
            }),
        4 => match val { Lis{l, ..} => 
            std::iter::once("\"".to_string())
            .chain(l.iter().map(|x| x.try_int().map_or('\u{FFFD}'.to_string(),
                |n| match char::from_u32(n as u32) {
                    Some('\x00'..='\x19' | '\x7F'..='\u{9F}' | '\\' | '"') =>
                        format!("\\{}", tochar(n as u8)),
                    Some(c) => c.to_string(), None => '\u{FFFD}'.to_string(),
                }
            )))
            .chain(std::iter::once("\"".to_string()))
            .collect(),
        _ => format(val, rest) }
        5 => match val { Lis{l, ..} => 
            l.iter().map(|x| x.try_int().map_or('\u{FFFD}',
                |n| n.try_into().map_or('\u{FFFD}', tochar)
            ))
            .collect(),
        _ => format(val, rest) }
        7 => match val { Lis{l, ..} =>
                "(".to_string() 
            + &l.iter().map(|x| format(x, rest)).collect::<Vec<_>>().join(" ") 
            + ")",
        _ => format(val, rest) }
        8 => match val { Lis{l, ..} => 
                "(".to_string() 
            + &l.iter().map(|x| indent(&format(x, rest), 1)).collect::<Vec<_>>().join("\n ")
            + ")",
        _ => format(val, rest) }
        9 => match val { Lis{l, ..} => {
            //let mut columns = 0;
            let mut col_lens = Vec::new();
            let list: Vec<Result<Vec<String>, String>> = l.iter().map(|i| match i {
                Lis{l, ..} => Ok(l.iter().enumerate().map(|(n, j)| {
                    let f = format(j, rest);
                    if l.len() + 1 > col_lens.len() { col_lens.resize(l.len() + 1, 0); }
                    if col_lens[n] < f.len() { col_lens[n] = f.len(); };
                    f
                }).collect()),
                #[allow(clippy::needless_borrow)] /*clippy bug i think*/
                _ => Err(format(&i, rest))
            }).collect();

            let mut string = String::new();
            string.push('(');
            for (n, i) in list.iter().enumerate() {
                match i {
                    Ok(l) => {
                        string.push('(');
                        for (n, j) in l.iter().enumerate() {
                            let _ = write!(string, "{: >width$}", j, width=col_lens[n]);
                            if n != l.len()-1 {string.push(' ')}
                        }; string.push(')');
                    }
                    Err(v) => string.push_str(&indent(v, 1))
                }
                if n != l.len()-1 {string.push_str("\n ");}
            }
            string.push(')');
            string
        }, _ => format(val, rest) }
        _ => format(val, rest)
    })
}

fn indent(string: &str, indent: usize) -> String {
    let mut iter = string.split_inclusive('\n');
    let mut out = String::with_capacity(string.len());
    if let Some(ln) = iter.next() { out.push_str(ln); }
    for ln in iter {
        for _ in 0..indent {out.push(' ');}
        out.push_str(ln);
    }
    out
}

impl Val {
    pub fn display_string(&self) -> String {
        self.try_int().map_or_else(|| match self {
            Lis { l, .. } => l.iter().map(
                |x| x.try_int().map_or_else(
                    | | x.display_string() + "\n",
                    |n| char::from_u32(n as u32).into_iter().collect::<String>())
            ).collect(),
            otherwise => format!("{}", otherwise),
        }, |n| format!("{}", n))
    }
}