use std::fmt::Display;

use smallvec::smallvec;
use crate::{b, Bstr};
use crate::codepage::tochars;
use crate::token::Tok;

// Value expression
#[derive(Clone, Debug)]
pub enum Ve {
    Var(Bstr),
    Num(f64),
    Snd(Vec<Ve>),  // strand
    Nom(Fe),       // function as value
    Afn1{ a: Box<Ve>, f: Fe             },  // apply monadic function
    Afn2{ a: Box<Ve>, f: Fe, b: Box<Ve> },  // apply dyadic  function 
}
// Function expression
#[derive(Clone, Debug)]
pub enum Fe {
    Var(Bstr),
    SetVar(Bstr),
    Aav1{            v: Bstr   , g: Box<Tg>}, //     apply monadic adverb
    Aav2{f: Box<Tg>, v: Bstr   , g: Box<Tg>}, //     apply dyadic  adverb
    Bind{            f: Box<Fe>, b: Box<Ve>}, // +1
    Trn1{a: Box<Fe>, f: Box<Fe>            }, // +/
    Trn2{a: Box<Fe>, f: Box<Fe>, b: Box<Ve>}, // +/2
    Dfn(Vec<Ve>),
}

// a thing (Tg) is either:
// - a function
// - a value
#[derive(Clone, Debug)]
pub enum Tg {
    Fe(Fe),
    Ve(Ve)
}

impl Display for Ve {
    fn fmt(&self, m: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ve::Var(v) => write!(m, ".{}", tochars(v)),
            Ve::Num(n) => write!(m, "˙{}", n),
            Ve::Snd(l) => {
                write!(m, "(")?;
                for v in l { write!(m, "{}", v)?; }
                write!(m, ")")?;
                Ok(())
            },
            Ve::Nom(v) => write!(m, "♪{}", v),
            Ve::Afn1{ a, f } => write!(m, "({} {})", a, f),
            Ve::Afn2{ a, f, b } => write!(m, "({} {} {})", a, f, b),
        }
    }
}

impl Display for Fe {
    fn fmt(&self, m: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fe::Var(v) => write!(m, ":{}", tochars(v)),
            Fe::SetVar(v) => write!(m, "→{}", tochars(v)),
            Fe::Aav1{    v, g } => write!(m, "[•{} {}]", tochars(v), g),
            Fe::Aav2{ f, v, g } => write!(m, "[{} ○{} {}]", f, tochars(v), g),
            Fe::Bind{    f, b } => write!(m, "[{} with {}]", f, b),
            Fe::Trn1{ a, f    } => write!(m, "[{} {}]", a, f),
            Fe::Trn2{ a, f, b } => write!(m, "[{} {} {}]", a, f, b),
            Fe::Dfn(efs) => {
                write!(m, "{{ ")?;
                for v in efs { write!(m, "{}; ", v)?; }
                write!(m, "}}")?;
                Ok(())
            },
        }
    }
}

impl Display for Tg {
    fn fmt(&self, m: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self { Tg::Fe(f) => write!(m, "{}", f),
                     Tg::Ve(v) => write!(m, "{}", v)}
    }
}

fn slice_offset<T>(code: &[T], slice: &[T]) -> usize {
    if slice.is_empty() { return code.len() }
    let ptr = slice.as_ptr();
    assert!(code.as_ptr_range().contains(&ptr));
    unsafe {
        // SAFETY: we check that `slice` is a subset of `code`, so this SHOULD be fine
        ptr.offset_from(code.as_ptr()) as usize
    }
}

fn parse_function(code: &[Tok]) -> (usize, Option<Fe>) {
    if let Some(t) = code.first() {
        match t {
            // these are functions until when they aren't
            Tok::Stmt(c @ b'A'..=b'Z') => (1, Some(Fe::SetVar(smallvec![*c + 32]))),
            Tok::Stmt(c @ b!('☺''☻')) => (1, Some(Fe::Var(smallvec![*c]))),
            Tok::VarSet(v) => (1, Some(Fe::SetVar(v.clone()))),
            // monadic adverbs
            Tok::Just(c @ b!('│''╡''╢''╞''╛''╜''╘''╙''═')) => {
                let (offset, thing) = parse_thing(&code[1..]);
                (offset+1, thing.map(|x| Fe::Aav1{v: smallvec![*c], g: Box::new(x)}))
            },
            Tok::Just(b'{') => {
                let mut slice = &code[1..];
                let (len, b) = parse_block(slice); slice = &slice[len..];
                (
                    slice_offset(code, slice) + usize::from(matches!(slice.first(), Some(Tok::Just(b'}')))),
                    Some(Fe::Dfn(b))
                )
            }
            Tok::VarFun(v) => (1, Some(Fe::Var(v.clone()))),
            _ => (0, None)
        }
    } else {(0, None)}
}

fn atom_token(chr: Tok) -> Option<Ve> {
    Some(match chr {
        Tok::Just(c @ b'0'..=b'9') => Ve::Num(f64::from(c - b'0')),
        Tok::Just(b!('Φ')) => Ve::Num(10.),
        Tok::Just(b!('Θ')) => Ve::Num(-1.),
        Tok::Just(b!('∞')) => Ve::Num(f64::INFINITY),
        Tok::Just(b!('█')) => Ve::Num(f64::NAN),
        Tok::Just(b!('ϕ')) => Ve::Snd(Vec::new()),
        Tok::VarVal(x) => Ve::Var(x),
        Tok::Chr(x) =>
            Ve::Num(if x <= 10 { -f64::from(x) } else { f64::from(x) }),
        Tok::Chr2(x, y) =>
            Ve::Snd(vec![Ve::Num(f64::from(x)), Ve::Num(f64::from(y))]),
        Tok::Num2(x, y) =>
            Ve::Num(f64::from(x)*253. + f64::from(y)),
        Tok::Num3(x, y, z) =>
            Ve::Num(f64::from(x)*253.*253. + f64::from(y)*253. + f64::from(z)),
        Tok::Num(l) => {
            let mut num = 0.;
            for i in l { num = num * 253. + f64::from(i) }
            Ve::Num(num)
        }
        Tok::HNum(x) => unsafe {
            // safety: HNums have only [0-9.]+, all are ascii characters
            Ve::Num(std::str::from_utf8_unchecked(&x).parse::<f64>().unwrap())
        },
        Tok::Str(x) =>
            Ve::Snd(x.iter().map(|&x| Ve::Num(f64::from(x))).collect()),
        Tok::Cst(x) => constant(x),
        _ => return None,
    })
}

fn parse_atom(code: &[Tok]) -> (usize, Option<Ve>) {
    if let Some(t) = code.first() {
        match t.clone() {
            Tok::Just(b'(') => {
                let mut slice = &code[1..];
                let (len, ev) = parse_expression(slice, usize::MAX); slice = &slice[len..];
                (
                    slice_offset(code, slice) + usize::from(matches!(slice[0], Tok::Just(b')'))),
                    Some(ev.unwrap_or(Ve::Num(f64::NAN)))
                )
            },
            Tok::Just(b!('♪')) => {
                let (len, t) = parse_thing(&code[1..]);
                if let Some(t) = t { (len+1, Some(match t {
                    Tg::Ve(ev) => Ve::Snd(vec![ev]),
                    Tg::Fe(ef) => Ve::Nom(ef),
                }))} else {(0, None)}
            },
            Tok::Just(s @ b!('┐''┘''┌''└')) => {
                let (len, ev) = parse_expression(&code[1..], match s {
                    b!('┐')=>2, b!('┘')=>3, b!('┌')=>4, b!('└')=>5, _=>unreachable!()
                });
                if ev.is_some() {(len + 1, ev)} else {(0, None)}
            }
            t => { let p = atom_token(t); (p.is_some() as usize, p) }
        }
    } else { (0, None) }
}

fn constant(thing: u8) -> Ve {
    match thing {
        b'a'..=b'z' => Ve::Num(match thing {
            b'a' => 12,     b'b' => 20,     b'c' => 99,     b'd' => 100,    b'e' => 999,
            b'f' => 1000,   b'g' => 9999,   b'h' => 10000,  b'i' => 100000, b'j' => 1000000,
            b'k' => 15,     b'l' => 16,     b'm' => 31,     b'n' => 32,     b'o' => 63,
            b'p' => 64,     b'q' => 127,    b'r' => 128,    b's' => 255,    b't' => 256,
            b'u' => 512,    b'v' => 1024,   b'w' => 2048,   b'x' => 4096,   b'y' => 32768,
            b'z' => 65536,  _ => unreachable!(),
        } as f64),
        _ => Ve::Num(f64::NAN),
    }
}


pub fn parse_thing(code: &[Tok]) -> (usize, Option<Tg>) {
    let mut thing: Tg;
    let mut slice = code;
    // try with a value
    let (len, oev) = parse_atom(slice); slice = &slice[len..];
    if let Some(ev) = oev {
        thing = Tg::Ve(ev);
    } else {
        // try with a function
        let (len, oef) = parse_function(slice); slice = &slice[len..];
        if let Some(ef) = oef {
            thing = Tg::Fe(ef);
        } else {
            // huh.
            return (0, None);
        }
    }
    
    if let Some(t) = slice.first() {
        // dyadic adverbs
        if let Tok::Just(c @ b!('║''╟''╧''╨''╤''╥''╕''╖''╒''╓''╪''╫')) = t {
            slice = &slice[1..];
            let (len, t) = parse_thing(slice); slice = &slice[len..];
            thing = Tg::Fe(Fe::Aav2{
                f: Box::new(thing),
                v: smallvec![*c],
                g: Box::new(t.unwrap_or(Tg::Ve(Ve::Num(f64::NAN))))});
        }
    }

    (slice_offset(code, slice), Some(thing))
}

// takes from iterator to make a strand. if it's only one element, it's just the one value. 
// if it's not a strand, returns None
fn strand(iter: &mut std::iter::Peekable<std::vec::IntoIter<Tg>>) -> Option<Ve> {
    let mut evs = Vec::new();
    while let Some(Tg::Ve(v)) = iter.peek() {
        evs.push(v.clone()); iter.next();
    }
    (!evs.is_empty()).then(|| if evs.len() == 1 { evs[0].clone() } else { Ve::Snd(evs) })
}

pub fn parse_expression(code: &[Tok], limit: usize) -> (usize, Option<Ve>) {
    let mut slice = code;
    // makes a list of things before processing
    let mut things = Vec::new();
    let mut last_was_stmt = false;
    while things.len() < limit {
        let (len, t) = parse_thing(slice);
        if let Some(thing) = t {
            if matches!(thing, Tg::Ve(_)) && last_was_stmt { break }
            things.push(thing);
        } else { break }
        last_was_stmt = matches!(slice.first(), Some(Tok::Stmt(_) | Tok::VarSet(_)));
        slice = &slice[len..];
    }
    //println!("{:?}", things);
    let mut iter = things.into_iter().peekable();

    let value = if let Some(start) = strand(&mut iter) {
        // Function application
        let mut value = start;
        while let Some(Tg::Fe(ef)) = iter.next() {
            value = if let Some(ev) = strand(&mut iter) {  // dyad
                Ve::Afn2{a: Box::new(value), f: ef, b: Box::new(ev)}
            } else {  // monad
                Ve::Afn1{a: Box::new(value), f: ef}
            }
        }
        value
    } else { match iter.next() {
        Some(Tg::Fe(ef)) => {
            // Train
            let mut value = if let Some(b) = strand(&mut iter) {
                Fe::Bind{f: Box::new(ef), b: Box::new(b)}
            } else { ef };
            while let Some(Tg::Fe(ef)) = iter.next() {
                value = if let Some(b) = strand(&mut iter) {  // dyad
                    Fe::Trn2{a: Box::new(value), f: Box::new(ef), b: Box::new(b)}
                } else {  // monad
                    Fe::Trn1{a: Box::new(value), f: Box::new(ef)}
                }
            }
            Ve::Nom(value)
        },
        Some(Tg::Ve(_)) => unreachable!(),
        None => return (0, None)
    }};
    (slice_offset(code, slice), Some(value))
}

fn parse_block(code: &[Tok]) -> (usize, Vec<Ve>) {
    let mut slice = code;
    let mut exps = Vec::new();
    loop {
        while let Some(Tok::Just(b!(';'))) = slice.first() { slice = &slice[1..]; }
        let (len, ev) = parse_expression(slice, usize::MAX); slice = &slice[len..];
        if let Some(ev) = ev {
            exps.push(ev);
        } else { break }
    }
    while let Some(Tok::Just(b!(';'))) = slice.first() { slice = &slice[1..]; }
    (slice_offset(code, slice), exps)
}

pub fn parse_file(code: &[Tok]) -> Vec<Ve> {
    let (len, exps) = parse_block(code);
    if code.len() > len {
        println!("unexpected token {:?}", code[len]);
    }
    exps
}