use std::collections::HashSet;
use std::fmt::Display;

use smallvec::smallvec;
use crate::{b, Bstr};
use crate::codepage::tochars;
use crate::token::Tok;

macro_rules! step {
    ($code:ident) => { *$code = &$code[1..]; };
}

// Value expression
#[derive(Clone, Debug)]
pub enum Expr {
    Var(Bstr),
    Num(f64),
    Snd(Vec<Expr>),  // strand
    Afn1 { a: Box<Expr>, f: Box<Expr>               },  // apply monadic function
    Afn2 { a: Box<Expr>, f: Box<Expr>, b: Box<Expr> },  // apply dyadic  function 
    SetVar(Bstr),
    Aav1 {              v: Bstr     , g: Box<Expr>}, //     apply monadic adverb
    Aav2 {f: Box<Expr>, v: Bstr     , g: Box<Expr>}, //     apply dyadic  adverb
    Bind {              f: Box<Expr>, b: Box<Expr>}, // +1
    Trn1 {a: Box<Expr>, f: Box<Expr>              }, // +/
    Trn2 {a: Box<Expr>, f: Box<Expr>, b: Box<Expr>}, // +/2
    Fork {a: Box<Expr>, f: Box<Expr>, b: Box<Expr>}, // ┼+/~
    Dfn  {s: Vec<Expr>, cap: HashSet<Bstr>},
}

#[derive(Debug)]
pub enum Role {
    Noun, Verb, Conj
}
use Role::{Noun, Verb, Conj};
const NAN: Expr = Expr::Num(f64::NAN);

impl Display for Expr {
    fn fmt(&self, m: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(v) => write!(m, ".{}", displayname(v)),
            Self::Num(n) => write!(m, "'{}", n),
            Self::Snd(l) => {
                write!(m, "(")?;
                for v in l { write!(m, "{}", v)?; }
                write!(m, ")")?;
                Ok(())
            },
            Self::Afn1 { a, f } => write!(m, "({} {})", a, f),
            Self::Afn2 { a, f, b } => write!(m, "({} {} {})", a, f, b),
            Self::SetVar(v) => write!(m, "→{}", displayname(v)),
            Self::Aav1 {    v, g } => write!(m, "[•{} {}]", displayname(v), g),
            Self::Aav2 { f, v, g } => write!(m, "[{} ○{} {}]", f, displayname(v), g),
            Self::Bind {    f, b } => write!(m, "[{} with {}]", f, b),
            Self::Trn1 { a, f    } => write!(m, "[{} {}]", a, f),
            Self::Trn2 { a, f, b } => write!(m, "[{} {} {}]", a, f, b),
            Self::Fork { a, f, b } => write!(m, "┼[{} {} {}]", a, f, b),
            Self::Dfn  { s: efs, .. } => {
                write!(m, "{{ ")?;
                for v in efs { write!(m, "{}; ", v)?; }
                write!(m, "}}")?;
                Ok(())
            },
        }
    }
}


fn displayname(bytes: &[u8]) -> String {
    if bytes.contains(&b' ') {
        format!("\"{}\"", tochars(bytes).replace('"', "\""))
    } else {
        tochars(bytes)
    }
}

fn word(code: &mut&[Tok], morphemes: &mut usize) -> Option<(Role, Expr)> {
    if *morphemes == 0 {return None}
    if let Some(t) = code.first() {
        let (rol, val) = match t {
            Tok::Stmt(c) => { step!(code); (Conj, Expr::Var(smallvec![*c])) },
            Tok::VarSet(v) => { step!(code); (Conj, Expr::SetVar(v.clone())) },
            Tok::VarAv1(name) => { step!(code);
                let word = word(code, morphemes);
                (Verb, Expr::Aav1{
                    v: name.clone(),
                    g: Box::new(word.map(|x| x.1).unwrap_or(NAN))
                })
            },
            #[allow(const_item_mutation)]
            Tok::Just(b!('┘')) => { step!(code);
                let a = word(code, &mut usize::MAX).map(|x| x.1).unwrap_or(NAN);
                let f = word(code, &mut usize::MAX).map(|x| x.1).unwrap_or(NAN);
                let b = word(code, &mut usize::MAX).map(|x| x.1).unwrap_or(NAN);
                (Verb, Expr::Fork{a: Box::new(a), f: Box::new(f), b: Box::new(b)})
            },
            #[allow(const_item_mutation)]
            Tok::Just(b!('└')) => { step!(code);
                let a = word(code, &mut usize::MAX).map(|x| x.1).unwrap_or(NAN);
                let f = word(code, &mut usize::MAX).map(|x| x.1).unwrap_or(NAN);
                let b = word(code, &mut 1         ).map(|x| x.1).unwrap_or(NAN);
                (Verb, Expr::Fork{a: Box::new(a), f: Box::new(f), b: Box::new(b)})
            },
            Tok::Just(b'{') => { step!(code);
                let s = block(code);
                let mut vars = HashSet::new();
                for i in &s { i.capture(&mut vars) }
                if let Some(Tok::Just(b'}')) = code.first() { step!(code); }
                (Noun, Expr::Dfn {s, cap: vars})
            },
            Tok::VarFun(v) | Tok::VarAv2(v) => {step!(code); (Verb, Expr::Var(v.clone()))},
            Tok::Just(b'(') => { step!(code);
                let expr = phrase_to_expr(phrase(code)).unwrap_or(Expr::Snd(vec![]));
                if let Some(Tok::Just(b')')) = code.first() { step!(code); }
                (Noun, expr)
            },
            Tok::Just(b!('♪')) => { step!(code);
                #[allow(const_item_mutation)]
                let (rol, arg) = word(code, &mut usize::MAX).unwrap_or((Noun, NAN));
                (Noun, match rol {
                    Noun => Expr::Snd(vec![arg]),
                    Verb | Conj => arg
                })
            },
            Tok::Just(s @ b!('┐''┤''╡''╢''╣')) => { step!(code);
                let p = phrase_by_words(code, match s {
                    b!('┐')=>2, b!('┤')=>3, b!('╡')=>4, b!('╢')=>5, b!('╣')=>6,
                _ => unreachable!()});
                let e = phrase_to_expr(p);
                (Noun, e.unwrap_or(NAN))
            },
            Tok::Just(s @ b!('│''┌''├''╞''╟''╠')) => { step!(code);
                let p = phrase_by_morphemes(code, match s {
                    b!('│')=>1, b!('┌')=>2, b!('├')=>3, b!('╞')=>4, b!('╟')=>5, b!('╠')=>6,
                _ => unreachable!()});
                let e = phrase_to_expr(p);
                (Noun, e.unwrap_or(NAN))
            },
            t => if let Some(p) = value_token(t.clone()) { 
                step!(code); (Noun, p)
            } else {return None}
        };
        if *morphemes > 1 { if let Some(Tok::VarAv2(n)) = code.first() {
            *morphemes -= 1;
            step!(code);
            let word = word(code, morphemes);
            return Some((Verb, Expr::Aav2{
                f: Box::new(val),
                v: n.clone(),
                g: Box::new(word.map(|x| x.1).unwrap_or(NAN)),
            }))
        }}
        Some((rol, val))
    } else {None}
}

pub fn phrase_by_morphemes(code: &mut&[Tok], mut morphemes: usize) -> Vec<(Role, Expr)> {
    let mut phrase = Vec::new();
    let mut last_was_conj = false;
    let morphemes = &mut morphemes;
    loop {
        // we need to make another reference in case we have to backtrack in case we get a Conj
        let copy = &mut &**code;
        let word = word(copy, morphemes);
        //println!("{:?} {:?}", word, morphemes);
        if let Some(word) = word {
            if last_was_conj && matches!(word.0, Noun) { break }
            last_was_conj = matches!(word.0, Conj);
            *code = copy; phrase.push(word);
        } else { break }
        if *morphemes >= 1 {*morphemes -= 1} else {break}
    };
    phrase
}

pub fn phrase_by_words(code: &mut&[Tok], words: usize) -> Vec<(Role, Expr)> {
    let mut phrase = Vec::with_capacity(if words > 10 {0} else {words});
    let mut last_was_conj = false;
    while phrase.len() < words {
        // we need to make another reference in case we have to backtrack in case we get a Conj
        let copy = &mut &**code;
        #[allow(const_item_mutation)]
        let word = word(copy, &mut usize::MAX);
        //println!("{:?} {:?}", word, morphemes);
        if let Some(word) = word {
            if last_was_conj && matches!(word.0, Noun) { break }
            last_was_conj = matches!(word.0, Conj);
            *code = copy; phrase.push(word);
        } else { break }
    };
    phrase

}

pub fn phrase(code: &mut&[Tok]) -> Vec<(Role, Expr)> {
    phrase_by_words(code, usize::MAX)
}


// takes from iterator to make a strand. if it's only one element, it's just the one value. 
// if it's not a strand, returns None
fn strand(iter: &mut std::iter::Peekable<std::vec::IntoIter<(Role, Expr)>>) -> Option<Expr> {
    let mut evs = Vec::new();
    while let Some((Noun, v)) = iter.peek() {
        evs.push(v.clone()); iter.next();
    }
    (!evs.is_empty()).then(|| if evs.len() == 1 { evs[0].clone() } else { Expr::Snd(evs) })
}


fn phrase_to_expr(things: Vec<(Role, Expr)>) -> Option<Expr> {
    let mut iter = things.into_iter().peekable();
    Some(if let Some(start) = strand(&mut iter) {
        // Function application
        let mut value = start;
        while let Some((Verb | Conj, ef)) = iter.next() {
            value = if let Some(ev) = strand(&mut iter) {  // dyad
                Expr::Afn2 {a: Box::new(value), f: Box::new(ef), b: Box::new(ev)}
            } else {  // monad
                Expr::Afn1 {a: Box::new(value), f: Box::new(ef)}
            }
        }
        value
    } else { match iter.next() {
        Some((Verb | Conj, ef)) => {
            // Train
            let mut value = if let Some(b) = strand(&mut iter) {
                Expr::Bind {f: Box::new(ef), b: Box::new(b)}
            } else { ef };
            while let Some((Verb | Conj, ef)) = iter.next() {
                value = if let Some(b) = strand(&mut iter) {  // dyad
                    Expr::Trn2 {a: Box::new(value), f: Box::new(ef), b: Box::new(b)}
                } else {  // monad
                    Expr::Trn1 {a: Box::new(value), f: Box::new(ef)}
                }
            }
            value
        },
        Some((Noun, _)) => unreachable!(),
        None => return None
    }})
}


fn value_token(chr: Tok) -> Option<Expr> {
    Some(match chr {
        Tok::Just(c @ b'0'..=b'9') => Expr::Num(f64::from(c - b'0')),
        Tok::Just(b!('Φ')) => Expr::Num(10.),
        Tok::Just(b!('Θ')) => Expr::Num(-1.),
        Tok::Just(b!('∞')) => Expr::Num(f64::INFINITY),
        Tok::Just(b!('█')) => NAN,
        Tok::Just(b!('ϕ')) => Expr::Snd(Vec::new()),
        Tok::VarVal(x) => Expr::Var(x),
        Tok::Chr(x) =>
            Expr::Num(if x <= 10 { -f64::from(x) } else { f64::from(x) }),
        Tok::Chr2(x, y) =>
            Expr::Snd(vec![Expr::Num(f64::from(x)), Expr::Num(f64::from(y))]),
        Tok::Num2(x, y) =>
            Expr::Num(f64::from(x)*253. + f64::from(y)),
        Tok::Num3(x, y, z) =>
            Expr::Num(f64::from(x)*253.*253. + f64::from(y)*253. + f64::from(z)),
        Tok::Num(l) => {
            let mut num = 0.;
            for i in l { num = num*253. + f64::from(i) }
            Expr::Num(num)
        }
        Tok::HNum(x) => unsafe {
            // safety: HNums have only [0-9.]+, all are ascii characters
            Expr::Num(std::str::from_utf8_unchecked(&x).parse::<f64>().unwrap())
        },
        Tok::Str(x) =>
            Expr::Snd(x.iter().map(|&x| Expr::Num(f64::from(x))).collect()),
        _ => return None,
    })
}

impl Expr {
fn capture(&self, vars: &mut HashSet<Bstr>) { // yeah...
    match self {
        Expr::Var(n) | Expr::SetVar(n) => { vars.insert(n.clone()); },
        Expr::Num(_) => (),
        Expr::Snd(l) => for i in l { i.capture(vars) }
        Expr::Afn1 { a, f    } => { a.capture(vars); f.capture(vars); },
        Expr::Afn2 { a, f, b } => { a.capture(vars); f.capture(vars); b.capture(vars); },
        Expr::Aav1 { v, g } => { vars.insert(v.clone()); g.capture(vars); },
        Expr::Aav2 { f, v, g } => { vars.insert(v.clone()); f.capture(vars); g.capture(vars); },
        Expr::Bind { f, b } => { f.capture(vars); b.capture(vars); },
        Expr::Trn1 { a, f } => { a.capture(vars); f.capture(vars); },
        Expr::Trn2 { a, f, b } => { a.capture(vars); f.capture(vars); b.capture(vars); },
        Expr::Fork { a, f, b } => { a.capture(vars); f.capture(vars); b.capture(vars); },
        Expr::Dfn { cap, .. } => { vars.extend(cap.iter().cloned()); }
    }
}
}


pub fn block(code: &mut&[Tok]) -> Vec<Expr> {
    let mut exps = Vec::new();
    loop {
        while let Some(Tok::Just(b!('·'))) = code.first() { step!(code); }
        let mut phrase = phrase(code);
        let ev = match phrase.pop() {
            Some((Conj, c)) => phrase_to_expr(phrase).map(|x| Expr::Afn1{a: Box::new(x), f: Box::new(c.clone())}),
            Some(els) => { phrase.push(els); phrase_to_expr(phrase) }
            None => None
        };
        if let Some(ev) = ev {
            exps.push(ev);
        } else { break }
    }
    while let Some(Tok::Just(b!('·'))) = code.first() { step!(code); }
    exps
}

pub fn parse(code: &[Tok]) -> Vec<Expr> {
    let mut slice = code;
    let exps = block(&mut slice);
    if !slice.is_empty() {
        println!("unexpected token {:?}", code[code.len() - slice.len()]);
    }
    exps
}