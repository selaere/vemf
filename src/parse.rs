use std::collections::HashSet;
use std::fmt::Display;

use smallvec::smallvec;
use crate::{b, Bstr};
use crate::codepage::tochars;
use crate::token::Tok;

use num_complex::Complex64 as c64;

macro_rules! step {
    ($code:ident) => { *$code = &$code[1..]; };
}

// Value expression
#[derive(Clone, Debug)]
pub enum Expr {
    Var(Bstr),
    Int(i64),
    Flt(c64),
    Snd(Vec<Expr>),  // strand
    Afn1 { a: Box<Expr>, f: Box<Expr>               },  // apply monadic function
    Afn2 { a: Box<Expr>, f: Box<Expr>, b: Box<Expr> },  // apply dyadic  function 
    SetVar(Bstr),
    Aav1 {               v: Bstr     , g: Box<Expr> }, // apply monadic adverb
    Aav2 { f: Box<Expr>, v: Bstr     , g: Box<Expr> }, // apply dyadic  adverb
    Bind {               f: Box<Expr>, b: Box<Expr> }, // +1
    Trn2 { a: Box<Expr>, f: Box<Expr>               }, // +/
    Trn3 { a: Box<Expr>, f: Box<Expr>, b: Box<Expr> }, // +/2
    Fork { a: Box<Expr>, f: Box<Expr>, b: Box<Expr> }, // └+/~
    Dfn  { s: Vec<Stmt>, cap: HashSet<Bstr> },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Discard(Expr),
    Return(Expr),
    Set(Expr, Bstr),
    Conj(Expr, Bstr),
    Cond(Expr, Box<Stmt>),
}

#[derive(Debug)]
pub enum Role {
    Noun, Verb
}
use Role::{Noun, Verb};
const NAN: Expr = Expr::Flt(c64::new(f64::NAN, f64::NAN));

impl Display for Expr {
    fn fmt(&self, m: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(v) => write!(m, ".{}", displayname(v)),
            Self::Int(n) => write!(m, "'{}", n),
            Self::Flt(n) => write!(m, "'{}", n),
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
            Self::Trn2 { a, f    } => write!(m, "[{} {}]", a, f),
            Self::Trn3 { a, f, b } => write!(m, "[{} {} {}]", a, f, b),
            Self::Fork { a, f, b } => write!(m, "└[{} {} {}]", a, f, b),
            Self::Dfn  { s: efs, .. } => {
                write!(m, "{{ ")?;
                for v in efs { write!(m, "{} ", v)?; }
                write!(m, "}}")?;
                Ok(())
            },
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, m: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Discard(e) => write!(m, "{}·", e),
            Stmt::Return(e) => write!(m, "{}◘", e),
            Stmt::Set(e, f) => write!(m, "{}→{}", e, displayname(f)),
            Stmt::Conj(e, f) => write!(m, "{}{}", e, displayname(f)),
            Stmt::Cond(i, t) => write!(m, "{}?{}", i, t),
        }
    }
}


fn displayname(bytes: &[u8]) -> String {
    if bytes.contains(&b' ') {
        format!("\"{}\"", tochars(bytes).replace('"', "\\\""))
    } else {
        tochars(bytes)
    }
}

fn word(code: &mut&[Tok], morphemes: &mut usize) -> Option<(Role, Expr)> {
    if *morphemes == 0 {return None}
    let Some(t) = code.first() else {return None};
    let (rol, val) = match t {
        Tok::Conj(c) => { step!(code); (Verb, Expr::Var(smallvec![*c])) },
        Tok::VarSet(v) => { step!(code); (Verb, Expr::SetVar(v.clone())) },
        Tok::VarAv1(name) => { step!(code);
            let word = word(code, morphemes);
            if let Some((_role, word)) = word {
                (Verb, Expr::Aav1{ v: name.clone(), g: Box::new(word) })
            } else {
                (Verb, Expr::Var(name.clone()))
            }
        },
        #[allow(const_item_mutation)]
        Tok::Just(b!('┘')) => { step!(code);
            let a = word(code, &mut usize::MAX).map_or(NAN, |x| x.1);
            let f = word(code, &mut usize::MAX).map_or(NAN, |x| x.1);
            let b = word(code, &mut usize::MAX).map_or(NAN, |x| x.1);
            (Verb, Expr::Fork{a: Box::new(a), f: Box::new(f), b: Box::new(b)})
        },
        #[allow(const_item_mutation)]
        Tok::Just(b!('└')) => { step!(code);
            let a = word(code, &mut usize::MAX).map_or(NAN, |x| x.1);
            let f = word(code, &mut usize::MAX).map_or(NAN, |x| x.1);
            let b = word(code, &mut 1         ).map_or(NAN, |x| x.1);
            (Verb, Expr::Fork{a: Box::new(a), f: Box::new(f), b: Box::new(b)})
        },
        Tok::Just(b'{') => { step!(code);
            let s = block(code);
            let mut vars = HashSet::new();
            for i in &s { i.capture(&mut vars) }
            if let Some(Tok::Just(b'}')) = code.first() { step!(code); }
            (Verb, Expr::Dfn {s, cap: vars})
        },
        Tok::VarFun(v) | Tok::VarAv2(v) => { step!(code); (Verb, Expr::Var(v.clone())) },
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
                Verb => arg
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
            (if *s == b!('│') {Verb} else {Noun}, e.unwrap_or(NAN))
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
            g: Box::new(word.map_or(NAN, |x| x.1)),
        }))
    }}
    Some((rol, val))
}

pub fn phrase_by_morphemes(code: &mut&[Tok], mut morphemes: usize) -> Vec<(Role, Expr)> {
    let mut phrase = Vec::new();
    let morphemes = &mut morphemes;
    loop {
        let mut backtrack: Option<&[Tok]> = None;
        if let Some(Tok::VarSet(_) | Tok::Conj(_)) = code.first() {
            let ptr = &**code;
            let word = word(code, morphemes);
            if let Some(word) = word { phrase.push(word); } else { break }
            if let Some(Tok::Just(b!('·'))) | None = code.first() {
                *code = ptr; phrase.pop(); break;
            }
            backtrack = Some(ptr);
            if *morphemes >= 1 {*morphemes -= 1} else {break}
        }
        let word = word(code, morphemes);
        //println!("{:?} {:?}", word, morphemes);
        if let Some(word) = word {
            if let Some(ptr) = backtrack { if let Noun = word.0 {
                *code = ptr; phrase.pop(); break;
            }}
            phrase.push(word);
        } else { break }
        if *morphemes >= 1 {*morphemes -= 1} else {break}
    };
    phrase
}

#[allow(const_item_mutation)]
pub fn phrase_by_words(code: &mut&[Tok], words: usize) -> Vec<(Role, Expr)> {
    let mut phrase = Vec::with_capacity(if words > 10 {0} else {words});
    while phrase.len() < words {
        let mut backtrack: Option<&[Tok]> = None;
        if let Some(Tok::VarSet(_) | Tok::Conj(_)) = code.first() {
            let ptr = &**code;
            let word = word(code, &mut usize::MAX);
            if let Some(word) = word { phrase.push(word); } else { break }
            if phrase.len() >= words { break }
            if let Some(Tok::Just(b!('·'))) | None = code.first() {
                *code = ptr; phrase.pop(); break;
            }
            backtrack = Some(ptr);
        }
        let word = word(code, &mut usize::MAX);
        //println!("{:?} {:?}", word, morphemes);
        if let Some(word) = word {
            if let Some(ptr) = backtrack { if let Noun = word.0 {
                *code = ptr; phrase.pop(); break;
            }}
            phrase.push(word);
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
        while let Some((Verb, ef)) = iter.next() {
            value = if let Some(ev) = strand(&mut iter) {  // dyad
                Expr::Afn2 {a: Box::new(value), f: Box::new(ef), b: Box::new(ev)}
            } else {  // monad
                Expr::Afn1 {a: Box::new(value), f: Box::new(ef)}
            }
        }
        value
    } else { match iter.next() {
        Some((Verb, ef)) => {
            // Train
            let mut value = if let Some(b) = strand(&mut iter) {
                Expr::Bind {f: Box::new(ef), b: Box::new(b)}
            } else { ef };
            while let Some((Verb, ef)) = iter.next() {
                value = if let Some(b) = strand(&mut iter) {  // dyad
                    Expr::Trn3 {a: Box::new(value), f: Box::new(ef), b: Box::new(b)}
                } else {  // monad
                    Expr::Trn2 {a: Box::new(value), f: Box::new(ef)}
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
        Tok::Just(c @ b'0'..=b'9') => Expr::Int(i64::from(c - b'0')),
        Tok::Just(b!('Φ')) => Expr::Int(10),
        Tok::Just(b!('Θ')) => Expr::Int(-1),
        Tok::Just(b!('∞')) => Expr::Flt(c64::new(f64::INFINITY, 0.)),
        Tok::Just(b!('Γ')) => Expr::Flt(c64::i()),
        Tok::Just(b!('█')) => NAN,
        Tok::Just(b!('ϕ')) => Expr::Snd(Vec::new()),
        Tok::VarVal(x) => Expr::Var(x),
        Tok::Chr(x) =>
            Expr::Int(if x <= 10 { -i64::from(x) } else { i64::from(x) }),
        Tok::Chr2(x, y) =>
            Expr::Snd(vec![Expr::Int(i64::from(x)), Expr::Int(i64::from(y))]),
        Tok::Num2(x, y) =>
            Expr::Int(i64::from(x)*253 + i64::from(y)),
        Tok::Num3(x, y, z) =>
            Expr::Int(i64::from(x)*253*253 + i64::from(y)*253 + i64::from(z)),
        Tok::Num(l) => {
            let mut num = 0;
            for i in l { num = num*253 + i64::from(i) }
            Expr::Int(num)
        }
        Tok::HNum(x) => {
            let num = std::str::from_utf8(&x).unwrap().parse::<f64>().unwrap();
            if num == num as i64 as f64 {
                Expr::Int(num as i64)
            } else {
                Expr::Flt(c64::new(num, 0.))
            }
        },
        Tok::Str(x) =>
            Expr::Snd(x.iter().map(|&x| Expr::Int(i64::from(x))).collect()),
        _ => return None,
    })
}

impl Expr {
fn capture(&self, vars: &mut HashSet<Bstr>) { // yeah...
    match self {
        Expr::Var(n) | Expr::SetVar(n) => { vars.insert(n.clone()); },
        Expr::Int(_) | Expr::Flt(_) => (),
        Expr::Snd(l) => for i in l { i.capture(vars) }
        Expr::Afn1 { a, f    } => { a.capture(vars); f.capture(vars); },
        Expr::Afn2 { a, f, b } => { a.capture(vars); f.capture(vars); b.capture(vars); },
        Expr::Aav1 {    v, g } => { vars.insert(v.clone()); g.capture(vars); },
        Expr::Aav2 { f, v, g } => { vars.insert(v.clone()); f.capture(vars); g.capture(vars); },
        Expr::Bind { f, b } => { f.capture(vars); b.capture(vars); },
        Expr::Trn2 { a, f } => { a.capture(vars); f.capture(vars); },
        Expr::Trn3 { a, f, b } => { a.capture(vars); f.capture(vars); b.capture(vars); },
        Expr::Fork { a, f, b } => { a.capture(vars); f.capture(vars); b.capture(vars); },
        Expr::Dfn { cap, .. } => { vars.extend(cap.iter().cloned()); }
    }
}
}


impl Stmt {
    fn capture(&self, vars: &mut HashSet<Bstr>) { // yeah...
        match self {
            Stmt::Discard(e) => { e.capture(vars); },
            Stmt::Return(e) => { e.capture(vars); },
            Stmt::Set(e, f) =>  { e.capture(vars); vars.insert(f.clone()); },
            Stmt::Conj(e, v) => { e.capture(vars); vars.insert(v.clone()); },
            Stmt::Cond(i, t) => { i.capture(vars); t.capture(vars); },
        }
    }
}

fn parse_stmt(code: &mut&[Tok], ev: Expr) -> Option<Stmt> {
    Some(match code.first() {
        Some(Tok::Conj(c)) => { step!(code); 
            Stmt::Conj(ev, smallvec![*c]) },
        Some(Tok::VarSet(v)) => { step!(code); 
            Stmt::Set(ev, v.clone()) },
        Some(Tok::Just(b!('◘'))) => { step!(code);
            Stmt::Return(ev) },
        Some(Tok::Just(b!('·'))) => { step!(code);
            Stmt::Discard(ev) },
        Some(Tok::Just(b!('}'))) | None => {
            Stmt::Return(ev) },
        Some(Tok::Just(b!('?'))) => { step!(code);
            let ev2 = phrase_to_expr(phrase(code)).unwrap_or(NAN);
            let Some(block) = parse_stmt(code, ev2) else {return None};
            Stmt::Cond(ev, Box::new(block))
        },
        _ => return None,
    })
}


pub fn block(code: &mut&[Tok]) -> Vec<Stmt> {
    let mut exps = Vec::new();
    loop {
        while let Some(Tok::Just(b!('·'))) = code.first() { step!(code); }
        if let Some(Tok::Just(b!('}'))) | None = code.first() { break };
        let ev = phrase_to_expr(phrase(code)).unwrap_or(NAN);
        let Some(stmt) = parse_stmt(code, ev) else { break };
        exps.push(stmt);
    }
    exps
}

pub fn parse(code: &[Tok]) -> Vec<Stmt> {
    let mut slice = code;
    let exps = block(&mut slice);
    if !slice.is_empty() {
        let num = code.len() - slice.len();
        println!("unexpected token {:?} at {}", code[num], num);
    }
    exps
}