use alloc::fmt::Display;

use crate::prelude::*;
use crate::codepage::tochars;
use crate::token::Tok;

use num_complex::Complex64 as c64;

fn step(t: &mut&[Tok]) { *t = &t[1..]; }

// Value expression
#[derive(Clone, Debug)]
pub enum Expr {
    Var(Bstr),
    Int(i64), Flt(c64),
    Snd(Vec<Expr>),  // strand
    Afn1(Box<Expr>, Box<Expr>),            // 1-
    Afn2(Box<Expr>, Box<Expr>, Box<Expr>), // 1+2
    SetVar(Bstr), MutVar(Bstr),
    Aav1(           Bstr, Box<Expr>), // ╕@
    Aav2(Box<Expr>, Bstr, Box<Expr>), // ~╙↑
    Bind(Box<Expr>, Box<Expr>), // +1
    Trn2(Box<Expr>, Box<Expr>), // +/
    Trn3(Box<Expr>, Box<Expr>, Box<Expr>), // +/2
    Fork(Box<Expr>, Box<Expr>, Box<Expr>), // └+/~
    Dfn { s: Vec<Stmt>, cap: HashSet<Bstr> },
    Block(Vec<Stmt>),
}
use Expr::*;

#[derive(Debug, Clone)]
pub enum Stmt {
    Discard(Expr), Return(Expr),
    Loc(Expr, Bstr), Mut(Expr, Bstr),
    DelLoc(Bstr),    DelMut(Bstr),
    Cond(Expr, Box<Stmt>),
}

#[derive(Debug)]
pub enum Role { Noun, Verb }
use Role::{Noun, Verb};

const NAN: Expr = Flt(c64::new(f64::NAN, f64::NAN));

impl Display for Expr {
fn fmt(&self, m: &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result {
    match self {
        Var(v) => write!(m, ".{}", display(v)),
        Int(n) => write!(m, "'{n}"),
        Flt(n) => write!(m, "'{n}"),
        Snd(l) => {
            write!(m, "(")?;
            for v in l { write!(m, "{v}")?; }
            write!(m, ")")?;
        Ok(()) },
        Afn1(a, f) => write!(m, "({a} {f})"),
        Afn2(a, f, b) => write!(m, "({a} {f} {b})"),
        SetVar(v) => write!(m, "→{}", display(v)),
        MutVar(v) => write!(m, "↔{}", display(v)),
        Aav1(v, g) => write!(m, "[•{} {}]", display(v), g),
        Aav2(f, v, g) => write!(m, "[{} ○{} {}]", f, display(v), g),
        Bind(f, b) => write!(m, "[{f} with {b}]"),
        Trn2(a, f) => write!(m, "[{a} {f}]"),
        Trn3(a, f, b) => write!(m, "[{a} {f} {b}]"),
        Fork(a, f, b) => write!(m, "└[{a} {f} {b}]"),
        Dfn  { s, .. } => {
            write!(m, "{{ ")?;
            for v in s { write!(m, "{v} ")?; }
            write!(m, "}}")?;
        Ok(()) },
        Block(s) => {
            write!(m, "[ ")?;
            for v in s { write!(m, "{v} ")?; }
            write!(m, "]")?;
        Ok(()) }
    }
}
}

impl Display for Stmt {
fn fmt(&self, m: &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result { match self {
    Stmt::Discard(e) => write!(m, "{e}·"),
    Stmt::Return(e) => write!(m, "{e}◘"),
    Stmt::Loc(e, f) => write!(m, "{}→{}·", e, display(f)),
    Stmt::Mut(e, f) => write!(m, "{}↔{}·", e, display(f)),
    Stmt::DelLoc(f) => write!(m, "→{}·", display(f)),
    Stmt::DelMut(f) => write!(m, "↔{}·", display(f)),
    Stmt::Cond(i, t) => write!(m, "{i}?{t}"),
}}
}

fn display(bytes: &[u8]) -> String {
    if bytes.contains(&b' ') {
        format!("\"{}\"", tochars(bytes).replace('"', "\\\""))
    } else { tochars(bytes) }
}

fn word_full(t: &mut&[Tok]) -> Option<(Role, Expr)> { word_cst(t, usize::MAX) }

fn word_cst(t: &mut&[Tok], mut morphemes: usize) -> Option<(Role, Expr)>{
    let ptr = &mut morphemes; word(t, ptr)
}

fn word(t: &mut&[Tok], morphemes: &mut usize) -> Option<(Role, Expr)> {
    if *morphemes == 0 {return None}
    let (rol, mut val) = match t.first()? {
        Tok::VSet(v) => { step(t); (Verb, SetVar(v.c())) },
        Tok::VMut(v) => { step(t); (Verb, MutVar(v.c())) },
        Tok::VAv1(name) => { step(t);
            let word = word(t, morphemes);
            match word {
                Some((_role, word)) => (Verb, Aav1(name.c(), bx(word))),
                None => (Verb, Var(name.c())),
            }
        },
        Tok::Just(b!('└')) => { step(t);
            let a = word_full(t).map_or(NAN, |x| x.1);
            let f = word_full(t).map_or(NAN, |x| x.1);
            let b = word(t, morphemes).map_or(NAN, |x| x.1);
            (Verb, Fork(bx(a), bx(f), bx(b)))
        },
        Tok::Just(b'{') => { step(t);
            let s = block(t);
            let mut vars = HashSet::new();
            for i in &s { i.capture(&mut vars) }
            if let Some(Tok::Just(b'}')) = t.first() { step(t); }
            (Verb, Dfn {s, cap: vars})
        },
        Tok::Just(b'[') => { step(t);
            let s = block(t);
            if let Some(Tok::Just(b']')) = t.first() { step(t); }
            (Noun, Block(s))
        }
        Tok::VVerb(v) => { step(t); (Verb, Var(v.c())) },
        Tok::Just(b'(') => { step(t);
            let expr = phrase_to_expr(phrase(t)).unwrap_or(Snd(vec![]));
            if let Some(Tok::Just(b')')) = t.first() { step(t); }
            (Noun, expr)
        },
        Tok::Just(b!('♪')) => { step(t);
            let (rol, arg) = word_full(t).unwrap_or((Noun, NAN));
            (Noun, match rol {
                Noun => Snd(vec![arg]),
                Verb => arg
            })
        },
        Tok::Just(s @ b!('┌''│''├''╞''╟''╠''┤''╡''╢''╣')) => { step(t);
            macro_rules! bl { [$($b:tt)+] => {[$(b!($b)),+]} }
            let p = phrase_by_morphemes(t, 
                bl!['┌''│''├''╞''╟''╠''┤''╡''╢''╣'].iter().position(|x| x == s).unwrap() + 1);
            let e = phrase_to_expr(p);
            (if *s == b!('┌') {Verb} else {Noun}, e.unwrap_or(NAN))
        },
        tok => if let Some(p) = value_token(tok.c()) { step(t); (Noun, p) } else {return None}
    };
    if *morphemes > 1 { if let Some(Tok::VAv2(l, n)) = t.first() {
        *morphemes -= 1;
        step(t);
        let word = word(t, morphemes);
        for i in l.iter().rev() { val = Aav1(i.c(), bx(val)) }
        return Some((Verb, Aav2( bx(val), n.c(), bx(word.map_or(NAN, |x| x.1)) )))
    }}
    Some((rol, val))
}

pub fn phrase_by_morphemes(t: &mut&[Tok], mut morphemes: usize) -> Vec<(Role, Expr)> {
    let mut phrase = Vec::new();
    let morphemes = &mut morphemes;
    loop {
        if let Some(word) = word(t, morphemes) { phrase.push(word); } else { break }
        if *morphemes >= 1 {*morphemes -= 1} else {break}
    };
    phrase
}

pub fn phrase(t: &mut&[Tok]) -> Vec<(Role, Expr)> {
    let mut phrase = Vec::new();
    while let Some(word) = word_full(t) { phrase.push(word); }
    phrase
}

// takes from iterator to make a strand. if it's only one element, it's just the one value. 
// if it's not a strand, returns None
fn strand(iter: &mut iter::Peekable<alloc::vec::IntoIter<(Role, Expr)>>) -> Option<Expr> {
    let mut evs = Vec::new();
    while let Some((Noun, v)) = iter.peek() {
        evs.push(v.c()); iter.next();
    }
    (!evs.is_empty()).then(|| if evs.len() == 1 { evs[0].c() } else { Snd(evs) })
}


fn phrase_to_expr(things: Vec<(Role, Expr)>) -> Option<Expr> {
    let mut iter = things.into_iter().peekable();
    Some(if let Some(start) = strand(&mut iter) { // Function application
        let mut value = start;
        while let Some((Verb, ef)) = iter.next() { value = match strand(&mut iter) {
            Some(ev) => Afn2(bx(value), bx(ef), bx(ev)),
            None     => Afn1(bx(value), bx(ef)),
        }}
    value } else if let Some((Verb, f)) = iter.next() { // Train
        let mut value = if let Some(b) = strand(&mut iter) { Bind(bx(f), bx(b)) } else { f };
        while let Some((Verb, f)) = iter.next() { value = match strand(&mut iter) {
            Some(b) => Trn3(bx(value), bx(f), bx(b)),
            None    => Trn2(bx(value), bx(f))
        }}
    value } else { return None })
}

fn numberise(mut num: i64) -> Expr {
    if num % 2 == 1 { num = 1-num; }
    Int(num / 2)
}

fn value_token(chr: Tok) -> Option<Expr> {
    Some(match chr {
        Tok::Just(c @ b'0'..=b'9') => Int(i64::from(c - b'0')),
        Tok::Just(b!('Φ')) => Int(10),
        Tok::Just(b!('Θ')) => Int(-1),
        Tok::Just(b!('∞')) => Flt(c64::new(f64::INFINITY, 0.)),
        Tok::Just(b!('Γ')) => Flt(c64::i()),
        Tok::Just(b!('█')) => NAN,
        Tok::Just(b!('φ')) => Snd(Vec::new()),
        Tok::Just(b!('π')) => Flt(c64::new(core::f64::consts::PI, 0.)),
        Tok::VNoun(x) => Var(x),
        Tok::Chr(x) => Int(i64::from(x)),
        Tok::Chr2(x, y) => Snd(vec![Int(i64::from(x)), Int(i64::from(y))]),
        Tok::Num2(   y, z) => numberise(                       i64::from(y)*253 + i64::from(z)),
        Tok::Num3(x, y, z) => numberise(i64::from(x)*253*253 + i64::from(y)*253 + i64::from(z)),
        Tok::Num(l) => {
            let mut num = 0;
            for i in l { num = num*253 + i64::from(i) }
        numberise(num) }
        Tok::HNum(x) => {
            let str = core::str::from_utf8(&x).unwrap();
            str.parse::<i64>().map_or_else(|_| Flt(c64::new(str.parse::<f64>().unwrap(), 0.)), Int)
        },
        Tok::Str(x) => Snd(x.iter().map(|&x| Int(i64::from(x))).collect()),
        _ => return None,
    })
}

impl Expr {
fn capture(&self, vars: &mut HashSet<Bstr>) { match self { // yeah...
    Var(n) | SetVar(n) | MutVar(n) => { vars.insert(n.c()); },
    Int(_) | Flt(_) => (),
    Snd(l) => for i in l { i.capture(vars) }
    Afn1(a, f)    => { a.capture(vars); f.capture(vars); },
    Afn2(a, f, b) => { a.capture(vars); f.capture(vars); b.capture(vars); },
    Aav1(v, g)    => { vars.insert(v.c()); g.capture(vars); },
    Aav2(f, v, g) => { vars.insert(v.c()); f.capture(vars); g.capture(vars); },
    Bind(f, b)    => { f.capture(vars); b.capture(vars); },
    Trn2(a, f)    => { a.capture(vars); f.capture(vars); },
    Trn3(a, f, b) => { a.capture(vars); f.capture(vars); b.capture(vars); },
    Fork(a, f, b) => { a.capture(vars); f.capture(vars); b.capture(vars); },
    Dfn { cap, .. } => { vars.extend(cap.iter().cloned()); }
    Block(s) => { for i in s {i.capture(vars)} }
}}
}

impl Stmt {
fn capture(&self, vars: &mut HashSet<Bstr>) { match self {
    Self::Discard(e) | Self::Return(e) | Self::Loc(e, _) | Self::Mut(e, _) => {
        e.capture(vars); },
    Self::DelLoc(_) | Self::DelMut(_) => { },
    Self::Cond(i, t) => { i.capture(vars); t.capture(vars); },
}}
}

fn parse_stmt(t: &mut&[Tok], expr: Option<Expr>) -> Option<Stmt> {
    Some(match t.first() {
        Some(Tok::VSetS(v)) => { step(t); match expr {
            Some(e) => Stmt::Loc(e, v.c()),
            None    => Stmt::DelLoc(v.c()),
        }},
        Some(Tok::VMutS(v)) => { step(t); match expr {
            Some(e) => Stmt::Mut(e, v.c()),
            None    => Stmt::DelMut(v.c()),
        }},
        Some(Tok::Just(b!('◘'))) => { step(t); Stmt::Return (expr.unwrap_or(NAN)) },
        Some(Tok::Just(b!('·'))) => { step(t); Stmt::Discard(expr.unwrap_or(NAN)) },
        Some(Tok::Just(b!('}'']'))) | None => { Stmt::Return(expr.unwrap_or(NAN)) },
        Some(Tok::Just(b!('?'))) => { step(t);
            let ev2 = phrase_to_expr(phrase(t));
            Stmt::Cond(expr.unwrap_or(NAN), bx(parse_stmt(t, ev2)?))
        },
        _ => return None,
    })
}

pub fn block(t: &mut&[Tok]) -> Vec<Stmt> {
    let mut exps = Vec::new();
    loop {
        while let Some(Tok::Just(b!('·'))) = t.first() { step(t); }
        if let Some(Tok::Just(b!('}'']'))) | None = t.first() { break };
        let ev = phrase_to_expr(phrase(t));
        if let Some(stmt) = parse_stmt(t, ev) {
            exps.push(stmt);
        } else { break }
    }
    exps
}

pub fn parse(t: &[Tok]) -> Vec<Stmt> {
    let mut slice = t;
    let exps = block(&mut slice);
    if !slice.is_empty() {
        let num = t.len() - slice.len();
        panic!("unexpected token {:?} at {}", t[num], num);
    }
    exps
}