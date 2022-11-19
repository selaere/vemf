mod intrn; mod list; mod adverb; mod disp; mod val;

use crate::{parse::{Expr, Stmt}, Bstr};
use std::{collections::HashMap, rc::Rc};
use adverb::AvT;

const STDLIB: &str = include_str!("../std.vemf");

pub use num_complex::Complex64 as c64;

pub const NAN: Val = Num(c64::new(f64::NAN, f64::NAN));

pub type Frame = HashMap<Bstr, Val>;

pub trait Interface<'io> {
    fn  input(&mut self, n: usize) -> Option<Box<dyn std::io::BufRead + 'io>>;
    fn output(&mut self, n: usize) -> Option<Box<dyn std::io::Write   + 'io>>;
}


pub struct NoIO;

impl<'io> Interface<'io> for NoIO {
    fn  input(&mut self, _: usize) -> Option<Box<dyn std::io::BufRead>> { None }
    fn output(&mut self, _: usize) -> Option<Box<dyn std::io::Write  >> { None }
}

/// vemf interpreter state
pub struct Env<'io> {
    pub stack: Vec<Frame>,
    pub interface: Box<dyn Interface<'io> + 'io>,
}

#[macro_export]
macro_rules! or_nan { ($expr:expr) => {
    match $expr {
        Some(x) => x,
        None => return NAN,
    }
} }

/// represents a vemf value
#[derive(Clone, Debug)]
pub enum Val {
    Num(c64),
    Int(i64),
    Lis { l: Rc<Vec<Val>>, fill: Rc<Val> },
    FSet(Bstr), FCng(Bstr),
    Dfn { loc: Rc<HashMap<Bstr, Val>>, s: Rc<[Stmt]> },
    Bind{ f: Rc<Val>, b: Rc<Val> },
    Trn2{ a: Rc<Val>, f: Rc<Val> },
    Trn3{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Fork{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Av(AvT, Option<Rc<Val>>, Rc<Val>),
    AvBuilder(AvT),
    Cycle,     DCycle(Rc<Vec<Val>>),
    Err(i32), // exit code
    Add, Sub, Mul, Div, DivE, Mod, Pow, Log, Lt, Gt, Eq, And, Or, Max, Min, Atanb, Approx, BAnd, BOr, BXor, Gamma,
    Gcd, Lcm, Binom, Get, Set, Call,
    Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan, Sign, BNot, BRepr,
    Complex, Cis, Real, Imag, Conj, Arg,
    Left, Right, Len, Shape, Index, Iota, Pair, Enlist, Ravel, Concat, Reverse, GetFill, SetFill,
    Print, Println, Exit, Format, Numfmt, Parse, Out, In, FromUtf8, ToUtf8,
    Takeleft, Takeright, Dropleft, Dropright, Replist, Match, Deal, Sample, Replicate,
    GradeUp, GradeDown, SortUp, SortDown, BinsUp, BinsDown, Encode, FromCp, ToCp, Group,
    LoadIntrinsics,
}


use Val::{Lis, Num, Int};
impl Default for Val {
    fn default() -> Self { NAN }
}

impl<'io> Env<'io> {
    
    pub fn new<'a>() -> Env<'a> {
        let mut locals = HashMap::new();
        locals.insert(Bstr::from(&b"loadintrinsics"[..]), Val::LoadIntrinsics);
        Env::from_frame(locals)
    }

    pub fn from_frame<'a>(frame: Frame) -> Env<'a> {
        Env { stack: vec![frame], interface: Box::new(NoIO) }
    }

    pub fn locals(&self) -> &Frame { self.stack.last().unwrap() }
    pub fn locals_mut(&mut self) -> &mut Frame { self.stack.last_mut().unwrap() }

    pub fn set_local(&mut self, name: Bstr, value: Val) {
        self.locals_mut().insert(name, value);
    }

    pub fn get_var_cap(&self, mut name: &[u8]) -> Option<Val> {
        if let Some(b!('[')) = name.first() {name = &name[1..]}
        let mut skipped = 0;
        loop {
            for frame in self.stack.iter().rev().skip(skipped) {
                if let Some(var) = frame.get(name) {
                    return Some(var.clone())
                }
            }
            if let Some(b!('[')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return None }
        }
    }

    pub fn get_var(&self, mut name: &[u8]) -> Option<Val> {
        let mut skipped = 0;
        loop {
            for frame in self.stack.iter().rev().skip(skipped) {
                if let Some(var) = frame.get(name) {
                    return Some(var.clone())
                }
            }
            if let Some(b!(']')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return None }
        }
    }

    pub fn mutate_var(&mut self, mut name: &[u8], func: Val) -> Option<Val> {
        let mut skipped = 0;
        loop {
            for (fmn, frame) in self.stack.iter_mut().enumerate().rev().skip(skipped) {
                if let Some((nam, val)) = frame.remove_entry(name) {
                    let val = func.monad(self, val);
                    self.stack[fmn].insert(nam, val.clone());
                    return Some(val);
                }
            }
            if let Some(b!(']')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return None }
        }
    }

    pub fn delete_var(&mut self, mut name: &[u8]) {
        let mut skipped = 0;
        loop {
            for (_, frame) in self.stack.iter_mut().enumerate().rev().skip(skipped) {
                if frame.remove_entry(name).is_some() { return; }
            }
            if let Some(b!(']')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return }
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Val {
        macro_rules! eval { ($v:expr) => {
            match self.eval($v) {
                Val::Err(x) => return Val::Err(x),
                x => x,
            }
        }}
        match expr {
            Expr::Var(s) => self.get_var(s).unwrap_or_default(),
            Expr::Int(n) => Int(*n),
            Expr::Flt(n) => Num(*n),
            Expr::Snd(l) => {    
                let mut v = Vec::with_capacity(l.len());
                for x in l { v.push(eval!(x)); }
                Lis { l: Rc::from(v), fill: NAN.rc() }
            },
            Expr::Afn1 { a, f } => {
                let a = eval!(a); let f = eval!(f);
                f.monad(self, a)
            },
            Expr::Afn2 { a, f, b } => {
                let a = eval!(a); let f = eval!(f); let b = eval!(b);
                f.dyad(self, a, b)
            },
            Expr::SetVar(v) => Val::FSet(v.clone()),
            Expr::CngVar(v) => Val::FCng(v.clone()),
            Expr::Aav1 { v, g } => {
                let g = eval!(&g.clone());
                self.get_var(&v[..]).unwrap_or_default().monad(self, g)
            }
            Expr::Aav2 { f, v, g } => {
                let f = eval!(&f.clone()); let g = eval!(&g.clone());
                self.get_var(&v[..]).unwrap_or_default().dyad(self, g, f)
            },
            Expr::Bind { f, b } => {
                let f = eval!(&f.clone()); let b = eval!(&b.clone());
                Val::Bind{f: f.rc(), b: b.rc()}
            },
            Expr::Trn2 { a, f } => {
                let a = eval!(&a.clone()); let f = eval!(&f.clone());
                Val::Trn2{a: a.rc(), f: f.rc()}
            },
            Expr::Trn3 { a, f, b } => {
                let a = eval!(&a.clone()); let f = eval!(&f.clone()); let b = eval!(&b.clone());
                Val::Trn3{a: a.rc(), f: f.rc(), b: b.rc()}
            },
            Expr::Fork { a, f, b } => {
                let a = eval!(&a.clone()); let f = eval!(&f.clone()); let b = eval!(&b.clone());
                Val::Fork{a: a.rc(), f: f.rc(), b: b.rc()}
            },
            Expr::Dfn { s, cap } => {
                let mut locals = HashMap::with_capacity(cap.len());
                for var in cap {
                    self.get_var_cap(var).and_then(|x| locals.insert(var.clone(), x));
                }
                Val::Dfn {s: Rc::from(&s[..]), loc: Rc::new(locals)}
            },
        }
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Option<Val> {
        macro_rules! eval { ($v:expr) => {
            match self.eval($v) {
                Val::Err(x) => return Some(Val::Err(x)),
                x => x,
            }
        }}
        match stmt {
            Stmt::Discard(expr) => { let _ = eval!(expr); },
            Stmt::Loc(a, v) => { let a = eval!(a); self.set_local(v.clone(), a); },
            Stmt::Mut(a, v) => { let a = eval!(a); self.mutate_var(v, a); },
            Stmt::DelLoc(v) => { self.locals_mut().remove(v); },
            Stmt::DelMut(v) => { self.delete_var(v); },
            Stmt::Return(expr) => { return Some(eval!(expr)); },
            Stmt::Cond(cond, then) => {
                let val = eval!(cond);
                let cond = val.is_scalar() && val.as_bool() || {
                    let a = self.locals().get(&[b!('α')][..]).cloned().unwrap_or(NAN);
                    let b = self.locals().get(&[b!('Σ')][..])
                        .map_or(false, |x| x.try_int() != Some(1))
                        .then(|| self.locals().get(&[b!('β')][..]).cloned().unwrap_or(NAN) );
                    val.call(self, a, b).as_bool()
                };
                if cond { return self.eval_stmt(then) }
            }
        };
        None
    }

    pub fn eval_block(&mut self, block: &[Stmt]) -> Val {
        for stmt in block.iter() { 
            if let Some(val) = self.eval_stmt(stmt) { return val };
        }
        NAN
    }
    pub fn include_string(&mut self, code: &str) -> Val {
        use crate::{token, parse, codepage};
        let tokens = token::tokenize(&codepage::tobytes(code).unwrap()[..]);
        //println!("{:?}", tokens);
        let parsed = parse::parse(&tokens);
        //for i in &parsed { println!("parsed: {}", i); }
        self.eval_block(&parsed)
    }

    pub fn include_stdlib(&mut self) {
        self.include_string(STDLIB);
    }

    pub fn include_file<F: std::io::Read>(&mut self, file: &mut F) -> std::io::Result<Val> {
        let mut code = String::new();
        file.read_to_string(&mut code)?;
        //println!("input : ```{}```", code);
        Ok(self.include_string(&code))
    }

    pub fn include_args(&mut self, args: &[String]) -> Vec<Val> {
        use smallvec::smallvec;
        let args: Vec<Val> = args.iter().map(|s| s.chars().map(|x| Int(x as i64)).collect()).collect();
        if let Some(x) = args.get(0) { self.set_local(smallvec![b!('α')], x.clone()); }
        if let Some(x) = args.get(1) { self.set_local(smallvec![b!('β')], x.clone()); }
        self.set_local(smallvec![b!('Σ')], Int(args.len() as _));
        self.set_local(smallvec![b!('δ')], Val::lis(args.clone()));
        args
    }

}

impl<'io> Default for Env<'io> {
    fn default() -> Self {
        Self::new()
    }
}

