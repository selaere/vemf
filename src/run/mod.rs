mod intrn; mod list; mod adverb; mod disp; mod number;

use crate::{parse::{Expr, Stmt}, Bstr};
use std::{collections::HashMap, rc::Rc};
use adverb::AvT;

const STDLIB: &str = include_str!("../std.vemf");

pub use num_complex::Complex64 as c64;

pub const NAN: Val = Num(c64::new(f64::NAN, f64::NAN));

#[derive(Debug)]
pub struct Env<'a> {
    pub locals: HashMap<Bstr, Val>,
    pub outer: Option<&'a Env<'a>>,
}

#[derive(Clone, Debug)]
pub enum Val {
    Num(c64),
    Int(i64),
    Lis { l: Rc<Vec<Val>>, fill: Rc<Val> },
    FSet(Bstr),
    Dfn { loc: Rc<HashMap<Bstr, Val>>, s: Rc<[Stmt]> },
    Bind{ f: Rc<Val>, b: Rc<Val> },
    Trn2{ a: Rc<Val>, f: Rc<Val> },
    Trn3{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Fork{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Av(AvT, Option<Rc<Val>>, Rc<Val>),
    AvBuilder(AvT),
    Cycle,     DCycle(Rc<Vec<Val>>),
    Add, Sub, Mul, Div, Mod, Pow, Log, Lt, Gt, Eq, Max, Min, Atanb, Approx, BAnd, BOr, BXor,
    Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan, Sign, BNot, BRepr,
    Complex, Cis, Real, Imag, Conj, Arg,
    Left, Right, Len, Shape, Index, Iota, Pair, Enlist, Ravel, Concat, Reverse, GetFill, SetFill,
    Print, Println, Exit, Format, Numfmt, Parse,
    Takeleft, Takeright, Dropleft, Dropright, Replist, Match, Deal, Sample, Replicate,
    GradeUp, GradeDown, SortUp, SortDown, BinsUp, BinsDown, Encode, FromCp, ToCp,
    LoadIntrinsics,
}


use Val::{Lis, Num, Int};
impl Default for Val {
    fn default() -> Self { NAN }
}

impl Env<'_> {
    
    pub fn new<'a>() -> Env<'a> {
        let mut locals = HashMap::new();
        locals.insert(Bstr::from(&b"loadintrinsics"[..]), Val::LoadIntrinsics);
        Env { locals, outer: None }
    }

    pub fn get_var(&self, name: &[u8]) -> Option<Val> {
        self.locals.get(name).cloned().or_else(|| self.outer.as_ref().and_then(|x| x.get_var(name)))
    }

    pub fn get_var_rec(&self, name: &[u8]) -> Option<Val> {
        if let Some(stripped) = name.strip_prefix(&[b!('■')]) {
            self.outer.as_ref().and_then(|x| x.get_var_rec(stripped))
        } else {
            self.get_var(name)
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Val {
        match expr {
            Expr::Var(s) => self.get_var(s).unwrap_or_default(),
            Expr::Int(n) => Int(*n),
            Expr::Flt(n) => Num(*n),
            Expr::Snd(l) => Lis {
                l: Rc::from(l.iter().map(|x| self.eval(x)).collect::<Vec<_>>()),
                fill: NAN.rc()
            },
            Expr::Afn1 { a, f } => {
                let a = self.eval(a); let f = self.eval(f);
                f.monad(self, a)
            },
            Expr::Afn2 { a, f, b } => {
                let a = self.eval(a); let f = self.eval(f); let b = self.eval(b);
                f.dyad(self, a, b)
            },
            Expr::SetVar(v) => Val::FSet(v.clone()),
            Expr::Aav1 { v, g } => {
                let g = self.eval(&*g.clone());
                self.locals.get(&v[..]).cloned().unwrap_or_default().monad(self, g)
            }
            Expr::Aav2 { f, v, g } => {
                let f = self.eval(&*f.clone()); let g = self.eval(&*g.clone());
                self.locals.get(&v[..]).cloned().unwrap_or_default().dyad(self, g, f)
            },
            Expr::Bind { f, b } => {
                let f = self.eval(&*f.clone()); let b = self.eval(&*b.clone());
                Val::Bind{f: f.rc(), b: b.rc()}
            },
            Expr::Trn2 { a, f } => {
                let a = self.eval(&*a.clone()); let f = self.eval(&*f.clone());
                Val::Trn2{a: a.rc(), f: f.rc()}
            },
            Expr::Trn3 { a, f, b } => {
                let a = self.eval(&*a.clone());
                let f = self.eval(&*f.clone());
                let b = self.eval(&*b.clone());
                Val::Trn3{a: a.rc(), f: f.rc(), b: b.rc()}
            },
            Expr::Fork { a, f, b } => {
                let a = self.eval(&*a.clone());
                let f = self.eval(&*f.clone());
                let b = self.eval(&*b.clone());
                Val::Fork{a: a.rc(), f: f.rc(), b: b.rc()}
            },
            Expr::Dfn { s, cap } => {
                let mut locals = HashMap::with_capacity(cap.len());
                for var in cap {
                    let a = if let Some(a) = var.strip_prefix(&[b!('■')]) {a} else {var};
                    self.get_var_rec(a).and_then(|x| locals.insert(var.clone(), x));
                }
                Val::Dfn {s: Rc::from(&s[..]), loc: Rc::new(locals)}
            },
        }
    }

    #[allow(clippy::needless_borrow)] /*clippy bug i think*/
    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Option<Val> {
        match stmt {
            Stmt::Discard(expr) => { let _ = self.eval(&expr); },
            Stmt::Conj(a, v) => {
                let a = self.eval(&a);
                self.locals.get(&v[..]).cloned().unwrap_or_default().monad(self, a);
            },
            Stmt::Set(a, v) => {
                let a = self.eval(&a);
                self.locals.insert(v.clone(), a);
            },
            Stmt::Return(expr) => { return Some(self.eval(&expr)); },
            Stmt::Cond(cond, then) => {
                let val = self.eval(&cond);
                let cond = val.is_scalar() && val.as_bool() || {
                    let a = self.locals.get(&[b!('α')][..]).cloned().unwrap_or(NAN);
                    let b = self.locals.get(&[b!('β')][..]).cloned();
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
        let tokens = token::tokenize(&codepage::tobytes(code).unwrap());
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

}

