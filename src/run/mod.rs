mod intrn;
mod list;

use crate::parse::{Tg, Fe, Ve};
use crate::Bstr;
use std::{collections::HashMap, rc::Rc};


pub const NAN: Val = Num(f64::NAN);

#[derive(Clone, Debug)]
pub struct Env<'a> {
    locals: HashMap<Bstr, Val>,
    outer: Option<&'a Env<'a>>,
}

#[derive(Clone, Debug)]
pub enum Val {
    Num(f64),
    Lis { l: Rc<Vec<Val>>, fill: Rc<Val> },
    FSet(Bstr),
    Dfn { loc: Rc<HashMap<Bstr, Val>>, s: Rc<[Ve]> },
    Bind{ f: Rc<Val>, b: Rc<Val> },
    Trn1{ a: Rc<Val>, f: Rc<Val> },
    Trn2{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Fork{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Selfie,    DSelfie(Rc<Val>),
    Variances, DVariances(Rc<Val>, Rc<Val>),
    Add, Sub, Mul, Div, Mod, Pow, Log, Lt, Gt, Eq,
    Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan,
    Left, Right, Len, Index,
    Print, Println, Exit,
    LoadIntrinsics,
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Num(n) => write!(f, "{}", n),
            Lis { l, .. } => {
                let mut iter = l.iter();
                write!(f, "(")?;
                if let Some(i) = iter.next() { write!(f, "{}", i)?; }
                for i in iter { write!(f, " {}", i)?; }
                write!(f, ")")?;
                Ok(())
            },
            Val::FSet(x) => write!(f, "→{}", crate::codepage::tochars(x)),
            _ => write!(f, "<function>"),
        }
    }
}

use Val::{Num, Lis};
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
        self.locals.get(name).cloned().or_else(|| self.outer.and_then(|x| x.get_var(name)))
    }

    pub fn eval(&mut self, expr: &Ve) -> Val {
        match expr {
            Ve::Var(s) => self.locals.get(&s[..]).cloned().unwrap_or_default(),
            Ve::Num(n) => Val::Num(*n),
            Ve::Snd(l) => Val::Lis {
                l: Rc::from(l.iter().map(|x| self.eval(x)).collect::<Vec<_>>()),
                fill: Rc::new(NAN)
            },
            Ve::Nom(f) => self.eval_f(f),
            Ve::Afn1 { a, f } => {
                let a = self.eval(a);
                let f = self.eval_f(f);
                f.monad(self, &a)
            },
            Ve::Afn2 { a, f, b } => {
                let a = self.eval(a);
                let f = self.eval_f(f);
                let b = self.eval(b);
                f.dyad(self, &a, &b)
            },
        }
    }
    pub fn eval_f(&mut self, expr: &Fe) -> Val {
        match expr {
            Fe::Var(s) => self.get_var(s).unwrap_or_default(),
            Fe::SetVar(v) => Val::FSet(v.clone()),
            Fe::Aav1 { v, g } => {
                let g = self.eval_tg(&*g.clone());
                self.locals.get(&v[..]).cloned().unwrap_or_default().monad(self, &g)
            }
            Fe::Aav2 { f, v, g } => {
                let f = self.eval_tg(&*f.clone());
                let g = self.eval_tg(&*g.clone());
                self.locals.get(&v[..]).cloned().unwrap_or_default().dyad(self, &g, &f)
            },
            Fe::Bind { f, b } => {
                let f = self.eval_f(&*f.clone());
                let b = self.eval(&*b.clone());
                Val::Bind{f: Rc::new(f), b: Rc::new(b)}
            },
            Fe::Trn1 { a, f } => {
                let a = self.eval_f(&*a.clone());
                let f = self.eval_f(&*f.clone());
                Val::Trn1{a: Rc::new(a), f: Rc::new(f)}
            },
            Fe::Trn2 { a, f, b } => {
                let a = self.eval_f(&*a.clone());
                let f = self.eval_f(&*f.clone());
                let b = self.eval(&*b.clone());
                Val::Trn2{a: Rc::new(a), f: Rc::new(f), b: Rc::new(b)}
            },
            Fe::Fork { a, f, b } => {
                let a = self.eval_tg(&*a.clone());
                let f = self.eval_tg(&*f.clone());
                let b = self.eval_tg(&*b.clone());
                Val::Fork{a: Rc::new(a), f: Rc::new(f), b: Rc::new(b)}
            },
            Fe::Dfn { s, cap } => {
                let mut locals = HashMap::with_capacity(cap.len());
                for var in cap {
                    self.locals.get(var)
                        // TODO iterate through outer
                        .cloned()
                        .and_then(|x| locals.insert(var.clone(), x));
                }
                Val::Dfn {s: Rc::from(&s[..]), loc: Rc::new(locals)}
            },
        }
    }
    fn eval_tg(&mut self, expr: &Tg) -> Val {
        match expr {
            Tg::Ve(ve) => self.eval(ve),
            Tg::Fe(fe) => self.eval_f(fe)
        }
    }

    pub fn eval_block(&mut self, block: &[Ve]) -> Val {
        let mut v = NAN;
        for expr in block.iter() {
            v = self.eval(expr);
        }
        v
    }

}
