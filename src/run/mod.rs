mod intrn;

use crate::parse::{Tg, Fe, Ve};
use crate::Bstr;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug)]
pub struct Env<'a> {
    locals: HashMap<Bstr, Val>,
    outer: Option<&'a Env<'a>>,
}

#[derive(Clone, Debug)]
pub enum Val {
    Num(f64),
    Lis { l: Rc<[Val]>, fill: Rc<Val> },
    FSet(Bstr),
    Dfn { loc: Rc<HashMap<Bstr, Val>>, s: Rc<[Ve]> },
    Add, Sub, Mul, Div, Pow, Neg, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt,
    Selfie,    DSelfie(Rc<Val>),
    Variances, DVariances(Rc<Val>, Rc<Val>),
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
    fn default() -> Self { Self::Num(f64::NAN) }
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
                fill: Rc::new(Num(f64::NAN))
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
            Fe::Bind { .. } => todo!(),
            Fe::Trn1 { .. } => todo!(),
            Fe::Trn2 { .. } => todo!(),
            Fe::Dfn {s, cap} => {
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

    pub fn index_value(&mut self, a: &Val, index: f64) -> Val {
        match a {
            Num(n) => Num(*n), // unchanged
            Lis { l, fill } => {
                if index < 0. || index.is_nan() { return (**fill).clone() }
                l.get(index as usize).cloned().unwrap_or_else(|| (**fill).clone())
            },
            x => x.monad(self, &Num(index))
        }
    }

    pub fn eval_block(&mut self, block: &[Ve]) -> Val {
        let mut v = Num(f64::NAN);
        for expr in block.iter() {
            v = self.eval(expr);
        }
        v
    }

}
