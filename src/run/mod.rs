mod intrn;
mod list;
mod adverb;

use crate::parse::{Expr};
use crate::Bstr;
use std::{collections::HashMap, rc::Rc};

const STDLIB: &str = include_str!("../std.vemf");

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
    Dfn { loc: Rc<HashMap<Bstr, Val>>, s: Rc<[Expr]> },
    Bind{ f: Rc<Val>, b: Rc<Val> },
    Trn2{ a: Rc<Val>, f: Rc<Val> },
    Trn3{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Fork{ a: Rc<Val>, f: Rc<Val>, b: Rc<Val> },
    Swap,      DSwap(Rc<Val>),
    Each,      DEach(Rc<Val>),
    Scalar,    DScalar(Rc<Val>),
    Scan,      DScan(Rc<Val>),
    Reduce,    DReduce(Rc<Val>),
    Valences,  DValences(Rc<Val>, Rc<Val>),
    Overleft,  DOverleft(Rc<Val>, Rc<Val>),
    Overright, DOverright(Rc<Val>, Rc<Val>),
    Over,      DOver(Rc<Val>, Rc<Val>),
    Monadic,   DMonadic(Rc<Val>),
    Add, Sub, Mul, Div, Mod, Pow, Log, Lt, Gt, Eq, Max, Min,
    Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan,
    Left, Right, Len, Index, Iota, Pair, Enlist, Ravel,
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
            z => write!(f, "<function {:?}>", z),
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

    pub fn eval(&mut self, expr: &Expr) -> Val {
        match expr {
            Expr::Var(s) => self.get_var(s).unwrap_or_default(),
            Expr::Num(n) => Val::Num(*n),
            Expr::Snd(l) => Val::Lis {
                l: Rc::from(l.iter().map(|x| self.eval(x)).collect::<Vec<_>>()),
                fill: Rc::new(NAN)
            },
            Expr::Afn1 { a, f } => {
                let a = self.eval(a);
                let f = self.eval(f);
                f.monad(self, &a)
            },
            Expr::Afn2 { a, f, b } => {
                let a = self.eval(a);
                let f = self.eval(f);
                let b = self.eval(b);
                f.dyad(self, &a, &b)
            },
            Expr::SetVar(v) => Val::FSet(v.clone()),
            Expr::Aav1 { v, g } => {
                let g = self.eval(&*g.clone());
                self.locals.get(&v[..]).cloned().unwrap_or_default().monad(self, &g)
            }
            Expr::Aav2 { f, v, g } => {
                let f = self.eval(&*f.clone());
                let g = self.eval(&*g.clone());
                self.locals.get(&v[..]).cloned().unwrap_or_default().dyad(self, &g, &f)
            },
            Expr::Bind { f, b } => {
                let f = self.eval(&*f.clone());
                let b = self.eval(&*b.clone());
                Val::Bind{f: Rc::new(f), b: Rc::new(b)}
            },
            Expr::Trn1 { a, f } => {
                let a = self.eval(&*a.clone());
                let f = self.eval(&*f.clone());
                Val::Trn2{a: Rc::new(a), f: Rc::new(f)}
            },
            Expr::Trn2 { a, f, b } => {
                let a = self.eval(&*a.clone());
                let f = self.eval(&*f.clone());
                let b = self.eval(&*b.clone());
                Val::Trn3{a: Rc::new(a), f: Rc::new(f), b: Rc::new(b)}
            },
            Expr::Fork { a, f, b } => {
                let a = self.eval(&*a.clone());
                let f = self.eval(&*f.clone());
                let b = self.eval(&*b.clone());
                Val::Fork{a: Rc::new(a), f: Rc::new(f), b: Rc::new(b)}
            },
            Expr::Dfn { s, cap } => {
                let mut locals = HashMap::with_capacity(cap.len());
                for var in cap {
                    self.get_var(var).and_then(|x| locals.insert(var.clone(), x));
                }
                Val::Dfn {s: Rc::from(&s[..]), loc: Rc::new(locals)}
            },
        }
    }

    pub fn eval_block(&mut self, block: &[Expr]) -> Val {
        let mut v = NAN;
        for expr in block.iter() {
            v = self.eval(expr);
        }
        v
    }
    pub fn include_string(&mut self, code: &str) -> Val{
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
