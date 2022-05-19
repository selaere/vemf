mod intrn;

use crate::parse::{Tg, Fe, Ve};
use crate::Bstr;
use smallvec::smallvec;
use intrn::Intrn;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug)]
pub struct Env {
    globals: HashMap<Bstr, Val>
}

#[derive(Clone, Debug)]
pub enum Val {
    Num(f64),
    Lis { l: Rc<[Val]>, fill: Rc<Val> },
    FIntrn(intrn::Intrn),  // intrinsic function
    FSet(Bstr),
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
            Val::FIntrn(_) => write!(f, "<function>"),
            Val::FSet(x) => write!(f, "→{}", crate::codepage::tochars(x)),
        }
    }
}

use Val::{Num, Lis};
impl Default for Val {
    fn default() -> Self { Self::Num(f64::NAN) }
}

const GLOBALS: &[(u8, Val)] = &[
    (b'a', Num(12.)),     (b'b', Num(20.)),     (b'c', Num(99.)),     (b'd', Num(100.)),    
    (b'e', Num(999.)),    (b'f', Num(1000.)),   (b'g', Num(9999.)),   (b'h', Num(10000.)),  
    (b'i', Num(100000.)), (b'j', Num(1000000.)),(b'k', Num(15.)),     (b'l', Num(16.)),     
    (b'm', Num(31.)),     (b'n', Num(32.)),     (b'o', Num(63.)),     (b'p', Num(64.)),
    (b'q', Num(127.)),    (b'r', Num(128.)),    (b's', Num(255.)),    (b't', Num(256.)),
    (b'u', Num(512.)),    (b'v', Num(1024.)),   (b'w', Num(2048.)),   (b'x', Num(4096.)),
    (b'y', Num(32768.)),  (b'z', Num(65536.)),
];

impl Env {
    
    pub fn new() -> Env {
        let mut globals = HashMap::with_capacity(GLOBALS.len());
        for (b, v) in GLOBALS { globals.insert(smallvec![*b], v.clone()); }
        globals.insert(Bstr::from(&b"loadintrinsics"[..]), Val::FIntrn(Intrn::LoadIntrinsics));
        Env { globals }
    }

    pub fn evaluate(&mut self, expr: &Ve) -> Val {
        match expr {
            Ve::Var(s) => self.globals.get(&s[..]).cloned().unwrap_or_default(),
            Ve::Num(n) => Val::Num(*n),
            Ve::Snd(l) => Val::Lis {
                l: Rc::from(l.iter().map(|x| self.evaluate(x)).collect::<Vec<_>>()),
                fill: Rc::new(Num(f64::NAN))
            },
            Ve::Nom(f) => self.evaluate_fe(f),
            Ve::Afn1 { a, f } => {
                let a = self.evaluate(a);
                let f = self.evaluate_fe(f);
                self.monad(&f, &a)
            },
            Ve::Afn2 { a, f, b } => {
                let a = self.evaluate(a);
                let f = self.evaluate_fe(f);
                let b = self.evaluate(b);
                self.dyad(&f, &a, &b)
            },
        }
    }
    pub fn evaluate_fe(&mut self, expr: &Fe) -> Val {
        match expr {
            Fe::Var(s) => self.globals.get(&s[..]).cloned().unwrap_or_default(),
            Fe::SetVar(v) => Val::FSet(v.clone()),
            Fe::Aav1 { v, g } => {
                let g = self.evaluate_tg(&*g.clone());
                self.monad(&self.globals.get(&v[..]).cloned().unwrap_or_default(), &g)
            }
            Fe::Aav2 { f, v, g } => {
                let f = self.evaluate_tg(&*f.clone());
                let g = self.evaluate_tg(&*g.clone());
                self.dyad(&self.globals.get(&v[..]).cloned().unwrap_or_default(), &g, &f)
            },
            Fe::Bind { .. } => todo!(),
            Fe::Trn1 { .. } => todo!(),
            Fe::Trn2 { .. } => todo!(),
            Fe::Dfn(_) => todo!(),
        }
    }
    fn evaluate_tg(&mut self, expr: &Tg) -> Val {
        match expr {
            Tg::Ve(ve) => self.evaluate(ve),
            Tg::Fe(fe) => self.evaluate_fe(fe)
        }
    }
    
    pub fn monad(&mut self, f: &Val, a: &Val) -> Val {
        match f {
            Num(_) | Lis { .. } => f.clone(),
            Val::FIntrn(i) => i.monad(self, a),
            Val::FSet(name) => {
                self.globals.insert(name.clone(), a.clone());
                a.clone()
            },
        }
    }
    
    pub fn dyad(&mut self, f: &Val, a: &Val, b: &Val) -> Val {
        match f {
            Num(_) | Lis { .. } => f.clone(),
            Val::FIntrn(i) => {
                i.dyad(self, a, b)
            },
            Val::FSet(name) => {
                self.globals.insert(name.clone(), a.clone());
                a.clone()
            },
        }
    }

    pub fn index_value(&mut self, a: &Val, index: f64) -> Val {
        match a {
            Num(n) => Num(*n), // unchanged
            Lis { l, fill } => {
                if index < 0. || index.is_nan() { return (**fill).clone() }
                l.get(index as usize).cloned().unwrap_or_else(|| (**fill).clone())
            },
            x => self.monad(&Num(index), x)
        }
    }
}
