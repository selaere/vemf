use std::rc::Rc;

use crate::{run::{Val, Env}, Bstr, b};
use smallvec::smallvec;
use Val::{Num, Lis};

const NAN: Val = Num(f64::NAN);


impl Val {

pub fn monad(&self, state: &mut Env, a: &Val) -> Val { 
    self.call(state, a, None)
}

pub fn dyad(&self, state: &mut Env, a: &Val, b: &Val) -> Val {
    self.call(state, a, Some(b))
}

pub fn call(&self, state: &mut Env, a: &Val, b: Option<&Val>) -> Val { 
    let bb = b.unwrap_or(a);
    match self {
        Num(_) | Lis { .. } => self.clone(),
        Val::FSet(name) => {
            state.locals.insert(name.clone(), a.clone());
            a.clone()
        },
        Self::Add => match (a, b) {(Num(a), Some(Num(b))) => Num(a + b), _ => NAN },
        Self::Sub => match (a, b) {(Num(a), Some(Num(b))) => Num(a - b), _ => NAN },
        Self::Mul => match (a, b) {(Num(a), Some(Num(b))) => Num(a * b), _ => NAN },
        Self::Div => match (a, b) {(Num(a), Some(Num(b))) => Num(a / b), _ => NAN },
        Self::Pow => match (a, b) {(Num(a), Some(Num(b))) => Num(a.powf(*b)), _ => NAN },
        Self::Neg  => match a { Num(a) => Num(-a), _ => NAN },
        Self::Sin  => match a { Num(a) => Num(a.sin() ), _ => NAN },
        Self::Asin => match a { Num(a) => Num(a.asin()), _ => NAN },
        Self::Cos  => match a { Num(a) => Num(a.cos() ), _ => NAN },
        Self::Acos => match a { Num(a) => Num(a.acos()), _ => NAN },
        Self::Tan  => match a { Num(a) => Num(a.tan() ), _ => NAN },
        Self::Atan => match a { Num(a) => Num(a.atan()), _ => NAN },
        Self::Sqrt => match a { Num(a) => Num(a.sqrt()), _ => NAN },
        Self::Selfie => Self::DSelfie(Rc::new(a.clone())),
        Self::DSelfie(g) => g.dyad(state, bb, a),
        Self::Variances => Self::DVariances(Rc::new(bb.clone()), Rc::new(a.clone())),
        Self::DVariances(f, g) => match b {
            Some(b) => g.dyad(state, a, b),
            None => f.monad(state, a)
        },
        Self::LoadIntrinsics => {
            macro_rules! load { ($($name:ident),*) => { $( {
                let mut name = Bstr::from(&b"intrn"[..]);
                name.extend(stringify!($name).to_ascii_lowercase().bytes());
                state.locals.insert(name, Self::$name)
            } );* }}
            load!(Add, Sub, Mul, Div, Pow, Neg, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, 
                Selfie, Variances);
            NAN
        }
        Self::Dfn { s, loc } => {
            let mut inner = Env { locals: (**loc).clone(), outer: Some(state) };
            inner.locals.insert(smallvec![b!('α')], a.clone());
            inner.locals.insert(smallvec![b!('β')], bb.clone());
            inner.locals.insert(smallvec![b!('ƒ')], self.clone());
            inner.eval_block(s)
        },
    }
}

}