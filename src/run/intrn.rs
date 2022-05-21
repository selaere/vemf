use std::rc::Rc;

use crate::{run::{Val, Env}, Bstr, b};
use smallvec::smallvec;
use Val::{Num, Lis};

const NAN: Val = Num(f64::NAN);


impl Val {

pub fn monad(&self, env: &mut Env, a: &Val) -> Val { 
    self.call(env, a, None)
}

pub fn dyad(&self, env: &mut Env, a: &Val, b: &Val) -> Val {
    self.call(env, a, Some(b))
}

pub fn call(&self, env: &mut Env, a: &Val, b: Option<&Val>) -> Val { 
    let bb = b.unwrap_or(a);
    match self {
        Num(_) | Lis { .. } => self.clone(),
        Val::FSet(name) => {
            env.locals.insert(name.clone(), a.clone());
            a.clone()
        },
        Self::Dfn { s, loc } => {
            let mut inner = Env { locals: (**loc).clone(), outer: Some(env) };
            inner.locals.insert(smallvec![b!('α')], a.clone());
            inner.locals.insert(smallvec![b!('β')], bb.clone());
            inner.locals.insert(smallvec![b!('ƒ')], self.clone());
            inner.eval_block(s)
        },
        Self::Selfie => Self::DSelfie(Rc::new(a.clone())),
        Self::DSelfie(g) => g.dyad(env, bb, a),
        Self::Variances => Self::DVariances(Rc::new(bb.clone()), Rc::new(a.clone())),
        Self::DVariances(f, g) => (if b.is_none() {f} else {g}).call(env, a, b),
        Self::LoadIntrinsics => {
            macro_rules! load { ($($name:ident,)*) => { $( {
                let mut name = Bstr::from(&b"in"[..]);
                name.extend(stringify!($name).to_ascii_lowercase().bytes());
                env.locals.insert(name, Self::$name)
            } );* }}
            load!(
                Selfie, Variances,
                Add, Sub, Mul, Div, Mod, Pow, Log, Lt, Eq, Gt,
                Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan,
            );
            Num(1.)
        }
        Self::Add  => match (a, b) {(Num(a), Some(Num(b))) => Num(a + b), _ => NAN },
        Self::Sub  => match (a, b) {(Num(a), Some(Num(b))) => Num(a - b), _ => NAN },
        Self::Mul  => match (a, b) {(Num(a), Some(Num(b))) => Num(a * b), _ => NAN },
        Self::Div  => match (a, b) {(Num(a), Some(Num(b))) => Num(a / b), _ => NAN },
        Self::Mod  => match (a, b) {(Num(a), Some(Num(b))) => Num(a % b), _ => NAN },
        Self::Pow  => match (a, b) {(Num(a), Some(Num(b))) => Num(a.powf(*b)), _ => NAN },
        Self::Log  => match (a, b) {(Num(a), Some(Num(b))) => Num(a.log(*b)), _ => NAN },
        Self::Lt   => match (a, b) {(Num(a), Some(Num(b))) => Val::from_bool(a < b), _ => NAN },
        Self::Eq   => match (a, b) {(Num(a), Some(Num(b))) => Val::from_bool(a == b), _ => NAN },
        Self::Gt   => match (a, b) {(Num(a), Some(Num(b))) => Val::from_bool(a > b), _ => NAN },
        Self::Isnan=> match a { Num(a) => Val::from_bool(a.is_nan()), _ => NAN },
        Self::Abs  => match a { Num(a) => Num(a.abs()  ), _ => NAN },
        Self::Neg  => match a { Num(a) => Num(-a       ), _ => NAN },
        Self::Ln   => match a { Num(a) => Num(a.ln()   ), _ => NAN },
        Self::Exp  => match a { Num(a) => Num(a.exp()  ), _ => NAN },
        Self::Sin  => match a { Num(a) => Num(a.sin()  ), _ => NAN },
        Self::Asin => match a { Num(a) => Num(a.asin() ), _ => NAN },
        Self::Cos  => match a { Num(a) => Num(a.cos()  ), _ => NAN },
        Self::Acos => match a { Num(a) => Num(a.acos() ), _ => NAN },
        Self::Tan  => match a { Num(a) => Num(a.tan()  ), _ => NAN },
        Self::Atan => match a { Num(a) => Num(a.atan() ), _ => NAN },
        Self::Sqrt => match a { Num(a) => Num(a.sqrt() ), _ => NAN },
        Self::Round=> match a { Num(a) => Num(a.round()), _ => NAN },
        Self::Ceil => match a { Num(a) => Num(a.ceil() ), _ => NAN },
        Self::Floor=> match a { Num(a) => Num(a.floor()), _ => NAN },
    }
}

fn from_bool(b: bool) -> Val { Num(f64::from(u8::from(b))) }

}