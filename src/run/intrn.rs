use std::rc::Rc;

use crate::{run::{Val, Env}, Bstr};
use Val::{Num, FIntrn};

const NAN: Val = Num(f64::NAN);

#[derive(Clone, Debug)]
pub enum Intrn {
    Add, Sub, Mul, Div, Pow, Neg, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt,
    Selfie,    DSelfie(Rc<Val>),
    Variances, DVariances(Rc<Val>, Rc<Val>),
    LoadIntrinsics,
}

impl Intrn {

pub fn monad(&self, state: &mut Env, a: &Val) -> Val { match self {
    Self::Neg  => match a { Num(a) => Num(-a), _ => NAN },
    Self::Sin  => match a { Num(a) => Num(a.sin()), _ => NAN },
    Self::Cos  => match a { Num(a) => Num(a.cos()), _ => NAN },
    Self::Tan  => match a { Num(a) => Num(a.tan()), _ => NAN },
    Self::Asin => match a { Num(a) => Num(a.asin()), _ => NAN },
    Self::Acos => match a { Num(a) => Num(a.acos()), _ => NAN },
    Self::Atan => match a { Num(a) => Num(a.atan()), _ => NAN },
    Self::Sqrt => match a { Num(a) => Num(a.sqrt()), _ => NAN },
    Self::Selfie => FIntrn(Self::DSelfie(Rc::new(a.clone()))),
    Self::DSelfie(g) => state.dyad(g, a, a),
    Self::DVariances(f, _) => state.monad(f, a),
    Self::LoadIntrinsics => {
        macro_rules! load { ($($name:ident),*) => { $( {
            let mut name = Bstr::from(&b"intrn"[..]);
            name.extend(stringify!($name).to_ascii_lowercase().bytes());
            state.globals.insert(name, FIntrn(Self::$name))
        } );* }}
        load!(Add, Sub, Mul, Div, Pow, Neg, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, 
            Selfie, Variances);
        NAN
    }
    _ => self.dyad(state, a, a) // α is doubled
}}

pub fn dyad(&self, state: &mut Env, a: &Val, b: &Val) -> Val { match self {
    Self::Add => match (a, b) {(Num(a), Num(b)) => Num(a + b), _ => NAN },
    Self::Sub => match (a, b) {(Num(a), Num(b)) => Num(a - b), _ => NAN },
    Self::Mul => match (a, b) {(Num(a), Num(b)) => Num(a * b), _ => NAN },
    Self::Div => match (a, b) {(Num(a), Num(b)) => Num(a / b), _ => NAN },
    Self::Pow => match (a, b) {(Num(a), Num(b)) => Num(a.powf(*b)), _ => NAN },
    Self::Variances => FIntrn(Self::DVariances(Rc::new(b.clone()), Rc::new(a.clone()))),
    Self::DSelfie(g) => state.dyad(g.as_ref(), b, a),
    Self::DVariances(_, g) => state.dyad(g.as_ref(), a, b),
    _ => self.monad(state, a) // β is ignored
}}

}