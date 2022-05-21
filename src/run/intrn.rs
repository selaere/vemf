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
        Val::Dfn { s, loc } => {
            let mut inner = Env { locals: (**loc).clone(), outer: Some(env) };
            inner.locals.insert(smallvec![b!('α')], a.clone());
            inner.locals.insert(smallvec![b!('β')], bb.clone());
            inner.locals.insert(smallvec![b!('ƒ')], self.clone());
            inner.eval_block(s)
        },
        Val::Selfie => Val::DSelfie(Rc::new(a.clone())),
        Val::DSelfie(g) => g.dyad(env, bb, a),
        Val::Variances => Val::DVariances(Rc::new(bb.clone()), Rc::new(a.clone())),
        Val::DVariances(f, g) => (if b.is_none() {f} else {g}).call(env, a, b),
        Val::LoadIntrinsics => {
            macro_rules! load { ($($name:ident,)*) => { $( {
                let mut name = Bstr::from(&b"in"[..]);
                name.extend(stringify!($name).to_ascii_lowercase().bytes());
                env.locals.insert(name, Val::$name)
            } );* }}
            load!(
                Selfie, Variances,
                Add, Sub, Mul, Div, Mod, Pow, Log, Lt, Eq, Gt,
                Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan,
                Left, Right, Len,
                Print, Println, Exit,
            );
            Num(1.)
        }
        Val::Add  => match (a, b) {(Num(a), Some(Num(b))) => Num(a + b), _ => NAN },
        Val::Sub  => match (a, b) {(Num(a), Some(Num(b))) => Num(a - b), _ => NAN },
        Val::Mul  => match (a, b) {(Num(a), Some(Num(b))) => Num(a * b), _ => NAN },
        Val::Div  => match (a, b) {(Num(a), Some(Num(b))) => Num(a / b), _ => NAN },
        Val::Mod  => match (a, b) {(Num(a), Some(Num(b))) => Num(a % b), _ => NAN },
        Val::Pow  => match (a, b) {(Num(a), Some(Num(b))) => Num(a.powf(*b)), _ => NAN },
        Val::Log  => match (a, b) {(Num(a), Some(Num(b))) => Num(a.log(*b)), _ => NAN },
        Val::Lt   => match (a, b) {(Num(a), Some(Num(b))) => Val::from_bool(a < b), _ => NAN },
        Val::Eq   => match (a, b) {(Num(a), Some(Num(b))) => Val::from_bool(a == b), _ => NAN },
        Val::Gt   => match (a, b) {(Num(a), Some(Num(b))) => Val::from_bool(a > b), _ => NAN },
        Val::Isnan=> match a { Num(a) => Val::from_bool(a.is_nan()), _ => NAN },
        Val::Abs  => match a { Num(a) => Num(a.abs()  ), _ => NAN },
        Val::Neg  => match a { Num(a) => Num(-a       ), _ => NAN },
        Val::Ln   => match a { Num(a) => Num(a.ln()   ), _ => NAN },
        Val::Exp  => match a { Num(a) => Num(a.exp()  ), _ => NAN },
        Val::Sin  => match a { Num(a) => Num(a.sin()  ), _ => NAN },
        Val::Asin => match a { Num(a) => Num(a.asin() ), _ => NAN },
        Val::Cos  => match a { Num(a) => Num(a.cos()  ), _ => NAN },
        Val::Acos => match a { Num(a) => Num(a.acos() ), _ => NAN },
        Val::Tan  => match a { Num(a) => Num(a.tan()  ), _ => NAN },
        Val::Atan => match a { Num(a) => Num(a.atan() ), _ => NAN },
        Val::Sqrt => match a { Num(a) => Num(a.sqrt() ), _ => NAN },
        Val::Round=> match a { Num(a) => Num(a.round()), _ => NAN },
        Val::Ceil => match a { Num(a) => Num(a.ceil() ), _ => NAN },
        Val::Floor=> match a { Num(a) => Num(a.floor()), _ => NAN },

        Val::Print   => { print  !("{}", a.display_string()); a.clone() },
        Val::Println => { println!("{}", a.display_string()); a.clone() },
        Val::Exit => match a {
            Num(n) => std::process::exit(*n as i32),
            _ => { eprintln!("{}", a.display_string()); std::process::exit(1); }
        }

        Val::Left => a.clone(),
        Val::Right => bb.clone(),
        Val::Len => Num(a.len()),

        Val::Bind { f: af, b: ab } => af.dyad(env, a, ab),
        Val::Trn1 { a: aa, f: af }        => { let x = aa.call(env, a, b); af.monad(env, &x) },
        Val::Trn2 { a: aa, f: af, b: ab } => { let x = aa.call(env, a, b); af.dyad(env, &x, ab) },
        Val::Fork { a: aa, f: af, b: ab } => {
            let l = aa.call(env, a, b);
            let r = ab.call(env, a, b);
            af.dyad(env, &l, &r)
        }
    }
}

fn from_bool(b: bool) -> Val { Num(f64::from(u8::from(b))) }


fn display_string(&self) -> String {
    match self {
        Num(n) => format!("{}", n),
        Lis { l, .. } => l.iter().flat_map(|x| match x {
            Num(n) => char::from_u32(*n as i32 as u32),
            _ => None
        }).collect(),
        otherwise => format!("{}", otherwise),
    }
}

pub fn len(&self) -> f64 {
    match self {
        Num(_) => 1.,
        Lis { l, .. } => l.len() as f64,
        _ => f64::INFINITY,
    }
}


pub fn index_value(&self, env: &mut Env, index: f64) -> Val {
    match self {
        Num(n) => Num(*n), // unchanged
        Lis { l, fill } => {
            if index < 0. || index.is_nan() { return (**fill).clone() }
            l.get(index as usize).cloned().unwrap_or_else(|| (**fill).clone())
        },
        x => x.monad(env, &Num(index))
    }
}

}