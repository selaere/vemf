use std::rc::Rc;

use crate::{Bstr, b};
use super::{Val, Env, NAN};
use smallvec::smallvec;
use Val::{Num, Lis};

impl Val {

pub fn monad(&self, env: &mut Env, a: &Val) -> Val { 
    self.call(env, a, None)
}

pub fn dyad(&self, env: &mut Env, a: &Val, b: &Val) -> Val {
    self.call(env, a, Some(b))
}

pub fn call(&self, env: &mut Env, a: &Val, b: Option<&Val>) -> Val { 
    let ba = b.unwrap_or(a);
    match self {

        Val::LoadIntrinsics => {
            macro_rules! load { ($($name:ident,)*) => { $( {
                let mut name = Bstr::from(&b"in"[..]);
                name.extend(stringify!($name).to_ascii_lowercase().bytes());
                env.locals.insert(name, Val::$name)
            } );* }}
            load!(
                Swap, Monadic, Each, Scalar, Scan, Reduce, Valences, Over, Overleft, Overright, Until, UntilScan, Power, PowerScan,
                Add, Sub, Mul, Div, Mod, Pow, Log, Lt, Eq, Gt, Max, Min,
                Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan,
                Left, Right, Len, Index, Iota, Pair, Enlist, Ravel, Concat, Reverse, GetFill, SetFill,
                Print, Println, Exit, Format, Numfmt, Parse, Leftpad, Replist, Cycle,
            ); Num(1.)
        }

        Num(_) | Lis { .. } => self.clone(),
        Val::FSet(name) => {
            env.locals.insert(name.clone(), a.clone());
            a.clone()
        },
        Val::Dfn { s, loc } => {
            let mut inner = Env { locals: (**loc).clone(), outer: Some(env) };
            inner.locals.insert(smallvec![b!('α')], a.clone());
            inner.locals.insert(smallvec![b!('β')], ba.clone());
            inner.locals.insert(smallvec![b!('ƒ')], self.clone());
            inner.eval_block(s)
        },

        Val::Bind { f: aa, b: bb } => aa.dyad(env, a, bb),
        Val::Trn2 { a: aa, f: ff }        => { let x = aa.call(env, a, b); ff.monad(env, &x) },
        Val::Trn3 { a: aa, f: ff, b: bb } => { let x = aa.call(env, a, b); ff.dyad(env, &x, bb) },
        Val::Fork { a: aa, f: ff, b: bb } => {
            let l = aa.call(env, a, b);
            let r = bb.call(env, a, b);
            ff.dyad(env, &l, &r)
        }

        Val::Swap      => Val::DSwap     (Rc::new(a.clone())),
        Val::Monadic   => Val::DMonadic  (Rc::new(a.clone())),
        Val::Each      => Val::DEach     (Rc::new(a.clone())),
        Val::Scalar    => Val::DScalar   (Rc::new(a.clone())),
        Val::Scan      => Val::DScan     (Rc::new(a.clone())),
        Val::Reduce    => Val::DReduce   (Rc::new(a.clone())),
        Val::Valences  => Val::DValences (Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::Over      => Val::DOver     (Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::Overleft  => Val::DOverleft (Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::Overright => Val::DOverright(Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::Until     => Val::DUntil    (Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::UntilScan => Val::DUntilScan(Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::Power     => Val::DPower    (Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::PowerScan => Val::DPowerScan(Rc::new(ba.clone()), Rc::new(a.clone())),
        Val::DSwap(g) => g.dyad(env, ba, a),
        Val::DMonadic(g) => g.monad(env, a),
        Val::DEach(g) =>   super::adverb::each(env, a, b, g),
        Val::DScalar(g) => super::adverb::scal(env, a, b, g),
        Val::DScan(g) =>   super::adverb::scan(env, a, b, g),
        Val::DReduce(g) => super::adverb::reduce(env, a, b, g),
        Val::DValences(f, g) => (if b.is_none() {f} else {g}).call(env, a, b),
        Val::DOver(f, g) => {
            let l = f.monad(env, a); let r = f.monad(env, ba); g.dyad(env, &l, &r)
        },
        Val::DOverleft(f, g) => {let x = f.monad(env, a); g.dyad(env, &x, ba)},
        Val::DOverright(f, g) => {let x = f.monad(env, ba); g.dyad(env, a, &x)},
        Val::DUntil(f, g) => super::adverb::until(env, a, b, f, g),
        Val::DUntilScan(f, g) => super::adverb::until_scan(env, a, b, f, g),
        Val::DPower(f, g) => super::adverb::power(env, a, b, f, g),
        Val::DPowerScan(f, g) => super::adverb::power_scan(env, a, b, f, g),
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
        Val::Max  => match (a, b) {(Num(a), Some(Num(b))) => Num(a.max(*b)), _ => NAN },
        Val::Min  => match (a, b) {(Num(a), Some(Num(b))) => Num(a.min(*b)), _ => NAN },
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
        Val::Format => format!("{}", a).chars().map(|x| Num(x as u32 as f64)).collect(),
        Val::Numfmt => match a { // TODO support more bases and stuff
            Num(a) => format!("{}", a).chars().map(|x| Num(x as u32 as f64)).collect(),
            _ => NAN 
        }
        Val::Parse => a.display_string().parse::<f64>().map(Num).unwrap_or(NAN),
        Val::Leftpad => super::list::reshape(env, a, ba),

        Val::Left => a.clone(),
        Val::Right => ba.clone(),
        Val::Len => Num(a.lenf()),
        Val::Index => a.index_at_depth(env, ba),
        Val::Iota => match a {
            Lis{l, ..} => super::list::iota(
                Vec::new(), &l.iter().cloned().filter_map(|x| match x {
                    Num(b) => Some(b as isize), _ => None
                }).collect::<Vec<isize>>()),
            Num(n) => if *n == f64::INFINITY {Val::Left} else {
                super::list::iota_scalar(*n as isize)},
            _ => Val::Bind{ f: Rc::new(Val::Right), b: Rc::new(NAN) }
        }
        Val::Pair => [a, ba].into_iter().cloned().collect(),
        Val::Enlist => [a].into_iter().cloned().collect(),
        Val::Ravel => {
            let mut list = Vec::new();
            super::list::ravel(a, &mut list);
            list.into_iter().cloned().collect()
        },
        Val::Concat => super::list::concat(a, ba),
        Val::Reverse => super::list::reverse(a),
        Val::GetFill => match a {
            Lis {fill, ..} => (**fill).clone(),
            _ => NAN
        },
        Val::SetFill => match a {
            Lis {l, ..} => Lis {l: Rc::clone(l), fill: Rc::new(ba.clone())},
            _ => a.clone(),
        },
        Val::Replist => if a.is_finite() {
            let num = match ba {Num(n) => *n as usize, _ => return NAN};
            let mut list = Vec::new();
            for _ in 0..num {list.extend(a.iterf().unwrap().cloned())};
            list.into_iter().collect()
        } else {a.clone()},
        Val::Cycle => match a.iterf() {
            Some(i) => Val::DCycle(Rc::from(&i.cloned().collect::<Vec<_>>()[..])),
            None => a.clone()
        },
        Val::DCycle(l) => match a {
            Num(a) => l[(*a as usize) % l.len()].clone(),
            _ => NAN,
        },
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

pub fn is_nan(&self) -> bool { match self { Num(n) => n.is_nan(), _ => false }}

pub fn is_finite(&self) -> bool { matches!(self, Num(_) | Lis {..})}

}
