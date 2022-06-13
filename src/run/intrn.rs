use std::rc::Rc;
use crate::{Bstr, b};
use super::{Val::{self, Lis, Num, Int}, Env, NAN, adverb::AvT, Complex64, CNAN, cmp, comp};
use smallvec::smallvec;

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
                Add, Sub, Mul, Div, Mod, Pow, Log, Lt, Eq, Gt, Max, Min, Atanb,
                Abs, Neg, Ln, Exp, Sin, Asin, Cos, Acos, Tan, Atan, Sqrt, Round, Ceil, Floor, Isnan,
                Complex, Real, Imag, Conj, Arg, Cis,
                Left, Right, Len, Shape, Index, Iota, Pair, Enlist, Ravel, Concat, Reverse, GetFill, SetFill,
                Print, Println, Exit, Format, Numfmt, Parse,
                Takeleft, Takeright, Dropleft, Dropright, Replist, Cycle, Match, Deal, Sample,
            );
            macro_rules! load_av {($($name:ident,)*) => { $( {
                let mut name = Bstr::from(&b"in"[..]);
                name.extend(stringify!($name).to_ascii_lowercase().bytes());
                env.locals.insert(name, Val::AvBuilder(AvT::$name))
            } );* }}
            load_av!(
                Swap, Const, Monadic,
                Each, EachLeft, Conform, Extend,
                Scan, ScanPairs, Reduce, Stencil, Valences,
                Overleft, Overright, Over,
                Until, UntilScan, Power, PowerScan,
            ); Int(1)
        }

        Lis { .. } | Num(_) | Int(_) => self.clone(),
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

        Val::AvBuilder(t) => Val::Av(*t, b.map(|x| x.clone().rc()), a.clone().rc()),
        Val::Av(t, f, g) => t.call(env, a, b, f.as_ref(), g),
        Val::Add => match (a, ba) {
            (Int(a), Int(b)) => Int(a.saturating_add(*b)),
            _ => Num(a.as_c() + ba.as_c()),
        }
        Val::Sub => match (a, ba) {
            (Int(a), Int(b)) => Int(a.saturating_sub(*b)),
            _ => Num(a.as_c() - ba.as_c()),
        }
        Val::Mul => match (a, ba) {
            (Int(a), Int(b)) => Int(a.saturating_mul(*b)),
            _ => Num(a.as_c() * ba.as_c()),
        }
        Val::Div => Num(a.as_c().fdiv(ba.as_c())),
        Val::Mod => match (a, ba) {
            (Int(a), Int(b)) => Int(a.rem_euclid(*b)),
            _ => Num(a.as_c() % ba.as_c()),
        }
        Val::Pow => match (a, ba) {
            (Int(a), Int(b)) if *b >= 0 => Int(a.saturating_pow(*b as u32)),
            _ => Num(a.as_c().powc(ba.as_c())),
        },
        Val::Log => Num(a.as_c().log(ba.as_c().norm())),
        Val::Lt => match (a, ba) {
            (Int(a), Int(b)) => Val::bool(a < b),
            _ => a.try_c().zip(ba.try_c()).map_or(NAN, |(a, b)| Val::bool(cmp(a, b).is_lt()))
        },
        Val::Eq => match (a, ba) {
            (Int(a), Int(b)) => Val::bool(a == b),
            _ => a.try_c().zip(ba.try_c()).map_or(NAN, |(a, b)| Val::bool(a == b))
        },
        Val::Gt => match (a, ba) {
            (Int(a), Int(b)) => Val::bool(a > b),
            _ => a.try_c().zip(ba.try_c()).map_or(NAN, |(a, b)| Val::bool(cmp(a, b).is_gt()))
        },
        Val::Max => match (a, ba) {
            (Int(a), Int(b)) => Int(*a.max(b)),
            _ => if cmp(a.as_c(), ba.as_c()).is_gt() {a.clone()} else {ba.clone()}
        },
        Val::Min => match (a, ba) {
            (Int(a), Int(b)) => Int(*a.min(b)),
            _ => if cmp(a.as_c(), ba.as_c()).is_lt() {a.clone()} else {ba.clone()}
        },
        Val::Atanb=> {
            let (y, x) = (a.as_c(), ba.as_c());
            Val::f64(f64::atan2(y.re + x.im, x.re - y.im))
        },
        Val::Isnan=> Val::bool(a.is_nan()),
        Val::Abs  => match a { Int(a) => Int(a.abs()), Num(a) => Val::f64(a.norm()), _ => NAN },
        Val::Neg  => match a { Int(a) => Int(-a),      Num(a) => Num(-a), _ => NAN },
        Val::Ln   => a.try_c().map_or(NAN, |a| Num(a.ln()  )),
        Val::Exp  => a.try_c().map_or(NAN, |a| Num(a.exp() )),
        Val::Sin  => a.try_c().map_or(NAN, |a| Num(a.sin() )),
        Val::Asin => a.try_c().map_or(NAN, |a| Num(a.asin())),
        Val::Cos  => a.try_c().map_or(NAN, |a| Num(a.cos() )),
        Val::Acos => a.try_c().map_or(NAN, |a| Num(a.acos())),
        Val::Tan  => a.try_c().map_or(NAN, |a| Num(a.tan() )),
        Val::Atan => a.try_c().map_or(NAN, |a| Num(a.atan())),
        Val::Sqrt => a.try_c().map_or(NAN, |a| Num(a.sqrt())),
        Val::Round=> match a { Int(a) => Int(*a), Num(a) => Val::f64(a.re.round()) , _ => NAN },
        Val::Ceil => match a { Int(a) => Int(*a), Num(a) => Val::f64(a.re.ceil()) , _ => NAN },
        Val::Floor=> match a { Int(a) => Int(*a), Num(a) => Val::f64(a.re.floor()), _ => NAN },

        Val::Complex=> a.try_c().zip(ba.try_c()).map_or(NAN,
            |(a,b)| Num(Complex64::new(b.re, a.re))),
        Val::Cis  => a.try_c().zip(ba.try_c()).map_or(NAN,
            |(a,b)| Num(Complex64::from_polar(b.re, a.re))),
        Val::Real => a.try_c().map_or(NAN,|a| Val::f64(a.re)),
        Val::Imag => a.try_c().map_or(NAN,|a| Val::f64(a.im)),
        Val::Conj => a.try_c().map_or(NAN,|a| Num(a.conj())),
        Val::Arg  => a.try_c().map_or(NAN,|a| Val::f64(a.arg())),

        Val::Print   => { print  !("{}", a.display_string()); a.clone() },
        Val::Println => { println!("{}", a.display_string()); a.clone() },
        Val::Exit => match a.try_int() {
            Some(n) => std::process::exit(n as i32),
            _ => { eprintln!("{}", a.display_string()); std::process::exit(1); }
        }
        Val::Format => super::disp::format(a, &b.map_or_else(Vec::new, 
            |x| x.iterf().cloned().collect::<Vec<_>>())
        ).chars().map(|x| Int(x as i64)).collect(),
        Val::Numfmt => if !a.is_scalar() {NAN} else {
            format!("{a}").chars().map(|x| Int(x as i64)).collect() }
        Val::Parse => a.display_string().parse::<Complex64>().map(Num).unwrap_or(NAN),
        Val::Takeleft => super::list::takeleft(env, a, ba),
        Val::Takeright => super::list::takeright(env, a, ba),
        Val::Dropleft =>  ba.try_int().map_or(NAN, |b| super::list::dropleft(a, b)),
        Val::Dropright => ba.try_int().map_or(NAN, |b| super::list::dropright(a, b)),

        Val::Left => a.clone(),
        Val::Right => ba.clone(),
        Val::Len => match a {
            Num(_) | Int(_) => Int(1),
            Lis { l, .. } => Int(l.len() as i64),
            _ => Val::f64(f64::INFINITY),
        },
        Val::Index => a.index_at_depth(env, ba),
        Val::Iota => match a {
            Lis{l, ..} => super::list::iota(
                Vec::new(), &l.iter().cloned().filter_map(|x| x.try_int()).collect::<Vec<i64>>()),
            Num(n) => if !n.is_finite() {Val::Left} else {super::list::iota_scalar(n.re as i64)},
            Int(n) => super::list::iota_scalar(*n),
            _ => Val::Av(AvT::Const, None, NAN.rc()),
        }
        Val::Pair => Val::lis(vec![a.clone(), ba.clone()]),
        Val::Enlist => Val::lis(vec![a.clone()]),
        Val::Ravel => {
            let mut list = Vec::new();
            super::list::ravel(a, &mut list);
            Val::lis(list)
        },
        Val::Concat => super::list::concat(a, ba),
        Val::Reverse => super::list::reverse(a),
        Val::GetFill => match a {
            Lis {fill, ..} => (**fill).clone(),
            _ => NAN
        },
        Val::SetFill => match a {
            Lis {l, ..} => Lis {l: Rc::clone(l), fill: ba.clone().rc()},
            _ => a.clone(),
        },
        Val::Replist => if a.is_finite() {
            let Some(num) = ba.try_int() else {return NAN};
            (0..num).flat_map(|_| a.iterf().cloned()).collect()
        } else {a.clone()},
        Val::Cycle => if a.is_finite() {
            Val::DCycle(Rc::from(&a.iterf().cloned().collect::<Vec<_>>()[..]))
        } else {a.clone()},
        Val::DCycle(l) => a.try_int().map_or(NAN, |a| l[(a as usize) % l.len()].clone()),
        Val::Match => Val::bool(a == ba),
        Val::Shape => Val::lis_fill(
            super::list::shape(a).iter().map(|x| Int(*x as i64)).collect(),
            Int(1),
        ),

        Val::Deal => a.try_int().zip(ba.try_int()).map_or(NAN, |(a, b)| {
            use rand::distributions::{Distribution, Uniform};
            if a == 0 { return std::iter::repeat(NAN).take(b as _).collect() }
            Uniform::from(0..a as _).sample_iter(rand::thread_rng()).take(b as _)
                .map(|x| Int(i64::from(x))).collect::<Val>()
        }),
        Val::Sample => a.try_int().zip(ba.try_int()).map_or(NAN, |(a, b)|
            rand::seq::index::sample(&mut rand::thread_rng(), a as _, b as _)
                .iter()
                .map(|x| Int(x as i64))
                .collect::<Val>(),
        )
    }
}


fn bool(b: bool) -> Val { Int(i64::from(b)) }

pub fn is_nan(&self) -> bool { match self { Num(n) => n.is_nan(), _ => false }}

pub fn is_finite(&self) -> bool { matches!(self, Int(_) | Num(_) | Lis {..})}

pub fn is_scalar(&self) -> bool { matches!(self, Int(_) | Num(_))}

//pub fn is_int(&self) -> bool { matches!(self, Int(_))}
//pub fn is_num(&self) -> bool { matches!(self, Num(_))}

pub fn as_bool(&self) -> bool {
    match self {
        Int(n) => *n != 0,
        Num(n) => !n.is_nan() && *n != num::zero::<Complex64>(),
        _ => false,
    }
}

pub fn rc(self) -> Rc<Self> { Rc::new(self) }

pub fn try_c(&self) -> Option<Complex64> {
    match self {
        Int(n) => Some(comp(*n as f64)),
        Num(n) => Some(*n),
        _ => None,
    }
}

pub fn try_int(&self) -> Option<i64> {
    match self {
        Int(n) => Some(*n),
        Num(n) => Some(n.re as i64),
        _ => None
    }
}

pub fn as_c(&self) -> Complex64 { self.try_c().unwrap_or(CNAN) }

pub fn f64(n: f64) -> Val { Num(comp(n)) }
}


