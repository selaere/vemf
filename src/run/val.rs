use core::cmp::Ordering;

use super::Val::{self, Num, Int, Lis};
use super::{NAN, c64, Env};
use crate::prelude::*;

impl Val {
    
    pub const NAN: Val = NAN;
    
    pub fn bool(b: bool) -> Val { Int(i64::from(b)) }

    //pub fn is_list(&self) -> bool { matches!(self, Lis {..})}

    pub fn is_nan(&self) -> bool { match self { Num(n) => n.is_nan(), _ => false }}

    pub fn is_infinite(&self) -> bool { !matches!(self, Int(_) | Num(_) | Lis {..})}

    pub fn is_scalar(&self) -> bool { matches!(self, Int(_) | Num(_))}

    pub fn as_bool(&self) -> bool { match self {
        Int(n) => *n != 0,
        Num(n) => !n.is_nan() && *n != c64::new(0., 0.),
        _ => false,
    }}

    pub fn try_c(&self) -> Option<c64> { match self {
        Int(n) => Some(c64::new(*n as f64, 0.)),
        Num(n) => Some(*n),
        _ => None,
    }}

    pub fn try_int(&self) -> Option<i64> { match self {
        Int(n) => Some(*n),
        Num(n) => Some(n.re as i64),
        _ => None
    }}

    pub fn as_c(&self) -> c64 { self.try_c().unwrap_or(c64::new(f64::NAN, f64::NAN)) }

    pub fn flt(n: f64) -> Val { Num(c64::new(n, 0.)) }

    pub fn approx(&self, other: &Val) -> bool {
        const TOLERANCE: f64 = 0.00000000023283064365386963; // $2^{-32}$
        fn close(a: c64, b: c64) -> bool {
            let d = (a - b).norm();
            d <= TOLERANCE
            || d / a.norm() <= TOLERANCE
            || d / b.norm() <= TOLERANCE
        }
        match (self, other) {
            (Num(l), Num(r)) => close(*l, *r) || l.is_nan() && r.is_nan(),
            (Int(l), Int(r)) => l == r,
            (Num(l), Int(r)) => close(*l, c64::new(*r as f64, 0.)),
            (Int(l), Num(r)) => close(*r, c64::new(*l as f64, 0.)),
            (Lis { l: l_l, fill: l_fill }, Lis { l: r_l, fill: r_fill }) => 
                l_fill == r_fill
                && l_l.len() == r_l.len()
                && l_l.iter().zip(r_l.iter()).all(|(x, y)| Val::approx(x, y)),
            _ => false
        }
    }

    pub fn cmpval(&self, other: &Val) -> Ordering {
        match (self, other) {
            (Int(m), Int(n)) => m.cmp(n),
            (a, b) => match (a.try_c(), b.try_c()) {
                (Some(m), Some(n)) => complexcmp(m, n),
                (None,    Some(_)) => Ordering::Greater,
                (Some(_), None) => Ordering::Less,
                (None,    None) => Ordering::Less,
            }
        }
    }
        
    pub fn monad(&self, env: &mut Env, a: Val) -> Val { 
        self.call(env, a, None)
    }

    pub fn dyad(&self, env: &mut Env, a: Val, b: Val) -> Val {
        self.call(env, a, Some(b))
    }

    pub fn call(&self, env: &mut Env, a: Val, b: Option<Val>) -> Val {
        match self {
            Val::Err(x) => Val::Err(*x),

            Lis { .. } | Num(_) | Int(_) => self.clone(),
            Val::FSet(name) => {
                env.set_local(name.clone(), a.clone());
                b.unwrap_or(a)
            },
            Val::FCng(name) => {
                env.mutate_var(name, a).unwrap_or(NAN)
            }
            Val::Dfn { s, loc } => {
                env.stack.push((**loc).clone());
                env.set_local(smallvec![b!('Σ')], Int(1 + i64::from(b.is_some())));
                env.set_local(smallvec![b!('α')], a);
                env.set_local(smallvec![b!('β')], b.unwrap_or(NAN));
                env.set_local(smallvec![b!('ƒ')], self.clone());
                let val = env.eval_block(s);
                env.stack.pop();
                val
            },

            Val::Bind { f: aa, b: bb } => aa.dyad(env, a, (**bb).clone()),
            Val::Trn2 { a: aa, f: ff } => {
                let x = aa.call(env, a, b);
                ff.monad(env, x)
            },
            Val::Trn3 { a: aa, f: ff, b: bb } => {
                let x = aa.call(env, a, b);
                ff.dyad(env, x, (**bb).clone())
            },
            Val::Fork { a: aa, f: ff, b: bb } => {
                let l = aa.call(env, a.clone(), b.clone());
                let r = bb.call(env, a, b);
                ff.dyad(env, l, r)
            }

            Val::AvBuilder(t) => Val::Av(*t, b.map(|x| x.rc()), a.rc()),
            Val::Av(t, f, g) => t.call(env, a, b, f.as_ref(), g),
            Val::Func(f) => f(env, a, b),
        }
    }

    pub fn rc(self) -> Rc<Self> { Rc::new(self) }

}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Num(l), Num(r)) => l == r || l.is_nan() && r.is_nan(),
            (Int(l), Int(r)) => l == r,
            (Num(l), Int(r)) => l.im == 0. && l.re == *r as f64,
            (Int(l), Num(r)) => r.im == 0. && r.re == *l as f64,
            (Lis { l: l_l, fill: l_fill }, Lis { l: r_l, fill: r_fill }) => 
                l_fill == r_fill
                && l_l.len() == r_l.len()
                && l_l.iter().zip(r_l.iter()).all(|(x, y)| x == y),
            _ => false
        }
    }
}
impl Eq for Val {}

pub fn complexcmp(a: c64, b: c64) -> Ordering {
    match (a.is_nan(), b.is_nan()) {
        (true , true ) => Ordering::Equal,
        (true , false) => Ordering::Less,
        (false, true ) => Ordering::Greater,
        (false, false) => a.re.total_cmp(&b.re).then_with(|| a.im.total_cmp(&b.im))
    }
}


func!(a :encode b => {
    if b.is_infinite() { return NAN };
    match a {
        Int(n) => encode_int(n, b),
        Num(n) => encode_flt(n.re, b),
        _ => NAN,
    }
});

fn encode_int(mut a: i64, b: Val) -> Val {
    let mut list = vec![Int(0); b.len() + 1];
    for (n, i) in b.into_iterf().enumerate().rev() {
        let i = or_nan!(i.try_int());
        if i == 0 { list[n+1] = Int(a); return Val::lis(list); }
        let m; (a, m) = (a.div_euclid(i), a % i);
        list[n+1] = Int(m);
        if a == 0 { return Val::lis(list); }
    }
    list[0] = Int(a);
    Val::lis(list)
}

fn encode_flt(mut a: f64, b: Val) -> Val {
    let mut list = vec![Val::flt(0.); b.len() + 1];
    for (n, i) in b.into_iterf().enumerate().rev() {
        let c64{re: i, ..} = or_nan!(i.try_c());
        if i == 0. { list[n+1] = Val::flt(a); return Val::lis(list); }
        let m; (a, m) = (a.div_euclid(i), a.rem_euclid(i));
        list[n+1] = Val::flt(m);
        if a == 0. { return Val::lis(list); }
    }
    list[0] = Val::flt(a);
    Val::lis(list)
}