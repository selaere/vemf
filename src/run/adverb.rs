use super::{Val, Env, NAN};
use crate::prelude::*;

#[derive(Copy, Clone, Debug)]
pub enum AvT {
    Swap, Const, Monadic,
    Each, EachLeft, Conform, Extend,
    Scan, ScanPairs, Reduce, Stencil, Valences,
    Overleft, Overright, Over, Forkleft, Forkright,
    Until, UntilScan, Power, PowerScan,
    Drill,
}

impl AvT {
pub fn call(&self, env: &mut Env, a: Val, b: Option<Val>, f: Option<&Rc<Val>>, g: &Rc<Val>) -> Val {
    let fg = f.unwrap_or(g);
    match self {
        Self::Swap =>      g.dyad(env, b.unwrap_or_else(|| a.clone()), a),
        Self::Const =>     (**g).clone(),
        Self::Monadic =>   g.monad(env, a),
        Self::Each =>      each(env, a, b, g),
        Self::EachLeft =>  each_left(env, a, b, g),
        Self::Conform =>   conform(env, a, b, g),
        Self::Extend =>    extend(env, a, b, g),
        Self::Scan =>      scan(env, a, b, g),
        Self::ScanPairs => scan_pairs(env, a, b, g),
        Self::Reduce =>    reduce(env, a, b, g),
        Self::Stencil =>   stencil(env, a, b, fg, g),
        Self::Valences =>  (if b.is_none() {fg} else {g}).call(env, a, b),
        Self::Over => {
            let l = fg.monad(env, a);
            let r = b.map(|b| fg.monad(env, b));
            g.call(env, l, r)
        },
        Self::Overleft  => { let l = fg.monad(env, a); g.call(env, l, b) },
        Self::Overright => { let r = b.map(|b| fg.monad(env, b)); g.call(env, a, r) },
        Self::Forkleft  => { let l = fg.call(env, a.clone(), b.clone()); g.dyad(env, l, b.unwrap_or(a)) },
        Self::Forkright => { let r = fg.call(env, a.clone(), b); g.dyad(env, a, r) },
        Self::Until =>     until(env, a, b, fg, g),
        Self::UntilScan => until_scan(env, a, b, fg, g),
        Self::Power =>     power(env, a, b, fg, g),
        Self::PowerScan => power_scan(env, a, b, fg, g),
        Self::Drill     => drill(env, a, b, fg, g),
    }
}
}

pub fn each(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>) -> Val {
    if a.is_scalar() && b.iter().all(|x| x.is_scalar()) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        if (a.is_infinite() && b.is_infinite())
        || (a.is_infinite() && b.is_scalar()  )
        || (a.is_scalar()   && b.is_infinite()) {
            Val::Fork { a: a.rc(), 
                        f: Val::Av(AvT::Conform, None, Rc::clone(g)).rc(), 
                        b: b.rc() }
        } else if a.is_scalar() {
            b.into_iterf().map(|x| g.dyad(env, a.clone(), x)).collect()
        } else if b.is_scalar() {
            a.into_iterf().map(|x| g.dyad(env, x, b.clone())).collect()
        } else {
            let len = usize::min(a.len(), b.len());
            a.itertake(env, len).zip(b.itertake(env, len))
                .map(|(a, b)| g.dyad(env, a, b)).collect()
        }
    } else { each_left(env, a, None, g) }
}

pub fn each_left(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>) -> Val {
    if a.is_scalar() {
        g.call(env, a, b)
    } else if a.is_infinite() {
        if let Some(b) = b {
            Val::Fork { a: a.rc(), f: Rc::clone(g), b: b.rc() }
        } else {
            Val::Trn2 { a: a.rc(), f: Rc::clone(g) }
        }
    } else { a.into_iterf().map(|x| g.call(env, x, b.clone())).collect() }
}

pub fn conform(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>) -> Val {
    if a.is_scalar() && b.iter().all(|x| x.is_scalar()) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        if (a.is_infinite() && b.is_infinite())
        || (a.is_infinite() && b.is_scalar())
        || (a.is_scalar() && b.is_infinite()) {
            Val::Fork { a: a.rc(), 
                        f: Val::Av(AvT::Conform, None, Rc::clone(g)).rc(), 
                        b: b.rc() }
        } else if a.is_scalar() {
            b.into_iterf().map(|x| conform(env, a.clone(), Some(x), g)).collect()
        } else if b.is_scalar() {
            a.into_iterf().map(|x| conform(env, x, Some(b.clone()), g)).collect()
        } else {
            let len = usize::min(a.len(), b.len());
            a.itertake(env, len).zip(b.itertake(env, len))
                .map(|(a, b)| conform(env, a, Some(b), g))
                .collect()
        }
    } else { extend(env, a, None, g) }
}

pub fn extend(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>) -> Val {
    if a.is_scalar() {
        g.call(env, a, b)
    } else if a.is_infinite() { 
        if let Some(b) = b {
            Val::Fork { a: a.rc(), f: Val::Av(AvT::Conform, None, Rc::clone(g)).rc(), b: b.rc() }
        } else {
            Val::Trn2 { a: a.rc(), f: Val::Av(AvT::Conform, None, Rc::clone(g)).rc() }
        }
    } else { a.into_iterf().map(|x| extend(env, x, b.clone(), g)).collect()}
}

pub fn scan(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>) -> Val {
    if a.is_infinite() { return NAN; }
    let mut values = Vec::with_capacity(a.len());
    let mut iter = a.into_iterf();
    if let Some(start) = iter.next() {
        let mut val = match b {
            Some(b) => g.dyad(env, b, start),
            None => start,
        };
        values.push(val.clone());
        for i in iter {
            val = g.dyad(env, val, i);
            values.push(val.clone());
        }
        Val::lis(values)
    } else { b.unwrap_or(NAN) }
}

pub fn reduce(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>) -> Val {
    if a.is_infinite() { return NAN; }
    let mut iter = a.into_iterf();
    if let Some(start) = iter.next() {
        let mut val = match b {
            Some(b) => g.dyad(env, b, start),
            None => start,
        };
        for i in iter {
            val = g.dyad(env, val, i);
        }
        val
    } else { b.unwrap_or(NAN) }
    
}

pub fn until_scan(env: &mut Env, a: Val, b: Option<Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let mut values = vec![a.clone()];
    let mut val = a;
    loop {
        let tried = g.call(env, val.clone(), b.clone());
        if f.dyad(env, tried.clone(), val).as_bool() { break }
        values.push(tried.clone());
        val = tried;
    }
    Val::lis(values)
}

pub fn until(env: &mut Env, a: Val, b: Option<Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let mut val = a;
    loop {
        let tried = g.call(env, val.clone(), b.clone());
        if f.dyad(env, tried.clone(), val.clone()).as_bool() { break }
        val = tried;
    }
    val
}

pub fn power_scan(env: &mut Env, a: Val, b: Option<Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let num = f.call(env, a.clone(), b.clone()).try_int().map_or(0, |x| x.try_into().unwrap_or(0));
    let mut values = Vec::with_capacity(num);
    values.push(a.clone());
    let mut val = a;
    for _ in 0..num {
        val = g.call(env, val, b.clone());
        values.push(val.clone());
    }
    Val::lis(values)
}

pub fn power(env: &mut Env, a: Val, b: Option<Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let num = f.call(env, a.clone(), b.clone()).try_int().map_or(0, |x| x.try_into().unwrap_or(0));
    let mut val = a;
    for _ in 0..num {
        val = g.call(env, val, b.clone());
    }
    val
}

#[allow(clippy::needless_borrow)]
pub fn scan_pairs(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>) -> Val {
    if a.len() == 0 { return Val::lis_fill(Vec::new(), a.fill()); }
    let elems = a.iterf().collect::<Vec<_>>();
    let mut list = Vec::with_capacity(elems.len());
    let first = if let Some(b) = b { g.dyad(env, b, elems[0].clone()) } else { elems[0].clone() };
    list.push(first);
    for i in 1..elems.len() {
        list.push(g.dyad(env, elems[i-1].clone(), elems[i].clone()));
    }
    Val::lis(list)
}

pub fn stencil(env: &mut Env, a: Val, b: Option<Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    if let Some(size) = f.call(env, a.clone(), b.clone()).try_int().map(|x| x as usize) {
        if a.is_infinite() { return Val::lis(Vec::new()); }
        (0..(a.len() + 1).saturating_sub(size)).map(|n| {
            g.call(env, a.iterf().skip(n).take(size).cloned().collect(), b.clone())
        }).collect()
    } else {
        // we could do something smart here like reshaping the output or using
        // multiple dimensions but uh
        Val::lis(Vec::new())
    }
}

pub fn drill(env: &mut Env, a: Val, b: Option<Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let iter = (**f).call(env, a.clone(), b.clone()).into_iterf();
    drill_iter(env, a, b, iter, g)
}

pub fn drill_iter(
    env: &mut Env,
    a: Val,
    b: Option<Val>,
    mut iter: Box<dyn super::list::GoodIter<Val>>,
    g: &Rc<Val>,
) -> Val {
    let index = iter.next();
    if let Some(index) = index {
        if index.is_nan() { return a; }
        let index = match index.try_int().and_then(|x|->Option<usize> { x.try_into().ok() }) {
            Some(i) => i, None => return a,
        };
        if let Val::Lis{l, fill} = a {
            let mut v = match Rc::try_unwrap(l) {
                Ok(l) => l,
                Err(l) => l.to_vec(),
            };
            if v.len() <= index {
                v.resize(index+1, (*fill).clone());
            }
            v[index] = drill_iter(env, core::mem::take(&mut v[index]), b, iter, g);
            Val::lis_fill(v, (*fill).clone())
        } else {
            g.call(env, a, b)
        }
    } else {
        g.call(env, a, b)
    }
}
