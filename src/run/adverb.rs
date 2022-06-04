use super::{Val::{self, Num, Lis}, Env, NAN};
use std::{rc::Rc, ops::Range};

#[allow(clippy::collapsible_else_if)]
pub fn each(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let (Num(_), Some(Num(_)) | None) = (a, b) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        let mut collect_range = |x:Range<usize>| Rc::new(x.map(|n| {
            let l = a.index(env, n); let r = b.index(env, n); g.dyad(env, &l, &r)
        }).collect());
        if !a.is_finite() && !b.is_finite() { 
            Val::Fork { a: a.clone().rc(), f: Val::DScalar(Rc::clone(g)).rc(), b: b.clone().rc() }
        } else if a.is_scalar() { Lis { l: collect_range(0..b.len()), fill: b.fill().rc() }
        } else if b.is_scalar() { Lis { l: collect_range(0..a.len()), fill: a.fill().rc() }
        } else { Lis { l: collect_range(0..usize::min(a.len(), b.len())), fill: a.fill().rc() }
        }
    } else {
        if !a.is_finite() { Val::Trn2 {
            a: a.clone().rc(), f: Val::DScalar(Rc::clone(g)).rc()
        }} else { Lis {
            l: Rc::new((0..a.len()).map(|n| {
                let l = a.index(env, n); g.monad(env, &l)
            }).collect()),
            fill: a.fill().rc()
        }}
    }
}

#[allow(clippy::collapsible_else_if)]
pub fn scal(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let (Num(_), Some(Num(_)) | None) = (a, b) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        let mut collect_range = |x:Range<usize>| Rc::new(x.map(|n| {
            let l = a.index(env, n); let r = b.index(env, n); scal(env, &l, Some(&r), g)
        }).collect());
        if !a.is_finite() && !b.is_finite() { 
            Val::Fork { a: a.clone().rc(), f: Val::DScalar(Rc::clone(g)).rc(), b: b.clone().rc() }
        } else if a.is_scalar() { Lis { l: collect_range(0..b.len()), fill: b.fill().rc() }
        } else if b.is_scalar() { Lis { l: collect_range(0..a.len()), fill: a.fill().rc() }
        } else { Lis { l: collect_range(0..usize::min(a.len(), b.len())), fill: a.fill().rc() }}
    } else {
        if !a.is_finite() { Val::Trn2 {
            a: a.clone().rc(), f: Val::DScalar(Rc::clone(g)).rc()
        }} else { Lis {
            l: Rc::new((0..a.len()).map(|n| {
                let l = a.index(env, n); scal(env, &l, None, g)
            }).collect()),
            fill: a.fill().rc()
        }}
    }
}


pub fn scan(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if !a.is_finite() { return NAN; }
    let mut iter = a.iterf();
    let mut values = Vec::with_capacity(a.len());
    let Some(start) = iter.next() else { return b.cloned().unwrap_or(NAN) };
    let mut val = match b {
        Some(b) => g.dyad(env, b, start),
        None => start.clone(),
    };
    values.push(val.clone());
    for i in iter {
        val = g.dyad(env, &val, i);
        values.push(val.clone());
    }
    values.into_iter().collect()
}

pub fn reduce(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if !a.is_finite() { return NAN; }
    let mut iter = a.iterf();
    let Some(start) = iter.next() else { return b.cloned().unwrap_or(NAN) };
    let mut val = match b {
        Some(b) => g.dyad(env, b, start),
        None => start.clone(),
    };
    for i in iter {
        val = g.dyad(env, &val, i);
    }
    val
}

pub fn until_scan(env: &mut Env, a: &Val, b: Option<&Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let mut values = vec![a.clone()];
    let mut val = a.clone();
    loop {
        let tried = g.call(env, &val, b);
        if matches!( f.dyad(env, &tried, &val), Num(n) if n != 0. || n.is_nan()) { break }
        values.push(tried.clone());
        val = tried;
    }
    values.into_iter().collect()
}


pub fn until(env: &mut Env, a: &Val, b: Option<&Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let mut val = a.clone();
    loop {
        let tried = g.call(env, &val, b);
        if matches!(f.dyad(env, &tried, &val), Num(n) if n != 0. || n.is_nan()) { break }
        val = tried;
    }
    val
}

pub fn power_scan(env: &mut Env, a: &Val, b: Option<&Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let num = match f.call(env, a, b) {
        Num(n) if n > 0. && !n.is_nan() => n as usize,
        _ => 0,
    };
    let mut values = Vec::with_capacity(num);
    let mut val = a.clone();
    for _ in 0..num {
        val = g.call(env, &val, b);
        values.push(val.clone());
    }
    values.into_iter().collect()
}


pub fn power(env: &mut Env, a: &Val, b: Option<&Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let num = match f.call(env, a, b) {
        Num(n) if n > 0. && !n.is_nan() => n as usize,
        _ => 0,
    };
    let mut val = a.clone();
    for _ in 0..num {
        val = g.call(env, &val, b);
    }
    val
}