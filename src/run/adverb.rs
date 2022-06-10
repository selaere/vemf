use super::{Val::{self, Num, Lis}, Env, NAN};
use std::{rc::Rc, ops::Range};

pub fn each(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let (Num(_), Some(Num(_)) | None) = (a, b) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        let mut collect_range = |x: Range<usize>| Rc::new(x.map(|n| {
            let l = a.index(env, n); let r = b.index(env, n); g.dyad(env, &l, &r)
        }).collect());
        if !a.is_finite() && !b.is_finite() { 
            Val::Fork { a: a.clone().rc(), f: Val::DConform(Rc::clone(g)).rc(), b: b.clone().rc() }
        } else if a.is_scalar() { Lis { l: collect_range(0..b.len()), fill: b.fill().rc() }
        } else if b.is_scalar() { Lis { l: collect_range(0..a.len()), fill: a.fill().rc() }
        } else { Lis { l: collect_range(0..usize::min(a.len(), b.len())), fill: a.fill().rc() }
        }
    } else { each_left(env, a, None, g) }
}

pub fn each_left(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let (Num(_), Some(Num(_)) | None) = (a, b) {
        g.call(env, a, b)
    } else if !a.is_finite() { if let Some(b) = b {
        Val::Fork { a: a.clone().rc(), f: g.clone(), b: b.clone().rc() }
    } else {
        Val::Trn2 { a: a.clone().rc(), f: g.clone() }
    }} else { Lis {
        l: Rc::new((0..a.len()).map(|n| {
            let l = a.index(env, n); g.call(env, &l, b)
        }).collect()),
        fill: a.fill().rc()
    }}
}


pub fn conform(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let (Num(_), Some(Num(_)) | None) = (a, b) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        let mut collect_range = |x: Range<usize>| Rc::new(x.map(|n| {
            let l = a.index(env, n); let r = b.index(env, n); conform(env, &l, Some(&r), g)
        }).collect());
        if !a.is_finite() && !b.is_finite() { 
            Val::Fork { a: a.clone().rc(), f: Val::DConform(Rc::clone(g)).rc(), b: b.clone().rc() }
        } else if a.is_scalar() { Lis { l: collect_range(0..b.len()), fill: b.fill().rc() }
        } else if b.is_scalar() { Lis { l: collect_range(0..a.len()), fill: a.fill().rc() }
        } else { Lis { l: collect_range(0..usize::min(a.len(), b.len())), fill: a.fill().rc() }}
    } else { extend(env, a, None, g) }
}

pub fn extend(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let (Num(_), Some(Num(_)) | None) = (a, b) {
        g.call(env, a, b)
    } else if !a.is_finite() { if let Some(b) = b {
        Val::Fork { a: a.clone().rc(), f: Val::DConform(Rc::clone(g)).rc(), b: b.clone().rc() }
    } else {
        Val::Trn2 { a: a.clone().rc(), f: Val::DConform(Rc::clone(g)).rc() }
    }} else { Lis {
        l: Rc::new((0..a.len()).map(|n| {
            let l = a.index(env, n); extend(env, &l, b, g)
        }).collect()),
        fill: a.fill().rc()
    }}
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

#[allow(clippy::needless_borrow)]
pub fn scan_pairs(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if a.len() == 0 {
        return Lis{ l: Rc::new(Vec::new()), fill: a.fill().rc() };
    }
    let elems = a.iterf().collect::<Vec<_>>();
    let mut list = Vec::with_capacity(elems.len());
    let first = if let Some(b) = b { g.dyad(env, b, &elems[0]) } else { elems[0].clone() };
    list.push(first);
    for i in 1..elems.len() {
        list.push(g.dyad(env, &elems[i-1], &elems[i]));
    }
    list.into_iter().collect::<Val>()
}

pub fn stencil(env: &mut Env, a: &Val, b: Option<&Val>, f: &Rc<Val>, g: &Rc<Val>) -> Val {
    let size: usize = match f.call(env, a, b) {
        Num(n) => n as usize,
        // we could do something smart here like reshaping the output or using
        // multiple dimensions but uh
        _ => return NAN,
    };
    if !a.is_finite() { return NAN; }
    // 1234567 3╫◄ = (123)(234)(345)(456)(567) l-n+1
    (0..(a.len() + 1).saturating_sub(size)).map(|n| {
        g.call(env, &a.iterf().skip(n).take(size).cloned().collect(), b)
    }).collect()

}