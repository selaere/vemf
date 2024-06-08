use super::{list::idepth, Env, Val, NAN};
use crate::prelude::*;

pub type AvT = fn(&mut Env, Val, Option<Val>, Option<&Rc<Val>>, &Rc<Val>) -> Val;

#[macro_export]
macro_rules! adverb {
    (@$env:tt, $a:ident $f:ident .$name:ident $g:ident $b:ident => $contents:expr) => {
        pub fn $name(
            $env: &mut $crate::Env, $a: Val, $b: Option<Val>, fg: Option<&Rc<Val>>, $g: &Rc<Val>
        ) -> Val { let $f = fg.unwrap_or($g); $contents }
    };
    (@$env:tt, $a:ident .$name:ident $g:ident $b:ident => $contents:expr) => {
        pub fn $name(
            $env: &mut $crate::Env, $a: Val, $b: Option<Val>, _: Option<&Rc<Val>>, $g: &Rc<Val>
        ) -> Val { $contents }
    };
}

adverb!(@env, a .swap g b => g.dyad(env, b.unwrap_or_else(|| a.c()), a));
adverb!(@_ , _a .constant g _b => (**g).c() );
adverb!(@env, a .toleft g _b => g.monad(env, a) );
adverb!(@env, a .toright g b => g.monad(env, b.unwrap_or(a)) );
adverb!(@env, a f .valences g b => (if b.is_none() {f} else {g}).call(env, a, b));
adverb!(@env, a f .over g b => {
    let l = f.monad(env, a);
    let r = b.map(|b| f.monad(env, b));
    g.call(env, l, r)
});
adverb!(@env, a f .bind g _b => g.dyad(env, a, (**f).c()));
adverb!(@env, a f .atop g b  => { let x = f.call(env, a, b); g.monad(env, x) });
adverb!(@env, a f .overleft  g b => { let l = f.monad(env, a); g.call(env, l, b) });
adverb!(@env, a f .overright g b => { let r = b.map(|b| f.monad(env, b)); g.call(env, a, r) });
adverb!(@env, a f .forkleft  g b => { let l = f.call(env, a.c(), b.c()); g.dyad(env, l, b.unwrap_or(a)) });
adverb!(@env, a f .forkright g b => { let r = f.call(env, a.c(), b); g.dyad(env, a, r) });

adverb!(@env, a .each        g b => at_depths(env, a, b, g, 1, 1));
adverb!(@env, a .eachleft    g b => at_depths(env, a, b, g, 1, 0));
adverb!(@env, a .eachright   g b => { let b = b.unwrap_or_else(|| a.c()); at_depths(env, a, Some(b), g, 0, 1)} ) ;
adverb!(@env, a .conform     g b => at_depths(env, a, b, g,-1,-1));
adverb!(@env, a .extend      g b => at_depths(env, a, b, g,-1, 0));
adverb!(@env, a .extendright g b => { let b = b.unwrap_or_else(|| a.c()); at_depths(env, a, Some(b), g, 0, -1)} ) ;

adverb!(@env, a .eachtrim g b => {
    let Some(b) = b else { return eachleft(env, a, None, None, g); };
    if !a.is_list() && !b.is_list() {
        return Val::Fork(a.rc(), g.c(), b.rc());
    }
    (0..match (a.is_scalar(), b.is_scalar()) {
        ( true,  true) => return g.call(env, a, Some(b)),
        (false,  true) => a.len(),
        ( true, false) => b.len(),
        (false, false) => usize::min(a.len(), b.len()),
    }).map(|n| {
        let l = a.index(env, n); let r = b.index(env, n); g.dyad(env, l, r)
    }).collect()
});

adverb!(@env, a .scan g b => {
    if a.is_infinite() { return NAN; }
    let mut values = Vec::with_capacity(a.len());
    let mut iter = a.into_iterf();
    let Some(start) = iter.next() else { return b.unwrap_or(NAN); };
    let mut val = match b {
        Some(b) => g.dyad(env, b, start),
        None => start,
    };
    values.push(val.c());
    for i in iter {
        val = g.dyad(env, val, i);
        values.push(val.c());
    }
    Val::lis(values)
});

adverb!(@env, a .reduce g b => {
    if a.is_infinite() { return NAN; }
    let mut iter = a.into_iterf();
    let Some(start) = iter.next() else { return b.unwrap_or(NAN); };
    let mut val = match b {
        Some(b) => g.dyad(env, b, start),
        None => start,
    };
    for i in iter { val = g.dyad(env, val, i); }
    val
});

adverb!(@env, a f .untilscan g b => {
    let mut values = vec![a.c()];
    let mut val = a;
    while !f.monad(env, val.c()).as_bool() {
        val = g.call(env, val.c(), b.c());
        values.push(val.c());
    }
    Val::lis(values)
});

adverb!(@env, a f .until g b => {
    let mut val = a;
    while !f.monad(env, val.c()).as_bool() {
        val = g.call(env, val.c(), b.c());
    }
    val
});

adverb!(@env, a f .untilscancmp g b => {
    let mut values = vec![a.c()];
    let mut val = a;
    loop {
        let tried = g.call(env, val.c(), b.c());
        if f.dyad(env, tried.c(), val).as_bool() { break }
        values.push(tried.c());
        val = tried;
    }
    Val::lis(values)
});

adverb!(@env, a f .untilcmp g b => {
    let mut val = a;
    loop {
        let tried = g.call(env, val.c(), b.c());
        if f.dyad(env, tried.c(), val.c()).as_bool() { break }
        val = tried;
    }
    val
});

adverb!(@env, a f .powerscan g b => {
    let num = f.call(env, a.c(), b.c());
    if let Some(num) = num.try_int() {
        let mut values = Vec::with_capacity(num as _);
        values.push(a.c());
        let mut val = a;
        for _ in 0..num {
            val = g.call(env, val, b.c());
            values.push(val.c());
        }
        Val::lis(values)
    } else if num.is_list() {
        let mut buf = Vec::with_capacity(num.len());
        for (i, f) in num.into_iterf().enumerate() {
            let l = a.index(env, i);
            buf.push(powerscan(env, l, b.c(), Some(&f.rc()), g));
        }
        Val::lis(buf)
    } else { NAN }
});

adverb!(@env, a f .power g b => {
    let num = f.call(env, a.c(), b.c());
    if let Some(num) = num.try_int() {
        let mut val = a;
        for _ in 0..num {
            val = g.call(env, val, b.c());
        }
        val
    } else if num.is_list() {
        let mut buf = Vec::with_capacity(num.len());
        for (i, f) in num.into_iterf().enumerate() {
            let l = a.index(env, i);
            buf.push(power(env, l, b.c(), Some(&f.rc()), g));
        }
        Val::lis(buf)
    } else { NAN }
});

adverb!(@env, a .scanpairs g b => {
    if a.is_empty() { return Val::lis_fill(Vec::new(), a.fill()); }
    let elems = a.iterf().collect::<Vec<_>>();
    let mut list = Vec::with_capacity(elems.len());
    let first = if let Some(b) = b { g.dyad(env, b, elems[0].c()) } else { elems[0].c() };
    list.push(first);
    for i in 1..elems.len() { list.push(g.dyad(env, elems[i-1].c(), elems[i].c())); }
    Val::lis(list)
});

adverb!(@env, a f .stencil g b => {
    if let Some(size) = f.call(env, a.c(), b.c()).try_int().map(|x| x as usize) {
        if a.is_infinite() { return Val::lis(Vec::new()); }
        (0..(a.len() + 1).saturating_sub(size)).map(|n| {
            g.call(env, a.iterf().skip(n).take(size).cloned().collect(), b.c())
        }).collect()
    } else {
        // we could do something smart here like using multiple dimensions but uh
        Val::lis(Vec::new())
    }
});

adverb!(@env, a f .drill g b => {
    let iter = (**f).call(env, a.c(), b.c()).into_iterf();
    drill_iter(env, a, b, iter, g)
});

pub fn drill_iter(
    env: &mut Env,
    a: Val,
    b: Option<Val>,
    mut iter: Box<dyn super::list::GoodIter<Val>>,
    g: &Rc<Val>,
) -> Val {
    let index = iter.next();
    let Some(index) = index else { return g.call(env, a, b); };
    if index.is_nan() { return a; }
    let Some(index) = index.try_int().and_then(|x| usize::try_from(x).ok()) else {return a};
    let Val::Lis{l, fill} = a else { return g.call(env, a, b); };
    let mut v = match Rc::try_unwrap(l) { Ok(l) => l, Err(l) => l.to_vec(), };
    if v.len() <= index { v.resize(index+1, (*fill).c()); }
    v[index] = drill_iter(env, core::mem::take(&mut v[index]), b, iter, g);
    Val::lis_fill(v, (*fill).c())
}

adverb!(@env, a f .amend g b => {
    let Some(indices) = f.call(env, a.c(), b.c()).iterf()
        .map(|x| x.try_int().and_then(|x| usize::try_from(x).ok()))
        .collect::<Option<Vec<usize>>>() else {return a};
    let Val::Lis{l, fill} = a else { return a; };
    let mut l = match Rc::try_unwrap(l) { Ok(l) => l, Err(l) => l.to_vec(), };
    let before = indices.iter().copied().map(|i| l.get(i).unwrap_or(&fill).c()).collect::<Val>();
    let after = g.call(env, before, b);
    if !after.is_list() {
        let max = indices.iter().max();
        if let Some(&max) = max { if l.len() <= max { l.resize(max, (*fill).c()); }}
        for i in indices.into_iter() { l[i] = after.c(); }
    } else {
        let (mut niter, mut viter) = (indices.iter(), after.into_iterf());
        while let Some(new) = viter.next() {
            if let Some(&i) = niter.next() { // replace items:
                if l.len() <= i { l.resize(i+1, (*fill).c()) }
                l[i] = new;
            } else { // insert items:
                let right = l.split_off(indices.last().copied().unwrap_or(l.len()-1) + 1);
                l.push(new); l.extend(viter); l.extend(right);
            break; }
        } // remove items:
        let to_be_removed = niter.copied().collect::<HashSet<_>>();
        let mut iter = 0..; 
        l.retain(|_| !to_be_removed.contains(&iter.next().unwrap()));
    }
    Val::Lis { l: Rc::new(l), fill }
});

adverb!(@env, a .cycle g _b => {
    a.try_int().map_or(NAN, |a| g.index(env, (a as usize) % g.len()))
});
adverb!(@env, a f .depthleft g b => {
    let mut d = or_nan!(f.try_int());
    if d < 0 { d = 0.max(d+idepth(&a)); }
    at_depths(env, a, b, g, d, 0)
});
adverb!(@env, a f .depthright g b => {
    let b = b.unwrap_or_else(|| a.c());
    let mut d = or_nan!(f.try_int());
    if d < 0 { d = 0.max(d+idepth(&b)); }
    at_depths(env, a, Some(b), g, 0, d)
});
adverb!(@env, a f .depthboth g b => {
    let (mut d1,mut d2) = or_nan!(Option::zip(f.index(env,0).try_int(), f.index(env,1).try_int()));
    if d1 < 0 { d1 = 0.max(d1+idepth(&a)); }
    if let Some(ref b) = b { if d2 < 0 { d2 = 0.max(d2+idepth(b)); } }
    at_depths(env, a, b, g, d1, d2)
});

pub fn at_depths(env: &mut Env, a: Val, b: Option<Val>, g: &Rc<Val>, d1: i64, d2: i64) -> Val {
    #[inline] fn dec(x: i64) -> i64 { if x==0 {0} else {x-1} }
    macro_rules! deeper {($l:expr,$r:expr)=>{ at_depths(env, $l, $r, g, dec(d1), dec(d2))}}
    if let Some(b) = b {
        // Dyadic case
        if a.is_infinite() && d1!=0 || b.is_infinite() && d2!=0 {
            return Val::Fork(a.rc(), Val::Av(depthboth, 
                Some(Val::lis(vec![Val::Int(dec(d1)),Val::Int(dec(d2))]).rc()), g.c()
            ).rc(), b.rc());
        }
        let items = (0..match (a.is_scalar()||d1==0, b.is_scalar()||d2==0) {
            ( true,  true) => return g.dyad(env, a, b),
            (false,  true) => a.len(),
            ( true, false) => b.len(),
            (false, false) => usize::max(a.len(), b.len()),
        }).map(|n| {
            let l = if d1==0 {a.c()} else {a.index(env, n)};
            let r = if d2==0 {b.c()} else {b.index(env, n)};
            deeper!(l,Some(r))
        }).collect();
        let fill = if !a.fill().is_nan() && !b.fill().is_nan() { deeper!(a.fill(), Some(b.fill())) }
                   else {NAN};
        Val::lis_fill(items, fill)
    // Monadic case
    } else if a.is_scalar() || d1==0 {
        g.monad(env, a)
    } else if a.is_infinite() {
        Val::atop(a.rc(), Val::Av(depthleft, Some(Val::Int(dec(d1)).rc()), g.c()).rc())
    } else {
        let fill = if !a.fill().is_nan() { deeper!(a.fill(), None) } else { NAN };
        Val::lis_fill(a.into_iterf().map(|x| deeper!(x, None)).collect(), fill)
    }
}



