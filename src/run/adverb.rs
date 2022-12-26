use super::{Val, Env, NAN};
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
adverb!(@env, a .monadic g _b => g.monad(env, a) );
adverb!(@env, a f .valences g b => (if b.is_none() {f} else {g}).call(env, a, b));
adverb!(@env, a f .over g b => {
    let l = f.monad(env, a);
    let r = b.map(|b| f.monad(env, b));
    g.call(env, l, r)
});

adverb!(@env, a f .overleft  g b => { let l = f.monad(env, a); g.call(env, l, b) });
adverb!(@env, a f .overright g b => { let r = b.map(|b| f.monad(env, b)); g.call(env, a, r) });
adverb!(@env, a f .forkleft  g b => { let l = f.call(env, a.c(), b.c()); g.dyad(env, l, b.unwrap_or(a)) });
adverb!(@env, a f .forkright g b => { let r = f.call(env, a.c(), b); g.dyad(env, a, r) });

adverb!(@env, a .each g b => 
    if a.is_scalar() && b.iter().all(|x| x.is_scalar()) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        if (a.is_infinite() && b.is_infinite())
        || (a.is_infinite() && b.is_scalar()  )
        || (a.is_scalar()   && b.is_infinite()) {
            Val::Fork { a: a.rc(), 
                        f: Val::Av(conform, None, Rc::clone(g)).rc(), 
                        b: b.rc() }
        } else if a.is_scalar() {
            b.into_iterf().map(|x| g.dyad(env, a.c(), x)).collect()
        } else if b.is_scalar() {
            a.into_iterf().map(|x| g.dyad(env, x, b.c())).collect()
        } else {
            let len = usize::min(a.len(), b.len());
            a.itertake(env, len).zip(b.itertake(env, len))
                .map(|(a, b)| g.dyad(env, a, b)).collect()
        }
    } else { eachleft(env, a, None, None, g) }
);

adverb!(@env, a .eachleft g b =>
    if a.is_scalar() {
        g.call(env, a, b)
    } else if a.is_infinite() {
        if let Some(b) = b {
            Val::Fork { a: a.rc(), f: Rc::clone(g), b: b.rc() }
        } else {
            Val::Trn2 { a: a.rc(), f: Rc::clone(g) }
        }
    } else { a.into_iterf().map(|x| g.call(env, x, b.c())).collect() }
);

adverb!(@env, a .conform g b =>
    if a.is_scalar() && b.iter().all(|x| x.is_scalar()) {
        g.call(env, a, b)
    } else if let Some(b) = b {
        if (a.is_infinite() && b.is_infinite())
        || (a.is_infinite() && b.is_scalar())
        || (a.is_scalar() && b.is_infinite()) {
            Val::Fork { a: a.rc(), 
                        f: Val::Av(conform, None, Rc::clone(g)).rc(), 
                        b: b.rc() }
        } else if a.is_scalar() {
            b.into_iterf().map(|x| conform(env, a.c(), Some(x), None, g)).collect()
        } else if b.is_scalar() {
            a.into_iterf().map(|x| conform(env, x, Some(b.c()), None, g)).collect()
        } else {
            let len = usize::min(a.len(), b.len());
            a.itertake(env, len).zip(b.itertake(env, len))
                .map(|(a, b)| conform(env, a, Some(b), None, g))
                .collect()
        }
    } else { extend(env, a, None, None, g) }
);

adverb!(@env, a .extend g b =>
    if a.is_scalar() {
        g.call(env, a, b)
    } else if a.is_infinite() { 
        if let Some(b) = b {
            Val::Fork { a: a.rc(), f: Val::Av(conform, None, Rc::clone(g)).rc(), b: b.rc() }
        } else {
            Val::Trn2 { a: a.rc(), f: Val::Av(conform, None, Rc::clone(g)).rc() }
        }
    } else { a.into_iterf().map(|x| extend(env, x, b.c(), None, g)).collect()}
);

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
val });


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
    let num = f.call(env, a.c(), b.c()).try_int().map_or(0, |x| x.try_into().unwrap_or(0));
    let mut values = Vec::with_capacity(num);
    values.push(a.c());
    let mut val = a;
    for _ in 0..num {
        val = g.call(env, val, b.c());
        values.push(val.c());
    }
    Val::lis(values)
});

adverb!(@env, a f .power g b => {
    let num = f.call(env, a.c(), b.c()).try_int().map_or(0, |x| x.try_into().unwrap_or(0));
    let mut val = a;
    for _ in 0..num {
        val = g.call(env, val, b.c());
    }
    val
});

adverb!(@env, a .scanpairs g b => {
    if a.len() == 0 { return Val::lis_fill(Vec::new(), a.fill()); }
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
    let Some(index) = index.try_int().and_then(|x| usize::try_from(x).ok() ) else {return a};
    let Val::Lis{l, fill} = a else { return g.call(env, a, b); };
    let mut v = match Rc::try_unwrap(l) { Ok(l) => l, Err(l) => l.to_vec(), };
    if v.len() <= index { v.resize(index+1, (*fill).c()); }
    v[index] = drill_iter(env, core::mem::take(&mut v[index]), b, iter, g);
    Val::lis_fill(v, (*fill).c())
}

adverb!(@env, a .cycle g _b => {
    a.try_int().map_or(NAN, |a| g.index(env, (a as usize) % g.len()))
});