use crate::prelude::*;
use iter::FusedIterator;
use super::{Val::{self, Lis, Num, Int}, Env, NAN, adverb, func::left};

impl Val {

    #[allow(clippy::len_without_is_empty)] // shut up!!! shut up!!!!
    pub fn len(&self) -> usize { match self {
        Num(_) | Int(_) => 1,
        Lis { l, .. } => l.len(),
        _ => usize::MAX,
    }}

    pub fn fill(&self) -> Val { match self {
        Num(_) | Int(_) => self.c(),
        Lis { fill, .. } => (**fill).c(),
        _ => NAN, // good enough
    }}

    pub fn indexval(&self, env: &mut Env, index: Val) -> Val {
        match self {
            Num(_) | Int(_) => self.c(), // unchanged
            Lis { l, fill } => 
                if index.is_nan() {(**fill).c()} 
                else if let Some(index) = index.try_int() {
                    if index < 0 { return (**fill).c() }
                    l.get(index as usize).cloned().unwrap_or_else(|| (**fill).c())
                } else {(**fill).c()},
            x => x.monad(env, index)
        }
    }

    pub fn index(&self, env: &mut Env, index: usize) -> Val {
        match self {
            Num(_) | Int(_) => self.c(), // unchanged
            Lis { l, fill } => {
                l.get(index).cloned().unwrap_or_else(|| (**fill).c())
            },
            x => x.monad(env, Int(index as i64))
        }
    }

    pub fn iterinf<'r, 'io>(self, env: &'r mut Env<'io>) -> Box<dyn InconvenientIter<'r, 'io> + 'r> {
        if self.is_infinite() {
            bx(InfIter { i: 0, value: self, env })
        } else if let Lis{l, ..} = self {
            match Rc::try_unwrap(l) {
                Ok(l) => bx(l.into_iter()),
                // https://smallcultfollowing.com/babysteps/blog/2018/09/02/rust-pattern-iterating-an-over-a-rc-vec-t/
                Err(l) => bx((0..l.len()).map(move |x| l[x].c()))
            }
        } else { bx(iter::once(self)) }
    }

    pub fn lis(vec: Vec<Val>) -> Val {
        Self::lis_fill(vec, NAN)
    }

    pub fn lis_fill(vec: Vec<Val>, fill: Val) -> Val {
        Lis{l: Rc::new(vec), fill: fill.rc()}
    }

    // create a finite iterator of `&Val`s. this returns a single item for functions, so it will 
    // never be infinite AND it doesnt need a &mut env which is more convenient most of the time
    pub fn iterf<'v>(&'v self) -> Box<dyn GoodIter<&'v Val> + 'v> {
        if let Lis{l, ..} = self {
            bx(l.iter())
        } else {
            bx(iter::once(self))
        }
    }

    // like iterf but it clones its values EXCEPT if the vec has 1 ref then it drains them
    pub fn into_iterf(self) -> Box<dyn GoodIter<Val>> {
        if let Lis{l, ..} = self {
            match Rc::try_unwrap(l) {
                Ok(l) => bx(l.into_iter()),
                Err(l) => bx((0..l.len()).map(move |x| l[x].c()))
            }
        } else { bx(iter::once(self)) }
    }

    pub fn itertake(self, env: &mut Env, len: usize) -> Box<dyn GoodIter<Val>> {
        if self.is_infinite() {
            bx(self.iterinf(env).take(len).collect::<Vec<_>>().into_iter())
        } else {
            bx(self.into_iterf().take(len))
        }
    }

}

pub trait GoodIter<V>: Iterator<Item=V> + ExactSizeIterator + DoubleEndedIterator + FusedIterator {}
impl<F, V> GoodIter<V> for F where F: Iterator<Item=V> + ExactSizeIterator + DoubleEndedIterator + FusedIterator {}

pub trait InconvenientIter<'r, 'io>: Iterator<Item=Val> {}
impl<'r, 'io> InconvenientIter<'r, 'io> for InfIter<'r, 'io> {}
impl<'r, 'io, F> InconvenientIter<'r, 'io> for F where F: GoodIter<Val> {}


func!(@env, a :index b => {
    let mut a = a;
    if b.is_nan() { return a.fill() }
    if a.is_infinite() { return a.monad(env, b) }
    for n in 0..b.len() {
        if a.is_scalar() { return a.c(); }
        let i = b.index(env, n);
        if i.is_nan() {
            if n+1 == b.len() {return a}
            return match a {
                Lis {l, ..} => {
                    let slice = b.iterinf(env).skip(n+1).collect::<Val>();
                    l.iter().map(|x| index(env, x.c(), Some(slice.c()))).collect::<Val>()
                }
                _ => iter::empty::<Val>().collect()
            };
        }
        a = a.indexval(env, i);
    }
    a
});

pub struct InfIter<'r, 'io> {
    i: i64,
    value: Val,
    env: &'r mut Env<'io>,
}

impl<'r, 'io> Iterator for InfIter<'r, 'io> {
    type Item = Val;
    fn next(&mut self) -> Option<Self::Item> {
        let val = Some(self.value.monad(self.env, Int(self.i)));
        self.i += 1; val
    }
    fn size_hint(&self) -> (usize, Option<usize>) { (usize::MAX, None) }
}

impl FromIterator<Val> for Val {
    fn from_iter<T: IntoIterator<Item = Val>>(iter: T) -> Self {
        Val::lis(iter.into_iter().collect())
    }
}

pub fn iiota(prefix: Vec<i64>, arg: &[i64]) -> Vec<Val> {
    if arg.is_empty() {
        return vec![prefix.into_iter().map(Int).collect()]
    }
    let iter: Box<dyn Iterator<Item=i64>> = if arg[0] > 0 {
        bx(0..arg[0])
    } else {
        bx((0..arg[0].abs()).rev())
    };
    iter.flat_map(|i| iiota([&prefix[..], &[i]].concat(), &arg[1..])).collect()
}

pub fn iota_scalar(arg: i64) -> Val {
    let iter: Box<dyn Iterator<Item=i64>> = if arg > 0 {
        bx(0..arg)
    } else {
        bx((0..arg.abs()).rev())
    };
    Val::lis(iter.map(Int).collect())
}

func!(a :len => match a {
    Num(_) | Int(_) => Int(1),
    Lis { l, .. } => Int(l.len() as i64),
    _ => Val::flt(f64::INFINITY),
});
func!(a :iota => match a {
    Lis{l, ..} => Val::lis(iiota(
        Vec::new(), &l.iter().cloned().filter_map(|x| x.try_int()).collect::<Vec<i64>>())),
    Num(n) => if n.is_infinite() {Val::Func(left)} else {iota_scalar(n.re as i64)},
    Int(n) => iota_scalar(n),
    _ => Val::Av(adverb::constant, None, NAN.rc()),
});
func!(a :pair b => Val::lis(vec![a, b]));
func!(a :enlist => Val::lis(vec![a]));

func!(a :getfill => a.fill());
func!(a :setfill b => match a { Lis {l, ..} => Lis {l, fill: b.rc()}, a => a });

func!(a :ravel => { let mut list = Vec::new(); vecravel(a, &mut list); Val::lis(list) });
pub fn vecravel(arg: Val, list: &mut Vec<Val>) {
    match arg {
        l @ Lis{ .. } => for i in l.into_iterf() {vecravel(i, list)},
        _ => list.push(arg)
    }
}

func!(a :replist b => if !a.is_infinite() {
    (0..or_nan!(b.try_int())).flat_map(|_| a.iterf().cloned()).collect()
} else {a});

func!(a :concat b => {
    if a.is_infinite() || b.is_infinite() { return NAN }
    if let Lis{l, ..} = a { return match Rc::try_unwrap(l) {
        Ok(mut l) => { l.extend(b.iterf().cloned()); Val::lis(l) },
        Err(l) => l.iter().cloned().chain(b.into_iterf()).collect()
    }}
    a.into_iterf().chain(b.into_iterf()).collect()
});

func!(a :reverse => {
    if a.is_infinite() { return iter::empty::<Val>().collect() }
    a.into_iterf().rev().collect()
});

func!(@env, a :takeleft  b => reshape(env, a, b, false));
func!(@env, a :takeright b => reshape(env, a, b, true));

pub fn reshape(env: &mut Env, a: Val, b: Val, isright: bool) -> Val {
    if b.is_infinite() { return NAN };
    let mut shape = Vec::<usize>::new();
    let mut spot = None::<usize>; // in 2█↑, the spot is Some(1)
    let mut product = if isright {-1} else {1};
    for i in b.iterf() { 
        if i.is_nan() && spot.is_none() { // there can only be 1 spot 
            spot = Some(shape.len());
            shape.push(1);
        } else if let Some(n) = i.try_int() {
            product *= n as isize;
            shape.push(n.unsigned_abs() as usize);
        } else { return NAN }
    }
    // fill up the spot
    if let Some(index) = spot {
        fn divceil(x: usize, y: usize) -> usize {x / y + usize::from(x % y != 0)}
        let num = divceil(
            match &a { 
                Val::Av(t, _, c) if *t as usize == adverb::cycle as usize => c.len(),
                e => e.len() },
            product.unsigned_abs(),
        );
        shape[index] = num;
        product *= num as isize;
    }
    let fill = a.fill();
    let mut iter = if product >= 0 {
        // pick from left
        a.iterinf(env)
    } else {
        // pick from right
        let elems = product.unsigned_abs();
        let bee = if a.len() < elems {
            iter::repeat(a.fill()).take(elems - a.len()).chain(a.iterinf(env)).collect::<Val>()
        } else {
            let len = a.len() - elems;
            a.iterinf(env).skip(len).collect::<Val>()
        };
        bee.iterinf(env)
    };
    ireshape(&mut iter, &shape[..], &fill)
}

pub fn ireshape(a: &mut dyn Iterator<Item=Val>, b: &[usize], fill: &Val) -> Val {
    if b.is_empty() {return a.next().unwrap_or_else(|| fill.c())}
    let (pre, suf) = (b[0], &b[1..]);
    (0..pre).map(move |_| ireshape(a, suf, fill)).collect()
}

func!(a :dropleft  b => b.try_int().map_or(NAN, |b| drop(a, b)));
func!(a :dropright b => b.try_int().map_or(NAN, |b| drop(a, -b)));

pub fn drop(a: Val, b: i64) -> Val {
    if a.is_infinite() { return NAN; }
    if b >= 0 {
        a.into_iterf().skip(b as _).collect()
    } else {
        a.into_iterf().rev().skip(-b as _).rev().collect()
    }
}

func!(a :shape b => Val::lis_fill(
    ishape(&a, b.try_int().unwrap_or(i64::MAX)).iter().map(|x| Int(*x as i64)).collect(),
    Int(1)
));
pub fn ishape(a: &Val, lim: i64) -> Vec<usize> {
    if !a.is_list() { return vec![]; };
    let mut shp = vec![a.len()];
    if lim == 0 { return shp; }
    //if lim < 64 {shp.resize(lim as _, 0)}
    for v in a.iterf() {
        let inr = ishape(v, lim-1);
        if inr.len() + 1 > shp.len() { shp.resize(inr.len() + 1, 0); }
        for (n, &i) in inr.iter().enumerate() {
            if shp[n+1] < i { shp[n+1] = i; }
        }
    }
    shp
}

func!(@env, a :replicate b => { let fill = a.fill();
    Val::lis_fill(ireplicate(env, a, b), fill)
});

pub fn ireplicate(env: &mut Env, a: Val, b: Val) -> Vec<Val> {
    let mut lis = Vec::new();
    let (afill, bfill, len) = (a.fill(), b.fill(), a.len());
    for (l,r) in a.into_iterf().zip(b.itertake(env, len).chain(iter::repeat(bfill))) {
        if let Some(n) = r.try_int() { if n != 0 {
            let val = if n > 0 {l} else {afill.c()};
            for _ in 0..(n.abs() - 1) { lis.push(val.c()); }
            lis.push(val);
        }} else {
            lis.extend(ireplicate(env, l, r).into_iter());
        }
    }
    lis
}

func!(a :gradeup => if let Lis {l, ..} = a {
    let mut lis = (0..l.len()).collect::<Vec<_>>();
    lis.sort_by(|&a, &b| l[a].cmpval(&l[b]));
    Val::lis(lis.into_iter().map(|x| Int(x as i64)).collect())
} else { Val::lis(vec![Int(0)]) });

func!(a :gradedown => if let Lis {l, ..} = a {
    let mut lis = (0..l.len()).collect::<Vec<_>>();
    lis.sort_by(|&a, &b| l[a].cmpval(&l[b]).reverse());
    Val::lis(lis.into_iter().map(|x| Int(x as i64)).collect())
} else { Val::lis(vec![Int(0)]) } );

func!( a :sortup => if let Lis {l, ..} = a {
    let mut list = Rc::try_unwrap(l).unwrap_or_else(|x| (*x).c());
    list.sort_by(|a, b| a.cmpval(b));
    Val::lis(list)
} else { Val::lis(vec![a]) } );

func!(a :sortdown => if let Lis {l, ..} = a {
    let mut list = Rc::try_unwrap(l).unwrap_or_else(|x| (*x).c());
    list.sort_by(|a, b| a.cmpval(b).reverse());
    Val::lis(list)
} else { Val::lis(vec![a]) });

func!(a :binsup b => if let Lis {l, ..} = a {
    Int(l.partition_point(|x| x.cmpval(&b).is_le()) as i64)
} else { Int(0) } );

func!( a :binsdown b => if let Lis {l, ..} = a {
    Int(l.partition_point(|x| x.cmpval(&b).is_gt()) as i64)
} else { Int(0) });

func!( @env, a :group b => {
    if a.is_infinite() { return NAN; }
    let mut lis: Vec<Vec<Val>> = Vec::new();
    let len = a.len();
    for (l, r) in a.into_iterf().zip(b.itertake(env, len)) {
        for i in r.into_iterf() {
            if i.is_nan() { continue }
            // epic type inference fail
            if let Some(i) = i.try_int().and_then(|x|->Option<usize> {x.try_into().ok()}) {
                if i >= lis.len() { lis.resize(i + 1, Vec::new()); }
                lis[i].push(l.c());
            };
        }
    }
    Val::lis_fill(
        lis.into_iter().map(Val::lis).collect(),
        Val::lis(Vec::new())
    )
});

func!(a :find b => {
    let Lis{l: a, ..} = a else { return NAN };
    let b = match b { Lis{l, ..} if l.len() != 0 => l, _ => Rc::new(vec![b]) };
    let mut res = vec![NAN; a.len()];
    let mut i = 0;
    while i+b.len() <= a.len() {
        if a[i..i+b.len()] == b[..] {
            for n in 0..b.len() { res[i+n] = Int(n as i64); }
            i += b.len();
        } else { i += 1; }
    }
    Val::lis(res)
});

func!(a :occcount => {
    let mut map = HashMap::new();
    let mut lis = Vec::with_capacity(a.len());
    for i in a.into_iterf() {
        let ptr = map.entry(i).or_insert(0);
        lis.push(Int(*ptr));
        *ptr += 1;
    }
    Val::lis(lis)
});

func!(a :uio b => {
    let mut used_matches = vec![false; a.len()];
    b.into_iterf().map(|i| {
        for (n, j) in a.iterf().enumerate() { if !used_matches[n] && i == *j {
            used_matches[n] = true;
            return Int(n as _)
        }}
        NAN
    }).collect()
});

func!(a :domainto b => {
    let mut lis = Vec::new();
    if let Some(b) = b.try_int() { domain_to(&mut lis, a, b, vec![]) }
    Val::lis(lis)
});

fn domain_to(lis: &mut Vec<Val>, a: Val, depth: i64, prefix: Vec<Val>) {
    if a.is_scalar() || depth == 0 {
        lis.push(Val::lis(prefix));
    } else {
        for (n, i) in a.into_iterf().enumerate() {
            let p = prefix.iter().cloned().chain(iter::once(Int(n as _))).collect();
            domain_to(lis, i, depth - 1, p);
        }
    }
}