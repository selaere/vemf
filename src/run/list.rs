use std::{rc::Rc, iter::FusedIterator};
use super::{Val::{self, Lis, Num, Int}, Env, NAN};

impl Val {

    pub fn len(&self) -> usize { match self {
        Num(_) | Int(_) => 1,
        Lis { l, .. } => l.len(),
        _ => usize::MAX,
    }}

    pub fn fill(&self) -> Val { match self {
        Num(_) | Int(_) => self.clone(),
        Lis { fill, .. } => (**fill).clone(),
        _ => NAN, // good enough
    }}

    pub fn indexval(&self, env: &mut Env, index: Val) -> Val {
        match self {
            Num(_) | Int(_) => self.clone(), // unchanged
            Lis { l, fill } => 
                if index.is_nan() {(**fill).clone()} 
                else if let Some(index) = index.try_int() {
                    if index < 0 { return (**fill).clone() }
                    l.get(index as usize).cloned().unwrap_or_else(|| (**fill).clone())
                } else {(**fill).clone()},
            x => x.monad(env, index)
        }
    }

    pub fn index(&self, env: &mut Env, index: usize) -> Val {
        match self {
            Num(_) | Int(_) => self.clone(), // unchanged
            Lis { l, fill } => {
                l.get(index).cloned().unwrap_or_else(|| (**fill).clone())
            },
            x => x.monad(env, Int(index as i64))
        }
    }

    pub fn index_at_depth(&self, env: &mut Env, index: Val) -> Val {
        let mut value = self.clone();
        for n in 0..index.len() {
            if value.is_scalar() { return value.clone(); }
            let i = index.index(env, n);
            if i.is_nan() {
                if n+1 == index.len() {return value}
                return match value {
                    Lis {l, ..} => {
                        let slice = index.iterinf(env).skip(n+1).collect::<Val>();
                        l.iter().map(|x| x.index_at_depth(env, slice.clone())).collect::<Val>()
                    }
                    _ => std::iter::empty::<Val>().collect()
                };
            }
            value = value.indexval(env, i);
        }
        value
    }

    pub fn iterinf<'r>(self, env: &'r mut Env) -> Box<dyn InconvenientIter<'r> + 'r> {
        if self.is_infinite() {
            Box::new(InfIter { i: 0, value: self, env })
        } else if let Lis{l, ..} = self {
            match Rc::try_unwrap(l) {
                Ok(l) => Box::new(l.into_iter()),
                // https://smallcultfollowing.com/babysteps/blog/2018/09/02/rust-pattern-iterating-an-over-a-rc-vec-t/
                Err(l) => Box::new((0..l.len()).map(move |x| l[x].clone()))
            }
        } else { Box::new(std::iter::once(self)) }
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
            Box::new(l.iter())
        } else {
            Box::new(std::iter::once(self))
        }
    }

    // like iterf but it clones its values EXCEPT if the vec has 1 ref then it drains them
    pub fn into_iterf(self) -> Box<dyn GoodIter<Val>> {
        if let Lis{l, ..} = self {
            match Rc::try_unwrap(l) {
                Ok(l) => Box::new(l.into_iter()),
                // https://smallcultfollowing.com/babysteps/blog/2018/09/02/rust-pattern-iterating-an-over-a-rc-vec-t/
                Err(l) => Box::new((0..l.len()).map(move |x| l[x].clone()))
            }
        } else { Box::new(std::iter::once(self)) }
    }

    pub fn itertake(self, env: &mut Env, len: usize) -> Box<dyn GoodIter<Val>> {
        if self.is_infinite() {
            Box::new(self.iterinf(env).take(len).collect::<Vec<_>>().into_iter())
        } else {
            Box::new(self.into_iterf().take(len))
        }
    }

}

pub trait GoodIter<V>: Iterator<Item=V> + ExactSizeIterator + DoubleEndedIterator + FusedIterator {}
impl<F, V> GoodIter<V> for F where F: Iterator<Item=V> + ExactSizeIterator + DoubleEndedIterator + FusedIterator {}

pub trait InconvenientIter<'r>: Iterator<Item=Val> {}
impl<'r> InconvenientIter<'r> for InfIter<'r> {}
impl<'r, F> InconvenientIter<'r> for F where F: GoodIter<Val> {}

pub struct InfIter<'r> {
    i: i64,
    value: Val,
    env: &'r mut Env,
}

impl<'r> Iterator for InfIter<'r> {
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

pub fn iota(prefix: Vec<i64>, arg: &[i64]) -> Val {
    if arg.is_empty() {
        return prefix.into_iter().map(Int).collect()
    }
    let iter: Box<dyn Iterator<Item=i64>> = if arg[0] > 0 {
        Box::new(0..arg[0])
    } else {
        Box::new((0..arg[0].abs()).rev())
    };
    Val::lis(iter.map(|i| iota([&prefix[..], &[i]].concat(), &arg[1..])).collect())
}


pub fn iota_scalar(arg: i64) -> Val {
    let iter: Box<dyn Iterator<Item=i64>> = if arg > 0 {
        Box::new(0..arg)
    } else {
        Box::new((0..arg.abs()).rev())
    };
    Val::lis(iter.map(Int).collect())
}

pub fn ravel(arg: Val, list: &mut Vec<Val>) {
    match arg {
        l @ Lis{ .. } => for i in l.into_iterf() {ravel(i, list)},
        _ => list.push(arg)
    }
}

pub fn concat(a: Val, b: Val) -> Val {
    if a.is_infinite() || b.is_infinite() { return NAN }
    if let Lis{l, ..} = a {
        return match Rc::try_unwrap(l) {
            Ok(mut l) => {
                // println!("efficient!!!");
                l.extend(b.iterf().cloned());
                Val::lis(l)
            },
            Err(l) => l.iter().cloned().chain(b.into_iterf()).collect()
        }
    }
    a.into_iterf().chain(b.into_iterf()).collect()
}

pub fn reverse(a: Val) -> Val {
    if a.is_infinite() { return std::iter::empty::<Val>().collect() }
    a.into_iterf().rev().collect()
}

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
            match &a { Val::DCycle(c) => c.len(), e => e.len() },
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
            std::iter::repeat(a.fill()).take(elems - a.len()).chain(a.iterinf(env)).collect::<Val>()
        } else {
            let len = a.len() - elems;
            a.iterinf(env).skip(len).collect::<Val>()
        };
        bee.iterinf(env)
    };
    reshape_iter(&mut iter, &shape[..], &fill)
}

pub fn reshape_iter(a: &mut dyn Iterator<Item=Val>, b: &[usize], fill: &Val) -> Val {
    if b.is_empty() {return a.next().unwrap_or_else(|| fill.clone())}
    let (pre, suf) = (b[0], &b[1..]);
    (0..pre).map(move |_| reshape_iter(a, suf, fill)).collect()
}

pub fn dropleft(a: Val, b: i64) -> Val {
    if b < 0 {return dropright(a, -b); }
    if a.is_infinite() { return NAN; }
    a.into_iterf().skip(b as _).collect()
}

pub fn dropright(a: Val, b: i64) -> Val {
    if b < 0 { return dropleft(a, -b); }
    if a.is_infinite() { return a; }
    a.into_iterf().rev().skip(b as _).rev().collect()
}

pub fn shape(a: &Val) -> Vec<usize> {
    if let Lis { l, .. } = a {
        let mut shp = vec![l.len()];
        for v in &**l {
            let inr = shape(v);
            if inr.len() + 1 > shp.len() { shp.resize(inr.len() + 1, 0); }
            for (n, &i) in inr.iter().enumerate() {
                if shp[n+1] < i { shp[n+1] = i; };
            }
        }
        shp
    } else { vec![] }
}


pub fn replicate(env: &mut Env, a: Val, b: Val) -> Vec<Val> {
    let mut lis = Vec::new();
    let (afill, bfill, len) = (a.fill(), b.fill(), a.len());
    for (l,r) in a.into_iterf().zip(b.itertake(env, len).chain(std::iter::repeat(bfill))) {
        if let Some(n) = r.try_int() {
            if n != 0 {
                let val = if n > 0 {l} else {afill.clone()};
                for _ in 0..(n.abs() - 1) { lis.push(val.clone()); }
                lis.push(val);
            }
        } else {
            lis.extend(replicate(env, l, r).into_iter());
        }
    }
    lis
}

pub fn grade_up(a: Val) -> Val {
    if let Lis {l, ..} = a {
        let mut lis = (0..l.len()).collect::<Vec<_>>();
        lis.sort_by(|a, b| l[*a].cmpval(&l[*b]));
        Val::lis(lis.into_iter().map(|x| Int(x as i64)).collect())
    } else {
        Val::lis(vec![Int(0)])
    }
}

pub fn grade_down(a: Val) -> Val {
    if let Lis {l, ..} = a {
        let mut lis = (0..l.len()).collect::<Vec<_>>();
        lis.sort_by(|a, b| l[*a].cmpval(&l[*b]).reverse());
        Val::lis(lis.into_iter().map(|x| Int(x as i64)).collect())
    } else { Val::lis(vec![Int(0)]) }
}

pub fn sort_up(a: Val) -> Val {
    if let Lis {l, ..} = a {
        let mut list = Rc::try_unwrap(l).unwrap_or_else(|x| (*x).clone());
        list.sort_by(|a, b| a.cmpval(b));
        Val::lis(list)
    } else { Val::lis(vec![a]) }
}

pub fn sort_down(a: Val) -> Val {
    if let Lis {l, ..} = a {
        let mut list = Rc::try_unwrap(l).unwrap_or_else(|x| (*x).clone());
        list.sort_by(|a, b| a.cmpval(b).reverse());
        Val::lis(list)
    } else { Val::lis(vec![a]) }
}

pub fn bins_up(a: &Val, b: &Val) -> Val {
    if let Lis {l, ..} = a {
        Int(l.partition_point(|x| x.cmpval(b).is_le()) as i64)
    } else { Int(0) }
}

pub fn bins_down(a: &Val, b: &Val) -> Val {
    if let Lis {l, ..} = a {
        Int(l.partition_point(|x| x.cmpval(b).is_gt()) as i64)
    } else { Int(0) }
}
