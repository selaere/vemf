use std::rc::Rc;

use super::Val::{self, Num, Lis};
use super::Env;
use super::NAN;

impl Val {
    
    pub fn lenf(&self) -> f64 { match self {
        Num(_) => 1.,
        Lis { l, .. } => l.len() as f64,
        _ => f64::INFINITY,
    }}

    pub fn len(&self) -> usize { match self {
        Num(_) => 1,
        Lis { l, .. } => l.len(),
        _ => usize::MAX,
    }}

    pub fn fill(&self) -> Val { match self {
        Num(n) => Num(*n),
        Lis { fill, .. } => (**fill).clone(),
        _ => NAN, // good enough
    }}

    pub fn indexval(&self, env: &mut Env, index: &Val) -> Val {
        match self {
            Num(n) => Num(*n), // unchanged
            Lis { l, fill } => if let &Num(index) = index {
                if index < 0. || index.is_nan() { return (**fill).clone() }
                l.get(index as usize).cloned().unwrap_or_else(|| (**fill).clone())
            } else {(**fill).clone()},
            x => x.monad(env, index)
        }
    }

    pub fn index(&self, env: &mut Env, index: usize) -> Val {
        match self {
            Num(n) => Num(*n), // unchanged
            Lis { l, fill } => {
                l.get(index).cloned().unwrap_or_else(|| (**fill).clone())
            },
            x => x.monad(env, &Num(index as f64))
        }
    }

    pub fn index_at_depth(&self, env: &mut Env, index: &Val) -> Val {
        let mut value = self.clone();
        for n in 0..index.len() {
            if let Num(n) = value { return Num(n); }
            let i = index.index(env, n);
            if i.is_nan() {
                if n+1 == index.len() {return value}
                match value {
                    Lis {l, ..} => {
                        let slice = index.iter(env).skip(n+1).collect::<Val>();
                        return l.iter().map(|x| x.index_at_depth(env, &slice)).collect::<Val>()
                    }
                    _ => return Val::Bind{ f: Rc::new(Val::Right), b: Rc::new(NAN) }
                }
            }
            value = value.indexval(env, &i);
        }
        value
    }

    pub fn iter<'a: 'v, 'v>(&'v self, env: &'v mut Env<'a>) -> ValueIter<'a, 'v> {
        ValueIter {
            i: 0, value: self, env
        }
    }

    // create a finite iterator of `&Val`s. this returns None for functions, so it will never be 
    // infinite AND it doesnt need a &mut env which is more convenient most of the time
    pub fn iterf<'v>(&'v self) -> Option<Box<dyn Iterf<'v> + 'v>> {
        match self {
            Lis{l, ..} => Some(Box::new(l.iter())),
            Num(_) => Some(Box::new(std::iter::once(self))),
            _ => None
        }
    }
}

pub trait Iterf<'v>: Iterator<Item=&'v Val> + ExactSizeIterator + DoubleEndedIterator {}
impl<'v> Iterf<'v> for std::iter::Once<&'v Val> {}
impl<'v> Iterf<'v> for std::slice::Iter<'v, Val> {}

pub struct ValueIter<'a: 'v, 'v> {
    i: usize,
    value: &'v Val,
    env: &'v mut Env<'a>,
}

impl<'a, 'v> Iterator for ValueIter<'a, 'v> {
    type Item = Val;

    fn next(&mut self) -> Option<Self::Item> {
        let val = match self.value {
            Num(n) => if self.i == 0 {Some(Num(*n))} else {None},
            Lis{l, ..} => l.get(self.i).cloned(),
            f => Some(f.monad(self.env, &Num(self.i as f64))),
        };
        self.i += 1; 
        val
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.value {
            Num(_) => (1, Some(1)),
            Lis{ l, .. } => (l.len(), Some(l.len())),
            _ => (usize::MAX, None)
        }
    }
}

impl FromIterator<Val> for Val {
    fn from_iter<T: IntoIterator<Item = Val>>(iter: T) -> Self {
        Lis{l: Rc::new(iter.into_iter().collect()), fill: Rc::new(NAN)}
    }
}

pub fn iota(prefix: Vec<isize>, arg: &[isize]) -> Val {
    if arg.is_empty() {
        return prefix.into_iter().map(|x| Num(x as f64)).collect()
    }
    let iter: Box<dyn Iterator<Item=isize>> = if arg[0] > 0 {
        Box::new(0..arg[0])
    } else {
        Box::new((0..arg[0].abs()).rev())
    };
    let lis = iter.map(|i| iota([&prefix[..], &[i]].concat(), &arg[1..])).collect();
    Lis{l: Rc::new(lis), fill: Rc::new(NAN)}
}


pub fn iota_scalar(arg: isize) -> Val {
    let iter: Box<dyn Iterator<Item=isize>> = if arg > 0 {
        Box::new(0..arg)
    } else {
        Box::new((0..arg.abs()).rev())
    };
    let lis = iter.map(|i| Num(i as f64)).collect();
    Lis{l: Rc::new(lis), fill: Rc::new(NAN)}
}

pub fn ravel<'a>(arg: &'a Val, list: &mut Vec<&'a Val>) {
    match arg {
        Lis{ l, .. } => for i in l.iter() {ravel(i, list)},
        _ => list.push(arg)
    }
}

pub fn concat(a: &Val, b: &Val) -> Val {
    if !(a.is_finite() && b.is_finite()) { return NAN }
    a.iterf().unwrap().chain(b.iterf().unwrap()).cloned().collect()
}

pub fn reverse(a: &Val) -> Val {
    if !a.is_finite() { return NAN }
    a.iterf().unwrap().rev().cloned().collect()
}

pub fn reshape_iter(a: &mut ValueIter, b: &[usize], fill: &Val) -> Val {
    if b.is_empty() {return a.next().unwrap_or_else(|| fill.clone())}
    let (pre, suf) = (b[0], &b[1..]);
    (0..pre).map(move |_| reshape_iter(a, suf, fill)).collect()
}

pub fn takeleft(env: &mut Env, a: &Val, b: &Val) -> Val {
    let Some(shape) = getshape(a, b) else {return NAN};
    let mut iter = a.iter(env);
    reshape_iter(&mut iter, &shape[..], &a.fill())
}

pub fn takeright(env: &mut Env, a: &Val, b: &Val) -> Val {
    let Some(shape) = getshape(a, b) else {return NAN};
    let elems = shape.iter().product::<usize>();
    let bee = if a.len() < elems {
        std::iter::repeat(a.fill()).take(elems - a.len()).chain(a.iter(env)).collect::<Val>()
    } else {
        a.iter(env).skip(a.len() - elems).collect::<Val>()
    };
    let mut iter = bee.iter(env);
    reshape_iter(&mut iter, &shape[..], &a.fill())
} 

pub fn getshape(a: &Val, b: &Val) -> Option<Vec<usize>> {
    let mut shape = Vec::new();
    let mut spot = None::<usize>;
    let Some(iter) = b.iterf() else {return None};
    for i in iter { match i {
        Num(n) if n.is_nan() && spot.is_none() => {
            spot = Some(shape.len());
            shape.push(1);
        },
        Num(n) => shape.push(*n as usize),
        _ => return None
    }};
    if let Some(index) = spot {
        fn divceil(x: usize, y: usize) -> usize {x / y + usize::from(x % y != 0)}
        shape[index] = divceil(match a {
            Val::DCycle(c) => c.len(), e => e.len()
        }, shape.iter().product());
    }
    Some(shape)
}

pub fn dropleft(a: &Val, b: f64) -> Val {
    if b < 0. {return dropright(a, -b);}
    if let Some(iter) = a.iterf() {
        iter.skip(b as usize).cloned().collect()
    } else {NAN}
}

pub fn dropright(a: &Val, b: f64) -> Val {
    if b < 0. {return dropleft(a, -b);}
    if let Some(iter) = a.iterf() {
        iter.rev().skip(b as usize).rev().cloned().collect()
    } else {NAN}
}