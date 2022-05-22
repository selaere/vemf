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
}

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