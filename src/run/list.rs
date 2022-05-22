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

pub fn each(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let Some(b) = b {
        let len = match (a.is_finite(), b.is_finite()) {
            (true, true) => usize::max(a.len(), b.len()),
            (false, true) => b.len(),
            (true, false) => a.len(),
            (false, false) => return Val::Trn3 {
                a: Rc::new(a.clone()), f: Rc::clone(g), b: Rc::new(b.clone())
            }
        };
        let mut values = Vec::with_capacity(len);
        for n in 0..len {
            let l = a.index(env, n); let r = b.index(env, n);
            values.push(g.dyad(env, &l, &r));
        }
        let fill = match (a, b) {
            (Lis { fill: fa, .. }, Lis { fill: fb, .. }) => g.dyad(env, fa, fb),
            _ => NAN,
        };
        Lis{l: Rc::new(values), fill: Rc::new(fill)}
    } else {
        if !a.is_finite() { return Val::Trn2 {
            a: Rc::new(a.clone()), f: Rc::clone(g)
        }}
        let mut values = Vec::with_capacity(a.len());
        for n in 0..a.len() {
            let x = a.index(env, n);
            values.push(g.monad(env, &x));
        }
        let fill = match a { Lis { fill, .. } => g.monad(env, fill), _ => NAN };
        Lis{l: Rc::new(values), fill: Rc::new(fill)}
    }
}

pub fn scal(env: &mut Env, a: &Val, b: Option<&Val>, g: &Rc<Val>) -> Val {
    if let Some(b) = b {
        match (a, b) {
            (Num(_), Num(_)) => g.dyad(env, a, b), // 1+2
            (Num(_)|Lis{..}, Num(_)|Lis{..}) => Lis {  // 123+4, 123+456
                l: Rc::new((0..usize::max(a.len(), b.len())).map(|n| {
                    let l = a.index(env, n); let r = b.index(env, n); scal(env, &l, Some(&r), g)
                }).collect()),
                fill: Rc::new(scal(env, &a.fill(), Some(&b.fill()), g))
            },
            (Lis{..}, _) => Lis {  // 123+Σ
                l: Rc::new((0..a.len()).map(|n| {
                    let l = a.index(env, n); let r = b.index(env, n); scal(env, &l, Some(&r), g)
                }).collect()),
                fill: Rc::new(a.fill()),
            },
            (_, Lis{..}) => Lis {  // Σ+123
                l: Rc::new((0..b.len()).map(|n| {
                    let l = a.index(env, n); let r = b.index(env, n); scal(env, &l, Some(&r), g)
                }).collect()),
                fill: Rc::new(b.fill()),
            },
            (_, _) => Val::Fork {  // 1+Σ | Σ+1 | Σ+Ω
                a: Rc::new(a.clone()), f: Rc::new(Val::DScalar(Rc::clone(g))), b: Rc::new(b.clone())
            },
        }
    } else {
        match a {
            Num(_) => g.monad(env, a),
            Lis{..} => Lis {
                l: Rc::new((0..a.len()).map(|n| {
                    let l = a.index(env, n); scal(env, &l, None, g)
                }).collect()),
                fill: Rc::new(a.fill()),
            },
            _ => Val::Trn2 {
                a: Rc::new(a.clone()), f: Rc::new(Val::DScalar(Rc::clone(g)))
            }
        }
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