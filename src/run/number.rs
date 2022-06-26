use super::Val::{self, Num, Int, Lis};
use super::{NAN, c64};

impl Val {
    
    pub const NAN: Val = NAN;
    
    pub fn bool(b: bool) -> Val { Int(i64::from(b)) }

    //pub fn is_list(&self) -> bool { matches!(self, Lis {..})}

    pub fn is_nan(&self) -> bool { match self { Num(n) => n.is_nan(), _ => false }}

    pub fn is_infinite(&self) -> bool { !matches!(self, Int(_) | Num(_) | Lis {..})}

    pub fn is_scalar(&self) -> bool { matches!(self, Int(_) | Num(_))}

    pub fn as_bool(&self) -> bool { match self {
        Int(n) => *n != 0,
        Num(n) => !n.is_nan() && *n != c64::new(0., 0.),
        _ => false,
    }}

    pub fn try_c(&self) -> Option<c64> { match self {
        Int(n) => Some(c64::new(*n as f64, 0.)),
        Num(n) => Some(*n),
        _ => None,
    }}

    pub fn try_int(&self) -> Option<i64> { match self {
        Int(n) => Some(*n),
        Num(n) => Some(n.re as i64),
        _ => None
    }}

    pub fn as_c(&self) -> c64 { self.try_c().unwrap_or(c64::new(f64::NAN, f64::NAN)) }

    pub fn flt(n: f64) -> Val { Num(c64::new(n, 0.)) }

    pub fn approx(&self, other: &Val) -> bool {
        const TOLERANCE: f64 = 0.00000000023283064365386963; // $2^{-32}$
        fn close(a: c64, b: c64) -> bool {
            let d = (a - b).norm();
            d <= TOLERANCE
            || d / a.norm() <= TOLERANCE
            || d / b.norm() <= TOLERANCE
        }
        match (self, other) {
            (Num(l), Num(r)) => close(*l, *r) || l.is_nan() && r.is_nan(),
            (Int(l), Int(r)) => l == r,
            (Num(l), Int(r)) => close(*l, c64::new(*r as f64, 0.)),
            (Int(l), Num(r)) => close(*r, c64::new(*l as f64, 0.)),
            (Lis { l: l_l, fill: l_fill }, Lis { l: r_l, fill: r_fill }) => 
                l_fill == r_fill
                && l_l.len() == r_l.len()
                && l_l.iter().zip(r_l.iter()).all(|(x, y)| Val::approx(x, y)),
            _ => false
        }
    }

    pub fn cmpval(&self, other: &Val) -> std::cmp::Ordering {
        use std::cmp::Ordering::{Greater, Less};
        match (self, other) {
            (Int(m), Int(n)) => m.cmp(n),
            (a, b) => match (a.try_c(), b.try_c()) {
                (Some(m), Some(n)) => complexcmp(m, n),
                (None,    Some(_)) => Greater,
                (Some(_), None) => Less,
                (None,    None) => Less,
            }
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Num(l), Num(r)) => l == r || l.is_nan() && r.is_nan(),
            (Int(l), Int(r)) => l == r,
            (Num(l), Int(r)) => l.im == 0. && l.re == *r as f64,
            (Int(l), Num(r)) => r.im == 0. && r.re == *l as f64,
            (Lis { l: l_l, fill: l_fill }, Lis { l: r_l, fill: r_fill }) => 
                l_fill == r_fill
                && l_l.len() == r_l.len()
                && l_l.iter().zip(r_l.iter()).all(|(x, y)| x == y),
            _ => false
        }
    }
}

pub fn complexcmp(a: c64, b: c64) -> std::cmp::Ordering {
    use std::cmp::Ordering::{Equal, Greater, Less};
    match (a.is_nan(), b.is_nan()) {
        (true, true) => Equal,
        (true, false) => Less,
        (false, true) => Greater,
        (false, false) => a.re.total_cmp(&b.re).then_with(|| a.im.total_cmp(&b.im))
    }
}


pub fn encode(a: Val, b: Val) -> Val {
    if b.is_infinite() { return NAN };
    match a {
        Int(n) => encode_int(n, b),
        Num(n) => encode_flt(n.re, b),
        _ => NAN,
    }
}

fn encode_int(mut a: i64, b: Val) -> Val{
    let mut list = vec![Int(0); b.len() + 1];
    for (n, i) in b.into_iterf().enumerate().rev() {
        let Some(i) = i.try_int() else { return NAN };
        if i == 0 { list[n+1] = Int(a); return Val::lis(list); }
        let m; (a, m) = (a.div_euclid(i), a % i);
        list[n+1] = Int(m);
        if a == 0 { return Val::lis(list); }
    }
    list[0] = Int(a);
    Val::lis(list)
}

fn encode_flt(mut a: f64, b: Val) -> Val {
    let mut list = vec![Val::flt(0.); b.len() + 1];
    for (n, i) in b.into_iterf().enumerate().rev() {
        let Some(c64{re: i, ..}) = i.try_c() else { return NAN };
        if i == 0. { list[n+1] = Val::flt(a); return Val::lis(list); }
        let m; (a, m) = (a.div_euclid(i), a.rem_euclid(i));
        list[n+1] = Val::flt(m);
        if a == 0. { return Val::lis(list); }
    }
    list[0] = Val::flt(a);
    Val::lis(list)
}