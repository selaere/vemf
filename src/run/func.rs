use crate::prelude::*;
use super::{Val::{self, Num, Int}, NAN, adverb, c64, val::complexcmp, list};

#[macro_export]
macro_rules! func {
    (@$env:tt, $a:ident :$name:ident $b:ident => $contents:expr) => {
        pub fn $name($env: &mut $crate::Env, $a: Val, ba: Option<Val>) -> Val { 
            let $b = ba.unwrap_or(NAN); $contents }
    };
    (@$env:tt, $a:ident :$name:ident $b:ident? => $contents:expr) => {
        pub fn $name($env: &mut $crate::Env, $a: Val, $b: Option<Val>) -> Val { $contents }
    };
    (@$env:tt, $a:ident :$name:ident => $contents:expr) => {
        pub fn $name($env: &mut $crate::Env, $a: Val, _: Option<Val>) -> Val { $contents }
    };
    ($a:ident : $($x:tt)+) => { func!(@_, $a :$($x)+); };
}

macro_rules! intfunc {
    ($(@$env:tt,)? $a:ident :$name:ident => $contents:expr) => {
        func!($(@$env,)? a :$name => a.try_int().map_or(NAN, |$a| $contents));
    };
    ($(@$env:tt,)? $a:ident :$name:ident $b:ident => $contents:expr) => {
        func!($(@$env,)? a :$name b => a.try_int().zip(b.try_int()).map_or(NAN, |($a, $b)| $contents));
    };
}

pub fn load_intrinsics(env: &mut super::Env) {
    use list::*; use super::val::encode;
    macro_rules! load_func {($($name:ident,)*) => { $( {
        let mut name = Bstr::from(&b"in"[..]);
        name.extend(stringify!($name).bytes());
        env.set_local(name, Val::Func($name))
    } );* }}
    load_func!(
        add, sub, mul, div, dive, rem, pow, log, lt, gt, and, or, max, min, atan2, approx, band, bor, bxor, fact, gcd, lcm, binom, abs, neg, ln, exp, sin, asin, cos, acos, tan, atan, sqrt, round, ceil, floor, isnan, sign, bnot, brepr, complex, cis, real, imag, conj, arg,
        left, right, get, set, call, islist, eval,
        shape, len, index, iota, pair, enlist, ravel, concat, reverse, getfill, setfill, matches,
        print, println, output, input, fromutf8, toutf8, fromcp, tocp, exit, format, numfmt, parse,
        takeleft, takeright, dropleft, dropright, replist, pick, sample, replicate, find, uio,
        reverse, gradeup, gradedown, sortup, sortdown, binsup, binsdown, encode, group, occcount,
        domainto,
    );
    macro_rules! load_av {($($name:ident,)*) => { $( {
        let mut name = Bstr::from(&b"in"[..]);
        name.extend(stringify!($name).to_ascii_lowercase().bytes());
        env.set_local(name, Val::AvBuilder(adverb::$name))
    } );* }}
    load_av!(
        swap, constant, monadic, bind, atop,
        each, eachleft, conform, extend,
        scan, scanpairs, reduce, stencil, valences,
        overleft, overright, over, forkleft, forkright,
        until, untilscan, power, powerscan, untilcmp, untilscancmp,
        drill, amend, cycle,
    );
}

func!(@env, a :set b => {
    let name = a.iterf().filter_map(|x| x.try_int().map(|x| x as u8)).collect::<Bstr>();
    env.set_local(name, b.c()); b
});
func!(@env, a :get => {
    let name = a.iterf().filter_map(|x| x.try_int().map(|x| x as u8)).collect::<Bstr>();
    env.get_var(&name).unwrap_or(NAN)
});
func!(a :left => a);
func!(a :right b? => b.unwrap_or(a));
func!(a :islist => Val::bool(!a.is_scalar()));

func!(a :add b => match (a, b) {
    (Int(a), Int(b)) => Int(a.saturating_add(b)),
    (a, b) => Num(a.as_c() + b.as_c()),
});

func!(a :sub b => match (a, b) {
    (Int(a), Int(b)) => Int(a.saturating_sub(b)),
    (a, b) => Num(a.as_c() - b.as_c()),
});
func!(a :mul b => match (a, b) {
    (Int(a), Int(b)) => Int(a.saturating_mul(b)),
    (a, b) => Num(a.as_c() * b.as_c()),
});
func!(a :div b => if b.as_c().im == 0. {
    Num(a.as_c().unscale(b.as_c().re))
} else {
    Num(a.as_c().fdiv(b.as_c()))
});
func!(a :rem b => match (a, b) {
    (Int(a), Int(b)) => if b == 0 {NAN} else { Int(a.rem_euclid(b)) },
    (a, b) => {
        let (a, b) = (a.as_c(), b.as_c());
        let mut r = a % b;
        if r.re < 0.0 { r += b.re.abs(); }
        if r.im < 0.0 { r += b.im.abs(); }
    Num(r) },
});
intfunc!(a :dive b => if b == 0 {NAN} else { Int(a.div_euclid(b)) });
func!(a :pow b => match (a, b) {
    (Int(a), Int(b)) if b >= 0 => Int(a.saturating_pow(b as u32)),
    (a, Int(b @ -0x80000000..=0x7FFFFFFF)) => Num(a.as_c().powi(b as i32)),
    (a, b) => Num(a.as_c().powc(b.as_c())),
});
func!(a :log b => Num(a.as_c().log(b.as_c().norm())));
func!(a :lt b => match (a, b) {
    (Int(a), Int(b)) => Val::bool(a < b),
    (a, b) => a.try_c().zip(b.try_c()).map_or(NAN, |(a, b)| Val::bool(complexcmp(a, b).is_lt()))
});
func!(a :gt b => match (a, b) {
    (Int(a), Int(b)) => Val::bool(a > b),
    (a, b) => a.try_c().zip(b.try_c()).map_or(NAN, |(a, b)| Val::bool(complexcmp(a, b).is_gt()))
});
func!(a :and b => Val::bool(a.as_bool() && b.as_bool()));
func!(a :or  b => Val::bool(a.as_bool() || b.as_bool()));
func!(a :max b => match (a, b) {
    (Int(a), Int(b)) => Int(a.max(b)),
    (a, b) => if complexcmp(a.as_c(), b.as_c()).is_gt() {a} else {b}
});
func!(a :min b => match (a, b) {
    (Int(a), Int(b)) => Int(a.min(b)),
    (a, b) => if complexcmp(a.as_c(), b.as_c()).is_lt() {a} else {b}
});
func!(a :atan2 b => {
    let (y, x) = (a.as_c(), b.as_c());
    Val::flt(f64::atan2(y.re + x.im, x.re - y.im))
});
func!(a :approx b => Val::bool(Val::approx(&a, &b)));
func!(a :isnan    => Val::bool(a.is_nan()));
intfunc!(a :gcd b    => Int(num_integer::gcd(a, b)));
intfunc!(a :lcm b    => Int(num_integer::lcm(a, b)));
intfunc!(a :binom b  => Int(num_integer::binomial(a, b)));
intfunc!(a :band b   => Int(a & b));
intfunc!(a :bor b    => Int(a | b));
intfunc!(a :bxor b   => Int(a ^ b));
intfunc!(a :bnot     => Int(!a));
intfunc!(a :brepr    => Val::lis_fill(
    a.to_be_bytes()
        .into_iter()
        .flat_map(|x| (0..8).rev().map(move |y| Val::bool(x & (1 << y) != 0)))
        .collect(),
    Int(0)
));
func!(a :abs   => match a { Int(a) => Int(a.abs()), Num(x) => Val::flt(x.norm()), _ => NAN });
func!(a :neg   => match a { Int(a) => Int(-a),      Num(a) => Num(-a),            _ => NAN });
func!(a :ln    => Num(a.as_c().ln()  ));
func!(a :exp   => Num(a.as_c().exp() ));
func!(a :sin   => Num(a.as_c().sin() ));
func!(a :asin  => Num(a.as_c().asin()));
func!(a :cos   => Num(a.as_c().cos() ));
func!(a :acos  => Num(a.as_c().acos()));
func!(a :tan   => Num(a.as_c().tan() ));
func!(a :atan  => Num(a.as_c().atan()));
func!(a :sqrt  => Num(a.as_c().sqrt()));
func!(a :fact  => Val::flt(libm::tgamma(a.as_c().re + 1.)));
func!(a :round => match a { Int(a) => Int(a), Num(a) => Val::flt(a.re.round()), _ => NAN });
func!(a :ceil  => match a { Int(a) => Int(a), Num(a) => Val::flt(a.re.ceil()) , _ => NAN });
func!(a :floor => match a { Int(a) => Int(a), Num(a) => Val::flt(a.re.floor()), _ => NAN });
func!(a :sign  => match a { Int(a) => Int(a.signum()), Num(a) => {
    if a == c64::new(0., 0.) { Int(0) } 
    else if a.im == 0. { Int(a.re.signum() as i64) }
    else if a.re == 0. { Num(c64::new(0., a.im.signum()))}
    else { Num(c64::from_polar(1., a.arg())) }
}, _ => NAN });

func!(a :complex b => Num(c64::new(b.as_c().re, a.as_c().re)));
func!(a :cis b => Num(c64::from_polar(b.as_c().re, a.as_c().re)));
func!(a :real  => Val::flt(a.as_c().re));
func!(a :imag  => Val::flt(a.as_c().im));
func!(a :conj  => Num(a.as_c().conj()));
func!(a :arg   => Val::flt(a.as_c().arg()));

func!(a :matches b => Val::bool(a == b));

func!(@env, a :print b? => {
    _ = env.interface.write(0, a.display_string().as_bytes());
b.unwrap_or(a) });
func!(@env, a :println b? => {
    _ = env.interface.write(0, a.display_string().as_bytes());
    _ = env.interface.write(0, b"\n");
b.unwrap_or(a) });
func!(@env, a :output b => {
    let stm = or_nan!(b.try_int().and_then(|x| usize::try_from(x).ok()));
    let bytes = a.iterf()
        .flat_map(|x| x.try_int().map(|x| (x & 0xff) as u8))
        .collect::<Vec<_>>();
    env.interface.write(stm, &bytes).map_or(NAN, |x| Int(x as i64))
});
func!(@env, a :input b => {
    let chars = or_nan!(a.try_int().and_then(|x| isize::try_from(x).ok()));
    let stm =   or_nan!(b.try_int().and_then(|x| usize::try_from(x).ok()));
    let mut buf;
    let size = or_nan!(if chars < 0 { // read line
        buf = Vec::with_capacity(128); env.interface.read_line(stm, &mut buf)
    } else if chars == isize::MAX { // don't allocate an infinite buffer
        buf = Vec::with_capacity(128); env.interface.read_to_end(stm, &mut buf)
    } else {
        buf = vec![0; chars as usize]; env.interface.read(stm, &mut buf)
    });
    buf.into_iter().take(size).map(|i| Int(i64::from(i))).collect()
});
func!(a :fromutf8 => String::from_utf8_lossy(
    &a.iterf().flat_map(|x| x.try_int().map(|x| (x & 0xff) as u8)).collect::<Vec<_>>()
).chars().map(|x| Int(x as _)).collect());
func!(a :toutf8 => a.iterf()
    .flat_map(|x| x.try_int().map(|x| 
        x.try_into().ok().and_then(char::from_u32).unwrap_or('\u{FFFD}')))
    .collect::<String>()
    .bytes()
    .map(|x| Int(i64::from(x)))
    .collect());

func!(@env, a :exit => if let Some(n) = a.try_int() {
    Val::Err(n as i32)
} else {
    _ = env.interface.write(0, a.display_string().as_bytes());
    _ = env.interface.write(0, b"\n");
    Val::Err(1)
});
func!(a :format b? => {
    let mut buf = String::new(); a.format(&mut buf,
        &b.as_ref().map_or_else(Vec::new, |x| x.iterf().cloned().collect::<Vec<_>>())).unwrap();
    buf.chars().map(|x| Int(x as i64)).collect()
});
func!(a :numfmt => if !a.is_scalar() {NAN} else { 
    format!("{a}").chars().map(|x| Int(x as i64)).collect() });
func!(a :parse => if let Some(a @ 0x30..=0x39) = a.try_int() { Int(a - 0x30) } else {
    a.display_string().parse::<c64>().map(Num).unwrap_or(NAN)
});

func!(a :fromcp => { if a.is_nan() {return NAN}; a.try_int()
    .and_then(|x| u8::try_from(x).ok())
    .map_or(NAN, |x| Int(
        if x == b'\n' {'\n'} else {crate::codepage::tochar(x)}
    as i64))
});
func!(a :tocp => { if a.is_nan() {return NAN}; a.try_int()
    .and_then(|x| u32::try_from(x).ok())
    .and_then(char::from_u32)
    .and_then(crate::codepage::tobyte)
    .map_or(NAN, |x| Int(i64::from(x)))
});

intfunc!(@env, a :pick b => {
    use rand::distributions::{Distribution, Uniform, Standard};
    if a <= 0 { 
        Standard.sample_iter(&mut env.rng).take(b as _).map(Val::flt).collect()
    } else {
        Uniform::from(0..a as _).sample_iter(&mut env.rng)
            .take(b as usize)
            .map(|x| Int(i64::from(x))).collect::<Val>()
    }
});
intfunc!(@env, a :sample b => 
    rand::seq::index::sample(&mut env.rng, a as _, usize::min(a as _, b as _))
        .iter()
        .map(|x| Int(x as i64))
        .collect::<Val>()
);
func!(@env, a :call b => if b.is_scalar() {
    a.monad(env, b)
} else {
    let aa = b.index(env, 0);
    let bb = (b.len() > 1).then(|| b.index(env, 1));
    a.call(env, aa, bb)
});
func!(@env, a :eval => 
    a.iterf().map(|x| x.try_int().and_then(|x| x.try_into().ok()) )
    .collect::<Option<Vec<u8>>>()
    .map_or(NAN, |x| env.include_bytes(&x)));