#[macro_use] mod func; mod list; mod adverb; mod disp; mod val; pub mod io;

use crate::parse::{Expr, Stmt};
use crate::prelude::*;
use adverb::AvT;

const STDLIB: &str = include_str!("../std.vemf");

pub use num_complex::Complex64 as c64;

pub const NAN: Val = Num(c64::new(f64::NAN, f64::NAN));

pub type Frame = HashMap<Bstr, Val>;

/// vemf interpreter state
pub struct Env<'io> {
    pub stack: Vec<Frame>,
    pub rng: Box<dyn rand::RngCore>,
    pub interface: Box<dyn io::Interface<'io> + 'io>,
}

#[macro_export]
macro_rules! or_nan { ($expr:expr) => {
    match $expr {
        Some(x) => x,
        None => return NAN,
    }
} }

pub type Func = fn(env: &mut Env, a: Val, b: Option<Val>) -> Val;

/// represents a vemf value
#[derive(Clone)]
pub enum Val {
    Num(c64),
    Int(i64),
    Lis { l: Rc<Vec<Val>>, fill: Rc<Val> },
    FSet(Bstr), FCng(Bstr),
    Dfn { loc: Rc<HashMap<Bstr, Val>>, s: Rc<[Stmt]> },
    Fork(Rc<Val>, Rc<Val>, Rc<Val>),
    Av(AvT, Option<Rc<Val>>, Rc<Val>),
    AvBuilder(AvT),
    Err(i32), // exit code
    Func(Func)
}

use Val::{Lis, Num, Int};
impl Default for Val {
    fn default() -> Self { NAN }
}


impl<'io> Env<'io> {
    
    pub fn new<'a>(rng: Box<dyn rand::RngCore>) -> Env<'a> {
        Env::from_frame(HashMap::new(), rng)
    }

    pub fn from_frame<'a>(frame: Frame, rng: Box<dyn rand::RngCore>) -> Env<'a> {
        Env { stack: vec![frame], interface: bx(io::NoIO), rng }
    }

    pub fn locals(&self) -> &Frame { self.stack.last().unwrap() }
    pub fn locals_mut(&mut self) -> &mut Frame { self.stack.last_mut().unwrap() }

    pub fn set_local(&mut self, name: Bstr, value: Val) {
        self.locals_mut().insert(name, value);
    }

    pub fn get_var_cap(&self, mut name: &[u8]) -> Option<Val> {
        if let Some(b!('[')) = name.first() {name = &name[1..]}
        let mut skipped = 0;
        loop {
            for frame in self.stack.iter().rev().skip(skipped) {
                if let Some(var) = frame.get(name) {
                    return Some(var.c())
                }
            }
            if let Some(b!('[')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return None }
        }
    }

    pub fn get_var(&self, mut name: &[u8]) -> Option<Val> {
        let mut skipped = 0;
        loop {
            for frame in self.stack.iter().rev().skip(skipped) {
                if let Some(var) = frame.get(name) {
                    return Some(var.c())
                }
            }
            if let Some(b!(']')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return None }
        }
    }

    pub fn mutate_var(&mut self, mut name: &[u8], func: Val, b: Option<Val>) -> Option<Val> {
        let mut skipped = 0;
        loop {
            for (fmn, frame) in self.stack.iter_mut().enumerate().rev().skip(skipped) {
                if let Some((nam, val)) = frame.remove_entry(name) {
                    let val = func.call(self, val, b);
                    self.stack[fmn].insert(nam, val.c());
                    return Some(val);
                }
            }
            if let Some(b!(']')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return None }
        }
    }

    pub fn delete_var(&mut self, mut name: &[u8]) {
        let mut skipped = 0;
        loop {
            for frame in self.stack.iter_mut().rev().skip(skipped) {
                if frame.remove_entry(name).is_some() { return; }
            }
            if let Some(b!(']')) = name.first() {
                name = &name[1..];
                skipped += 1;
            } else { return }
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Val {
        macro_rules! eval { ($v:expr) => {
            match self.eval($v) {
                Val::Err(x) => return Val::Err(x),
                x => x,
            }
        }}
        match expr {
            Expr::Var(s) => self.get_var(s).unwrap_or_default(),
            Expr::Int(n) => Int(*n),
            Expr::Flt(n) => Num(*n),
            Expr::Snd(l) => {    
                let mut v = Vec::with_capacity(l.len());
                for x in l { v.push(eval!(x)); }
                Lis { l: Rc::from(v), fill: NAN.rc() }
            },
            Expr::Afn1(a, f) => {
                let a = eval!(a); let f = eval!(f);
                f.monad(self, a)
            },
            Expr::Afn2(a, f, b) => {
                let a = eval!(a); let f = eval!(f); 
                if let Val::FSet(_) = f { f.monad(self, a); return eval!(b) }
                let b = eval!(b);
                f.dyad(self, a, b)
            },
            Expr::SetVar(v) => Val::FSet(v.c()),
            Expr::MutVar(v) => Val::FCng(v.c()),
            Expr::Aav1(v, g) => {
                let g = eval!(&g.c());
                self.get_var(&v[..]).unwrap_or_default().monad(self, g)
            }
            Expr::Aav2(f, v, g) => {
                let f = eval!(&f.c()); let g = eval!(&g.c());
                self.get_var(&v[..]).unwrap_or_default().dyad(self, g, f)
            },
            Expr::Bind(f, b) => {
                let f = eval!(&f.c()); let b = eval!(&b.c());
                Val::bind(f.rc(), b.rc())
            },
            Expr::Trn2(a, f) => {
                let a = eval!(&a.c()); let f = eval!(&f.c());
                Val::atop(a.rc(), f.rc())
            },
            Expr::Trn3(a, f, b) => {
                let a = eval!(&a.c()); let f = eval!(&f.c()); let b = eval!(&b.c());
                Val::atop(a.rc(), Val::bind(f.rc(), b.rc()).rc())
            },
            Expr::Fork(a, f, b) => {
                let a = eval!(&a.c()); let f = eval!(&f.c()); let b = eval!(&b.c());
                Val::Fork(a.rc(), f.rc(), b.rc())
            },
            Expr::Dfn { s, cap } => {
                let mut locals = HashMap::with_capacity(cap.len());
                for var in cap {
                    self.get_var_cap(var).and_then(|x| locals.insert(var.c(), x));
                }
                Val::Dfn{s: Rc::from(&s[..]), loc: Rc::new(locals)}
            },
            Expr::Block(s) => self.eval_block(s)
        }
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Option<Val> {
        macro_rules! eval { ($v:expr) => {
            match self.eval($v) {
                Val::Err(x) => return Some(Val::Err(x)),
                x => x,
            }
        }}
        match stmt {
            Stmt::Discard(expr) => { _ = eval!(expr); },
            Stmt::Loc(a, v) => { let a = eval!(a); self.set_local(v.c(), a); },
            Stmt::Mut(a, v) => { let a = eval!(a); self.mutate_var(v, a, None); },
            Stmt::DelLoc(v) => { self.locals_mut().remove(v); },
            Stmt::DelMut(v) => { self.delete_var(v); },
            Stmt::Return(expr) => { return Some(eval!(expr)); },
            Stmt::Cond(cond, then) => {
                let val = eval!(cond);
                let cond = val.is_scalar() && val.as_bool() || {
                    let a = self.locals().get(&[b!('α')][..]).cloned().unwrap_or(NAN);
                    let b = self.locals().get(&[b!('Σ')][..])
                        .map_or(false, |x| x.try_int() != Some(1))
                        .then(|| self.locals().get(&[b!('β')][..]).cloned().unwrap_or(NAN) );
                    val.call(self, a, b).as_bool()
                };
                if cond { return self.eval_stmt(then) }
            }
        };
        None
    }

    pub fn eval_block(&mut self, block: &[Stmt]) -> Val {
        for stmt in block.iter() { 
            if let Some(val) = self.eval_stmt(stmt) { return val };
        }
        NAN
    }
    pub fn include_string(&mut self, code: &str) -> Val {
        self.include_bytes(&crate::codepage::tobytes(code).unwrap()[..])
    }
    pub fn include_bytes(&mut self, code: &[u8]) -> Val {
        use crate::{token, parse};
        let tokens = token::tokenize(code);
        //println!("{:?}", tokens);
        let parsed = parse::parse(&tokens);
        //for i in &parsed { println!("parsed: {i}"); }
        self.eval_block(&parsed)
    }

    pub fn include_stdlib(&mut self) {
        func::load_intrinsics(self);
        self.include_string(STDLIB);
    }

    #[cfg(any(feature = "std", test))]
    pub fn include_file<F: std::io::Read>(&mut self, file: &mut F) -> std::io::Result<Val> {
        let mut code = String::new();
        file.read_to_string(&mut code)?;
        Ok(self.include_string(&code))
    }

    pub fn run_string(&mut self, code: &str, format: &[Val]) -> Result<(), i32> {
        let mut res = self.include_string(code);
        if let Val::Err(x) = res { return Err(x); }
        if res.is_infinite() { res = res.call(
            self,
            self.get_var(&[b!('α')]).unwrap_or(NAN),
            self.get_var(&[b!('β')]),
        ); }
        if let Val::Err(x) = res { return Err(x); }
        res.format(&mut io::FromInterface(&mut *self.interface), format).unwrap();
        Ok(())
    }

    pub fn include_args(&mut self, args: &[String]) {
        let args: Vec<Val> = args.iter().map(|s| s.chars().map(|x| Int(x as i64)).collect()).collect();
        if let Some(x) = args.get(0) { self.set_local(bstr![b!('α')], x.c()); }
        if let Some(x) = args.get(1) { self.set_local(bstr![b!('β')], x.c()); }
        self.set_local(bstr![b!('Σ')], Int(args.len() as _));
        self.set_local(bstr![b!('δ')], Val::lis(args.c()));
    }

}