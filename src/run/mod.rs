#![allow(dead_code)]

mod intrinsics;

use crate::parse::{Fe, Ve};
use intrinsics::Intr;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug)]
pub struct Env {
    globals: HashMap<Vec<u8>, Val>
}

#[derive(Clone, Debug)]
pub enum Val {
    Num(f64),
    Lis{i: Rc<[Val]>, fill: Box<Val>},
    FIn(intrinsics::Intr),  // intrinsic function
    FSet(Vec<u8>),
}
use Val::{Num, Lis};
impl Default for Val {
    fn default() -> Self { Self::Num(f64::NAN) }
}

const GLOBALS: &[(u8, Val)] = &[
    (b'a', Num(12.)),     (b'b', Num(20.)),     (b'c', Num(99.)),     (b'd', Num(100.)),    (b'e', Num(999.)),
    (b'f', Num(1000.)),   (b'g', Num(9999.)),   (b'h', Num(10000.)),  (b'i', Num(100000.)), (b'j', Num(1000000.)),
    (b'k', Num(15.)),     (b'l', Num(16.)),     (b'm', Num(31.)),     (b'n', Num(32.)),     (b'o', Num(63.)),
    (b'p', Num(64.)),     (b'q', Num(127.)),    (b'r', Num(128.)),    (b's', Num(255.)),    (b't', Num(256.)),
    (b'u', Num(512.)),    (b'v', Num(1024.)),   (b'w', Num(2048.)),   (b'x', Num(4096.)),   (b'y', Num(32768.)),
    (b'z', Num(65536.)),
    (b'+', Val::FIn(Intr::Add))
];

impl Env {
    
    pub fn new() -> Env {
        let mut globals = HashMap::with_capacity(GLOBALS.len());
        for (b, v) in GLOBALS { globals.insert(vec![*b], v.clone()); }
        Env { globals }
    }

    pub fn evaluate(&mut self, expr: &Ve) -> Val {
        match expr {
            Ve::Var(s) => self.globals.get(&s[..]).cloned().unwrap_or_default(),
            Ve::Num(n) => Val::Num(*n),
            Ve::Snd(l) => Val::Lis{
                i: Rc::from(l.iter().map(|x| self.evaluate(x)).collect::<Vec<_>>()),
                fill: Default::default()
            },
            Ve::Nom(f) => self.evaluate_fe(f),
            Ve::Afn1{ a, f } => {
                let a = self.evaluate(a);
                let v = self.evaluate_fe(f);
                match v {
                    Num(_) | Lis { .. } => v,
                    Val::FIn(i) => i.monad(self, &a),
                    Val::FSet(name) => {
                        self.globals.insert(name, a.clone());
                        a
                    },
                }
            },
            Ve::Afn2{ a, f, b } => {
                let a = self.evaluate(a);
                let v = self.evaluate_fe(f);
                let b = self.evaluate(b);
                match v {
                    Num(_) | Lis { .. } => v,
                    Val::FIn(i) => i.dyad(self, &a, &b),
                    Val::FSet(name) => {
                        self.globals.insert(name, a.clone());
                        a
                    },
                }
            },
        }
    }
    pub fn evaluate_fe(&self, expr: &Fe) -> Val {
        match expr {
            Fe::Var(s) => self.globals.get(&s[..]).cloned().unwrap_or_default(),
            Fe::SetVar(v) => Val::FSet(v.clone()),
            Fe::Aav1 { .. } => todo!(),
            Fe::Aav2 { .. } => todo!(),
            Fe::Bind { .. } => todo!(),
            Fe::Trn1 { .. } => todo!(),
            Fe::Trn2 { .. } => todo!(),
            Fe::Dfn(_) => todo!(),
        }
    }
}