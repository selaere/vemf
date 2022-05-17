use crate::run::{Val, Env};


#[derive(Clone, Debug)]
pub enum Intr {
    Add
}

impl Intr {

pub fn monad(&self, _: &Env, _: &Val) -> Val {
    todo!()
}

pub fn dyad(&self, _state: &Env, a: &Val, b: &Val) -> Val { match self {
    Self::Add => match a {
        Val::Num(a) => match b {
            Val::Num(b) => Val::Num(a + b),
            _ => todo!()
        },
        _ => todo!()
    }
}}

}