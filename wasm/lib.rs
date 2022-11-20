use std::cell::{RefCell, RefMut};

use vemf::{self, Env, codepage, Val};
use wasm_bindgen::prelude::*;

struct Output<'io> {
    bufref: &'io RefCell<Vec<u8>>
}
impl<'io> vemf::Interface<'io> for Output<'io> {
    fn read(&mut self, _: usize, _: &mut [u8]) -> Option<usize> { None }
    fn write(&mut self, stm: usize, slice: &[u8]) -> Option<usize> {
        if stm == 0 || stm == 1 {
            let mut borrow = self.bufref.borrow_mut();
            borrow.extend(slice);
            Some(slice.len())
        } else { None }
    }
}

struct Handle<'io>(RefMut<'io, Vec<u8>>);
use std::io::{Result as IOResult, IoSlice};

impl<'io> std::io::Write for Handle<'io> {
    fn write(&mut self, buf: &[u8]) -> IOResult<usize> { self.0.write(buf) }
    fn write_vectored(&mut self, bufs: &[IoSlice<'_>]) -> IOResult<usize> { self.0.write_vectored(bufs) }
    fn write_all(&mut self, buf: &[u8]) -> IOResult<()> { self.0.write_all(buf) }
    fn flush(&mut self) -> IOResult<()> { self.0.flush() }
}

#[wasm_bindgen]
pub fn evaluate(s: &str, fmt: &str) -> String {
    let outbuf = RefCell::new(Vec::new());
    let mut env = Env::new(Box::new(rand::thread_rng()));
    env.interface = Box::new(Output {bufref: &outbuf});
    env.include_stdlib();
    let out = env.include_string(s).format(
        &fmt.chars()
        .filter_map(|x| x.is_ascii_digit().then_some(Val::Int(x as i64 - 0x30)))
        .collect::<Vec<_>>()[..]);
    env.interface = Box::new(vemf::NoIO);
    let borrow = outbuf.borrow();
    String::from_utf8_lossy(&borrow).into_owned() + &out
}

#[wasm_bindgen]
pub fn rewrite(s: &str) -> String {
    codepage::tochars_ln(&vemf::rewrite(&codepage::tobytes(s).unwrap()))
}