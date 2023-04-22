use std::{cell::{RefCell, RefMut}, collections::VecDeque};

use vemf::{self, Env, codepage, Val, bx};
use wasm_bindgen::prelude::*;

struct Output<'io> {
    bufref: &'io RefCell<Vec<u8>>,
    inputs: Box<[VecDeque<u8>]>,
}
impl<'io> vemf::Interface<'io> for Output<'io> {
    fn read(&mut self, stm: usize, size: isize) -> Option<Vec<u8>> {
        self.inputs.get_mut(stm).map(|inp|
            inp.drain(..(size as usize).min(inp.len())).collect()
        )
    }
    fn read_line(&mut self, stm: usize) -> Option<Vec<u8>> {
        self.inputs.get_mut(stm).map(|inp| {
            let a = inp.iter().position(|x| *x == b'\n').unwrap_or(inp.len());
            inp.drain(0..(a+1)).collect()
        })
    }
    fn read_to_end(&mut self, stm: usize) -> Option<Vec<u8>> {
        self.inputs.get_mut(stm).map(|inp| {
            inp.drain(..).collect()
        })
    }
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

fn fmtstring(format: &str) -> Vec<Val> {
    format.chars()
        .filter_map(|x| x.is_ascii_digit().then_some(Val::Int(x as i64 - 0x30)))
        .collect::<Vec<_>>()
}

#[allow(dead_code)]
#[wasm_bindgen]
pub struct EvaluateRes {
    output: String,
    pub error: Option<i32>,
}
#[wasm_bindgen]
impl EvaluateRes {
    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }
}

#[wasm_bindgen]
#[allow(clippy::boxed_local)]
pub fn evaluate(s: &str, fmt: &str, args: Box<[JsValue]>, inputs: Box<[JsValue]>) -> EvaluateRes {
    let outbuf = RefCell::new(Vec::new());
    let mut env = Env::new();
    env.rng = bx(rand::thread_rng());
    env.interface = bx(Output {
        bufref: &outbuf,
        inputs: inputs.iter()
            .map(|x| VecDeque::from(x.as_string().unwrap_or(String::new()).into_bytes()))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });
    env.include_stdlib();
    env.include_args(&args.iter().map(|x| x.as_string().unwrap_or(String::new())).collect::<Vec<_>>());
    let error = env.run_string(s, &fmtstring(fmt));
    env.interface = bx(vemf::NoIO);
    let borrow = outbuf.borrow();

    EvaluateRes{ output: String::from_utf8_lossy(&borrow).into_owned(), error: error.err() }
}

#[wasm_bindgen]
pub fn escape1c(a: char) -> Option<char> {
    Some(codepage::tochar(vemf::escape_1c(codepage::tobyte(a)?)?))
}

#[wasm_bindgen]
pub fn escape2c(a: char, b: char) -> Option<char> {
    Some(codepage::tochar(vemf::escape_2c([codepage::tobyte(a)?, codepage::tobyte(b)?])?))
}

#[wasm_bindgen]
pub fn tobytes(a: &str) -> Box<[u8]> {
    codepage::tobytes(a).into_boxed_slice()
}

#[wasm_bindgen]
pub fn tochars(a: &[u8]) -> String {
    codepage::tochars_ln(a)
}

#[wasm_bindgen]
pub fn rewrite(s: &str) -> String {
    vemf::rewrite(&codepage::tobytes(s))
}