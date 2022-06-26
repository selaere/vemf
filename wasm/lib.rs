use vemf::{self, Env, codepage, Val};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn evaluate(s: &str, fmt: &str) -> String {
    let mut env = Env::new();
    env.include_stdlib();
    env.include_string(s).format(
        &fmt.chars()
        .filter_map(|x| x.is_ascii_digit().then(|| Val::Int(x as i64 - 0x30) ))
        .collect::<Vec<_>>()[..])
}

#[wasm_bindgen]
pub fn rewrite(s: &str) -> String {
    codepage::tochars_ln(&vemf::rewrite(&codepage::tobytes(s).unwrap()))
}