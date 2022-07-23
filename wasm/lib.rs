use vemf::{self, Env, codepage, Val};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn evaluate(s: &str, fmt: &str) -> String {
    let mut env = Env::new();
    env.output.push(Box::new(Vec::new()));
    env.include_stdlib();
    let out = env.include_string(s).format(
        &fmt.chars()
        .filter_map(|x| x.is_ascii_digit().then_some(Val::Int(x as i64 - 0x30)))
        .collect::<Vec<_>>()[..]);
    (if let Some(buf) = env.output.first().and_then(|x| x.as_any().downcast_ref::<Vec<_>>()) {
        String::from_utf8_lossy(buf).into_owned()
    } else {
        String::from("error retrieving output")
    }) + &out
}

#[wasm_bindgen]
pub fn rewrite(s: &str) -> String {
    codepage::tochars_ln(&vemf::rewrite(&codepage::tobytes(s).unwrap()))
}