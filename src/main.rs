#![feature(let_else)]

#[macro_use] mod codepage;
mod test;
mod token;
mod parse;
mod run;

pub type Bstr = smallvec::SmallVec<[u8; 16]>; // length will be the same as a Vec in 64bit archs

fn main() {
    //println!("sizeof(Val) = {}", std::mem::size_of::<&run::Val>());
    if let Some(path) = std::env::args().nth(1) {
        let mut state = run::Env::new();
        state.include_stdlib();
        println!("{}", state.include_file(&mut std::fs::File::open(path).unwrap()).unwrap());
    } else {
        let mut state = run::Env::new();
        state.include_stdlib();
        loop {
            print!("vemf> ");
            let _ = std::io::Write::flush(&mut std::io::stdout());
            let mut code = String::new();
            std::io::stdin().read_line(&mut code).expect("error while reading from stdin");
            if code.trim_start().starts_with(')') {
                state.include_string(&format!("__ⁿ({})☻", &code[1..])); continue;
            }
            let val = state.include_string(&code);
            if code.trim_end().ends_with(['·', '☻']) { continue }
            println!("{}", val);
            state.locals.insert(Bstr::from(&b"__"[..]), val);
        }
    }
}
