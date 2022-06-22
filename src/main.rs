#![feature(let_else)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::map_unwrap_or)]
#![warn(clippy::semicolon_if_nothing_returned)]

use std::path::PathBuf;
use clap::Parser;
use crate::run::Val;

#[macro_use] mod codepage;
mod token; mod parse; mod run; mod test;

pub type Bstr = smallvec::SmallVec<[u8; 16]>; // length will be the same as a Vec in 64bit archs

#[derive(Parser)]
#[clap(dont_collapse_args_in_usage = true)]
struct Args {
    /// filename of the file that will be read.
    /// if not given, opens up a REPL.
    #[clap(index = 1)]
    filename: Option<PathBuf>,

    /// arguments given to the script. sets α, β and δ accordingly
    #[clap(index = 2)]
    arguments: Vec<String>,

    /// whether to include the standard library
    #[clap(short = 's', long)]
    no_stdlib: bool,

    /// how to format the output. use like dyadic ⁿ. ignored in repl mode.
    #[clap(short, long, default_value = "0")]
    formatting: String,
}

fn main() {
    let args = Args::parse();
    let mut state = run::Env::new();
    if !args.no_stdlib {
        state.include_stdlib();
    }
    //println!("sizeof(Val) = {}", std::mem::size_of::<run::Val>());
    if let Some(path) = args.filename {
        let arguments = state.include_args(&args.arguments);
        let mut res = state.include_file(&mut std::fs::File::open(path).unwrap()).unwrap();
        if !res.is_finite() {
            res = res.call(
                &mut state,
                arguments.get(0).cloned().unwrap_or(run::NAN),
                arguments.get(1).cloned()
            );
        }
        println!("{}", res.format(
            &args.formatting.chars()
            .filter_map(|x| x.is_ascii_digit().then(|| Val::Int(x as i64 - 0x30) ))
            .collect::<Vec<_>>()[..]));
    } else {
        println!("welcome to vemf repl. enjoy your stay");
        loop {
            print!("    ");
            let _ = std::io::Write::flush(&mut std::io::stdout());
            let mut code = String::new();
            std::io::stdin().read_line(&mut code).expect("error while reading from stdin");
            if code.trim_start().starts_with(')') {
                if let Some((l, r)) = code[1..].split_once(' ') {
                    let val = state.include_string(r);
                    state.locals.insert(Bstr::from(&b"__"[..]), val);
                    state.include_string(&format!("__ⁿ({})☻", l));
                } else {
                    state.include_string(&format!("__ⁿ({})☻", &code[1..]));
                }
                continue;
            }
            let val = state.include_string(&code);
            if !val.is_nan() { println!("{}", val); }
            state.locals.insert(Bstr::from(&b"__"[..]), val);
        }
    }
}
