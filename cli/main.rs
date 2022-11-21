use std::{path::PathBuf, io::{Read, Write}, fs::File};
use clap::Parser;
use vemf::{Bstr, codepage, Val, Env};

#[derive(Parser)]
#[clap(dont_collapse_args_in_usage = true)]
#[clap(mut_arg("help", |a| a.help("print help information")))]
struct Args {
    /// filename of the file that will be read.
    /// if not given, opens up a REPL.
    #[clap(index = 1)]
    filename: Option<PathBuf>,

    /// arguments given to the script. sets α, β and δ accordingly
    #[clap(index = 2)]
    arguments: Vec<String>,

    /// whether to include the standard library
    #[clap(long)]
    no_stdlib: bool,

    /// how to format the output. use like dyadic ⁿ. ignored in repl mode.
    #[clap(short, long, default_value = "0")]
    format: String,

    /// if given, prints to stdout the file, rewritten without ' escapes
    #[clap(short, long)]
    rewrite: bool,
    /*
    #[clap(short, long)]
    input: Vec<String>,*/
}

fn rewrite(path: PathBuf) {
    let mut code = String::new();
    File::open(path).unwrap()
        .read_to_string(&mut code).unwrap();
    println!("{}", codepage::tochars_ln(
        &vemf::rewrite( &codepage::tobytes(code.trim_end()).unwrap() )
    ));
}


fn main() {
    let args = Args::parse();
    let mut state = Env::new(Box::new(rand::thread_rng()));
    if !args.no_stdlib {
        state.include_stdlib();
    }
    state.interface = Box::new(vemf::StdIO {});
    //println!("sizeof(Val) = {}", std::mem::size_of::<Val>());
    if let Some(path) = args.filename {
        if args.rewrite { return rewrite(path); }
        let arguments = state.include_args(&args.arguments);
        let mut res = state.include_file(&mut File::open(path).unwrap()).unwrap();
        if let Val::Err(x) = res { std::process::exit(x); }
        if res.is_infinite() { res = res.call(
            &mut state,
            arguments.get(0).cloned().unwrap_or(Val::NAN),
            arguments.get(1).cloned()
        ); }
        if let Val::Err(x) = res { std::process::exit(x); }
        let form = res.format(
            &args.format.chars()
            .filter_map(|x| x.is_ascii_digit().then_some(Val::Int(x as i64 - 0x30)))
            .collect::<Vec<_>>()[..]);
        if !form.is_empty() { println!("{}", form); }
    } else {
        repl(state, args);
    }
}

fn repl(mut state: Env, args: Args) {
    println!("welcome to vemf repl. enjoy your stay");
    loop {
        print!("    ");
        let _ = std::io::stdout().flush();
        let mut code = String::new();
        std::io::stdin().read_line(&mut code).expect("error while reading from stdin");
        if code.trim_start().starts_with(')') {
            if let Some((l, r)) = code[1..].split_once(' ') {
                let val = state.include_string(r);
                state.set_local(Bstr::from(&b"__"[..]), val);
                state.include_string(&format!("__ⁿ({})☻", l));
            } else {
                state.include_string(&format!("__ⁿ({})☻", &code[1..]));
            }
            continue;
        }
        if args.rewrite {
            println!(" r: {}", codepage::tochars(
                &vemf::rewrite(
                    &codepage::tobytes(code.trim_end()).unwrap()
                )
            ));
        }
        let val = state.include_string(&code);
        if !val.is_nan() { println!("{}", val); }
        state.set_local(Bstr::from(&b"__"[..]), val);
    }
}