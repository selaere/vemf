use std::{path::{PathBuf, Path}, io::{Read, Write}, fs::File};
use vemf::{Bstr, codepage, Val, Env, FromIoWrite, bx};

struct Options {
    filename: Option<PathBuf>,
    arguments: Vec<String>,
    no_stdlib: bool,
    inspect: bool,
    format: String,
    rewrite: bool,
}

fn rewrite(path: &Path) {
    let mut code = String::new();
    File::open(path).unwrap().read_to_string(&mut code).unwrap();
    println!("{}", codepage::tochars_ln(
        &vemf::rewrite( &codepage::tobytes(code.trim_end()).unwrap() )
    ));
}

fn fmtstring(format: &str) -> Vec<Val> {
    format.chars()
        .filter_map(|x| x.is_ascii_digit().then_some(Val::Int(x as i64 - 0x30)))
        .collect::<Vec<_>>()
}

fn parse_args() -> Options {
    let mut iter = std::env::args();
    let mut opts = Options{
        filename: None,
        arguments: vec![],
        no_stdlib: false,
        inspect: false,
        format: String::from("0"),
        rewrite: false,
    };
    _ = iter.next();
    loop {
        let arg = iter.next();
        match arg.as_deref() {
            Some("-f" | "--format") => {
                opts.format = iter.next().unwrap_or_else(|| String::from(""));
            }
            Some("-h" | "--help") => {
                println!("\
usage: vemf [options] [filename] [arguments]
  <filename>: filename of the file that will be executed. optional. if not given, opens up a REPL
  <arguments>: arguments given to the script. all arguments after the filename will be passed to \
the script unchanged. sets α, β and δ accordingly.
  -h/--help: print this
  -f/--format <format>: how to format the output, 0 by default. use like dyadic ⁿ. ignored in repl \
mode.
  -r/--rewrite: if given, print to stdout the file, rewritten without ' escapes
  -i/--inspect: open the repl after running file
  --no-stdlib: do not use the standard library\
");
                std::process::exit(0);
            },
            Some("-r" | "--rewrite") => { opts.rewrite   = true; }
            Some("--no-stdlib")      => { opts.no_stdlib = true; }
            Some("-i" | "--inspect") => { opts.inspect   = true; }
            Some(x) if x.starts_with('-') => {
                panic!("unrecognized option {x}")
            }
            Some(_) => {
                opts.filename = Some(PathBuf::from(arg.unwrap()));
                opts.arguments.extend(iter.by_ref());
                break                
            }
            None => break,
        }
    }
    opts
}

fn main() {
    let opts = parse_args();
    let mut env = Env::new(bx(rand::thread_rng()));
    if !opts.no_stdlib { env.include_stdlib(); }
    env.interface = bx(vemf::StdIO {});
    if let Some(ref path) = opts.filename {
        if opts.rewrite { return rewrite(path); }
        env.include_args(&opts.arguments);
        let mut code = String::new();
        File::open(path).unwrap_or_else(|e|
            panic!("error while opening file {}: {}", path.display(), e)
        ).read_to_string(&mut code).unwrap_or_else(|e|
            panic!("error while opening file {}: {}", path.display(), e)
        );
        if let Err(x) = env.run_string(&code, &fmtstring(&opts.format)) {
            std::process::exit(x)
        };
        if opts.inspect { repl(env, opts); }
    } else { repl(env, opts); }
}

fn repl(mut env: Env, args: Options) {
    println!("welcome to vemf repl. enjoy your stay");
    loop {
        print!("    ");
        _ = std::io::stdout().flush();
        let mut code = String::new();
        std::io::stdin().read_line(&mut code).expect("error while reading from stdin");
        if args.rewrite { println!(" r: {}", codepage::tochars(&vemf::rewrite(
            &codepage::tobytes(code.trim_end()).unwrap()
        )));}
        if let Some(cmd) = code.trim_start().strip_prefix(')') {
            if let Some((l, r)) = cmd.split_once(' ') {
                let val = env.include_string(r);
                val.format(&mut FromIoWrite(std::io::stdout()), &fmtstring(l)).unwrap();
                env.set_local(Bstr::from(&b"__"[..]), val);
            } else if let Some(x) = env.get_var(b"__") {
                x.format(&mut FromIoWrite(std::io::stdout()), &fmtstring(cmd)).unwrap();
            }
            println!(); continue;
        }
        let val = env.include_string(&code);
        if !val.is_nan() { println!("{val}"); }
        env.set_local(Bstr::from(&b"__"[..]), val);
    }
}