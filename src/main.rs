use std::{path::PathBuf, io::{Read, Write}, fs::File};
use vemf::{Bstr, codepage, Val, Env, FromIoWrite, bx};

struct Options {
    filename: Option<PathBuf>,
    arguments: Vec<String>,
    no_stdlib: bool,
    inspect: bool,
    format: String,
    rewrite: bool,
    prompt: String,
    use_utf8: bool,
    code: Option<String>
}

fn rewrite(thing: &[u8]) {
    println!("{}", codepage::tochars_ln(&vemf::rewrite(thing)));
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
        prompt: std::env::var("VEMF_PROMPT").unwrap_or_else(|_| String::from("    ")),
        use_utf8: true,
        code: None,
    };
    _ = iter.next();
    loop {
        let arg = iter.next();
        match arg.as_deref() {
            Some("-f" | "--format") => {
                opts.format = iter.next().unwrap_or_else(|| String::from(""));
            }
            Some("-h" | "--help") => {
                print!("\
usage: vemf [options] [filename] [arguments]
  <filename>: filename of the file that will be executed. optional. if not given, opens up a REPL
  <arguments>: arguments given to the script. all arguments after the filename will be passed to \
the script unchanged. sets α, β and δ accordingly.
  -h/--help: print this
  -f/--format <format>: how to format the output, 0 by default. use like dyadic ⁿ. ignored in repl.
  -r/--rewrite: if given, print to stdout the file, rewritten without ' escapes (doesn't work very well right now)
  -i/--inspect: open the repl after running file
  --no-stdlib: do not use the standard library
  -p/--prompt <prompt>: repl only. use <prompt> as the input prompt
  -b: read file using the vemf codepage instead of utf-8
  -e <code>: execute <code> instead of reading file
");
                std::process::exit(0);
            },
            Some("-r" | "--rewrite") => { opts.rewrite   = true; }
            Some("--no-stdlib")      => { opts.no_stdlib = true; }
            Some("-i" | "--inspect") => { opts.inspect   = true; }
            Some("-b")               => { opts.use_utf8  = false; }
            Some("-p" | "--prompt") => {
                opts.prompt = iter.next().unwrap_or_else(|| String::from(""));
            }
            Some("-e") => {
                opts.code = Some(iter.next().unwrap_or_else(|| String::from("░♪₧Ö·")));
            }
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = parse_args();
    let mut env = Env::new(bx(rand::thread_rng()));
    let mut code;
    if !opts.no_stdlib { env.include_stdlib(); }
    env.interface = bx(vemf::StdIO {});
    env.include_args(&opts.arguments);
    if let Some(c) = opts.code.as_ref() {
        code = c.as_bytes().to_vec();
    } else if let Some(path) = opts.filename.as_ref() {
        code = Vec::new();
        File::open(path)?.read_to_end(&mut code)?;
    } else {
        repl(env, opts);
        return Ok(());
    }
    if opts.use_utf8 {
        code = codepage::tobytes(core::str::from_utf8(&code)?)
            .ok_or("input contains characters outside codepage")?;
    }
    if opts.rewrite { rewrite(&code); return Ok(()) }
    if let Err(x) = env.run_bytes(&code, &fmtstring(&opts.format)) {
        std::process::exit(x)
    };
    println!();
    if opts.inspect { repl(env, opts); }
    Ok(())
}

fn repl(mut env: Env, mut opts: Options) {
    println!("welcome to vemf repl. enjoy your stay");
    opts.format = String::from("");
    loop {
        print!("{}", opts.prompt);
        _ = std::io::stdout().flush();
        let mut code = String::new();
        std::io::stdin().read_line(&mut code).expect("error while reading from stdin");
        if let Some(cmd) = code.trim_start().trim_end_matches(&['\n','\r']).strip_prefix(')') {
            if cmd == "exit" {
                return
            } else if cmd == "r" {
                opts.rewrite = !opts.rewrite;
                continue;
            } else if let Some(code) = cmd.strip_prefix("r ") {
                println!(" r: {}", codepage::tochars(&vemf::rewrite(
                    &codepage::tobytes(code.trim_end()).unwrap()
                )));
                continue;
            } else if let Some(code) = cmd.strip_prefix("prompt ") {
                opts.prompt = code.to_string();
                continue;
            } else if cmd.starts_with("help") {
                print!("\
)help: show this
)<fmt>: format using <fmt>
)r: toggle rewriting
)exit: exit repl (^C should also work)
)prompt <prompt>: change prompt
");
                continue;
            } else if cmd.starts_with(|x: char| char::is_ascii_digit(&x)) {
                if let Some((l, r)) = cmd.split_once(' ') {
                    let val = env.include_string(r);
                    if !val.is_nan() {
                        val.format(&mut FromIoWrite(std::io::stdout()), &fmtstring(l)).unwrap();
                    }
                    env.set_local(Bstr::from(&b"__"[..]), val);
                } else if let Some(x) = env.get_var(b"__") {
                    x.format(&mut FromIoWrite(std::io::stdout()), &fmtstring(cmd)).unwrap();
                    opts.format = cmd.to_string()
                }
            }
        }
        if opts.rewrite { println!(" r: {}", codepage::tochars(&vemf::rewrite(
            &codepage::tobytes(code.trim_end()).unwrap()
        )));}
        let val = env.include_string(&code);
        if !val.is_nan() {
            val.format(&mut FromIoWrite(std::io::stdout()), &fmtstring(&opts.format)).unwrap();
        }
        println!();
        env.set_local(Bstr::from(&b"__"[..]), val);
    }
}