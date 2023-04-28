use std::{path::PathBuf, io::{Read, Write}, fs::File};
use vemf::{Bstr, codepage, Val, Env, FromIoWrite, bx};

struct Options {
    filename: Option<PathBuf>,
    arguments: Vec<String>,
    no_stdlib: bool,
    no_rng: bool,
    inspect: bool,
    format: String,
    rewrite: bool,
    prompt: String,
    binary: bool,
    code: Option<String>,
    reencode: Option<Encoding>,
}

fn fmtstring(format: &str) -> Vec<Val> {
    format.chars()
        .filter_map(|x| x.is_ascii_digit().then_some(Val::Int(x as i64 - 0x30)))
        .collect::<Vec<_>>()
}
enum Encoding{ Utf8, Vemf }

fn parse_args() -> Options {
    let mut iter = std::env::args();
    let mut opts = Options{
        filename: None,
        arguments: vec![],
        no_stdlib: false,
        no_rng: false,
        inspect: false,
        format: String::from("0"),
        rewrite: false,
        prompt: std::env::var("VEMF_PROMPT").unwrap_or_else(|_| String::from("    ")),
        binary: false,
        code: None,
        reencode: None
    };
    _ = iter.next();
    loop {
        let arg = iter.next();
        match arg.as_deref() {
            Some("-f" | "--format") => {
                opts.format = iter.next().unwrap_or_else(|| String::from("1"));
            }
            Some("-h" | "--help") => {
                println!("\
usage: vemf [options] [filename] [arguments]
    <filename>: filename of the file that will be executed. optional. if not given, opens up a REPL
    <arguments>: arguments given to the script. all arguments after the filename will be passed to \
the script unchanged. sets α, β and δ accordingly.
    -h/--help: print this
    -f/--format <format>: how to format the output, 0 by default. use like dyadic ⁿ. ignored in repl.
    -i/--inspect: open the repl after running file
    -p/--prompt <prompt>: repl only. use <prompt> as the input prompt
    -b/--binary: read file using the vemf codepage instead of utf-8
    -e/--execute <code>: execute <code> instead of reading file
    --no-stdlib: do not use the standard library
    --no-rng:    do not use a random number generator
    -r/--rewrite: print the file rewritten without ' escapes
    -c/--encode:  print the file reencoded in the vemf codepage
    -C/--decode:  print the file reencoded in utf-8 (use along with -b)");
                std::process::exit(0);
            },
            Some("-r" | "--rewrite") => { opts.rewrite   = true; }
            Some("--no-stdlib")      => { opts.no_stdlib = true; }
            Some("--no-rng")         => { opts.no_rng    = true; }
            Some("-i" | "--inspect") => { opts.inspect   = true; }
            Some("-b" | "--binary")  => { opts.binary    = true; }
            Some("-p" | "--prompt") => {
                opts.prompt = iter.next().unwrap_or_else(|| String::from(""));
            }
            Some("-e" | "--execute") => {
                opts.code = Some(iter.next().unwrap_or_else(|| String::from("░♪₧Ö·")));
            }
            Some("-c" | "--encode") => opts.reencode = Some(Encoding::Vemf),
            Some("-C" | "--decode") => opts.reencode = Some(Encoding::Utf8),
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
    let mut env = Env::new();
    if !opts.no_rng { env.rng = bx(rand::thread_rng()); }
    let mut code: Vec<u8>;
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
    if opts.reencode.is_some() || opts.rewrite {
        if !opts.binary {
            code = codepage::tobytes(core::str::from_utf8(&code)?);
        }
        match opts.reencode {
            Some(Encoding::Vemf) => print!("{}", match opts.rewrite {
                true  => vemf::rewrite(&code),
                false => codepage::tochars(&code),
            }),
            _ => std::io::stdout().write_all(&match opts.rewrite {
                true  => codepage::tobytes(&vemf::rewrite(&code)),
                false => code,
            })?,
        }
        return Ok(());
    }
    let val = match opts.binary {
        true  => env.include_bytes(&code),
        false => env.include_string(core::str::from_utf8(&code)?),
    };
    if let Err(x) = env.run_value(val, &fmtstring(&opts.format)) { std::process::exit(x); }
    println!();
    if opts.inspect { repl(env, opts); }
    Ok(())
}

fn repl(mut env: Env, mut opts: Options) {
    println!("welcome to vemf repl. enjoy your stay");
    opts.format = String::from("1");
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
                println!(" r: {}", vemf::rewrite(
                    &codepage::tobytes(code.trim_end())
                ));
                continue;
            } else if let Some(code) = cmd.strip_prefix("prompt ") {
                opts.prompt = code.to_string();
                continue;
            } else if cmd.starts_with("help") {
                println!("\
)help: show this
)<fmt>: format using <fmt>
)r: toggle rewriting
)exit: exit repl (^C should also work)
)prompt <prompt>: change prompt");
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
                println!();
                continue;
            }
        }
        if opts.rewrite { println!(" r: {}", vemf::rewrite(
            &codepage::tobytes(code.trim_end())
        )); }
        let val = env.include_string(&code);
        if !val.is_nan() {
            val.format(&mut FromIoWrite(std::io::stdout()), &fmtstring(&opts.format)).unwrap();
        }
        println!();
        env.set_local(Bstr::from(&b"__"[..]), val);
    }
}