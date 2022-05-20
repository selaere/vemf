#[macro_use] mod codepage;
mod token;
mod parse;
mod run;

pub type Bstr = smallvec::SmallVec<[u8; 16]>; // length will be the same as a Vec in 64bit archs

fn main() {
    println!("sizeof(Val) = {}", std::mem::size_of::<run::Val>());
    if let Some(path) = std::env::args().nth(1) {
        let code = std::fs::read_to_string(path).expect("error while opening file");
        println!("input : ```{}```", code);
        let tokens = token::tokenize(&codepage::tobytes(&code).unwrap());
        //println!("{:?}", tokens);
        let parsed = parse::parse(&tokens);
        for i in &parsed { println!("parsed: {}", i); }
        let mut state = run::Env::new();
        println!("{}", state.eval_block(&parsed));
    } else {
        let mut state = run::Env::new();
        loop {
            print!("vemf> ");
            let _ = std::io::Write::flush(&mut std::io::stdout());
            let mut code = String::new();
            std::io::stdin().read_line(&mut code).expect("error while reading from stdin");
            // TODO make better
            //println!("input : ```{}```", code);
            let tokens = token::tokenize(&codepage::tobytes(&code).unwrap());
            //println!("{:?}", tokens);
            let parsed = parse::parse(&tokens);
            println!("{}", state.eval_block(&parsed));
        }
    }
}
