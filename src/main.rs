#[macro_use] mod codepage;
mod token;
mod parse;
mod run;

fn main() {
    if let Some(path) = std::env::args().nth(1) {
        let code = std::fs::read_to_string(path).expect("error while opening file");
        println!("input : ```{}```", code);
        let tokens = token::tokenize(&codepage::tobytes(&code).unwrap());
        //println!("{:?}", tokens);
        let parsed = parse::parse_file(&tokens);
        for i in &parsed { println!("parsed: {}", i); }
        let mut state = run::Env::new();
        for (n, expr) in parsed.iter().enumerate() {
            let v = state.evaluate(expr);
            if n == parsed.len() - 1 {
                println!("result: {:?}", v);
            }
        }
        println!();
    } else {
        println!("bruh")
    }
}
