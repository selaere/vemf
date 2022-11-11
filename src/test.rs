#[test]
fn test() -> Result<(), String> {
    let code: &str = include_str!("../doc/raw.txt");
    println!("doing doc tests...");
    let mut tested = 0;
    let mut failed = 0;
    let mut env = super::run::Env::new();
    env.include_stdlib();
    for (n, line) in code.lines().enumerate() {
        if let Some(line) = line.strip_prefix("> ") {
            if let Some((i, o)) = line.split_once(" ≡ ") {
                let left = env.include_string(i);
                let right = env.include_string(o);
                if left != right {
                    println!("test failed line {} ``{i} ≡ {o}``: {left} ≢ {right}", n+1);
                    failed += 1;
                } else {
                    //println!("test passed line {} ``{i} : {o}``", n+1);
                }
                tested += 1;
            } else if let Some((i, o)) = line.split_once(" ± ") {
                let left = env.include_string(i);
                let right = env.include_string(o);
                if !left.approx(&right) {
                    println!("test failed line {} ``{i} ± {o}``: {left} ≢ {right}", n+1);
                    failed += 1;
                }
                tested += 1;
            }
        }
    }
    println!("failed {}/{} tests", failed, tested);
    if failed != 0 {
        Err(format!("failed {}/{} tests", failed, tested))
    } else {Ok(())}
}