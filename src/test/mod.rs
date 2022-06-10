#[test]
fn test() -> Result<(), String> {
    let code: &str = include_str!("test.vemf");
    println!("doing unit tests...");
    let mut tested = 0;
    let mut failed = 0;
    let mut env = super::run::Env::new();
    env.include_stdlib();
    for (n, line) in code.lines().enumerate() {
        if line.trim().is_empty() { continue }
        if line.trim().starts_with("' ") { continue }
        if let Some((i, o)) = line.split_once(" : ") {
            let left = env.include_string(i);
            let right = env.include_string(o);
            if left != right {
                println!("test failed line {} ``{i} : {o}``: {left} ≢ {right}", n+1);
                failed += 1;
            } else {
                //println!("test passed line {} ``{i} : {o}``", n+1);
            }
            tested += 1;
        } else {println!("i didnt understand line {}", n)}
    }
    println!("failed {}/{} tests", failed, tested);
    if failed != 0 {
        Err(format!("failed {}/{} tests", failed, tested))
    } else {Ok(())}
}