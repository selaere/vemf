use core::cell::RefCell;

use alloc::collections::VecDeque;
use rand::rngs;
use crate::{prelude::*, run::io::io_result};

const DOCS: &str = include_str!("../doc/raw.txt");

#[test]
fn docs() -> Result<(), String> {
    println!("doing doc tests...");
    let mut tested = 0;
    let mut failed = 0;
    let mut env = super::run::Env::new(bx(rngs::mock::StepRng::new(0, 0)));
    env.include_stdlib();
    for (n, line) in DOCS.lines().enumerate() {
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
    println!("failed {failed}/{tested} tests");
    if failed != 0 {
        Err(format!("failed {failed}/{tested} tests"))
    } else {Ok(())}
}

#[test]
fn docs_escapes() {
    use crate::codepage::{tobyte, tochars, tochar};
    let mut trolls = Vec::new();
    for page in DOCS.split("\n---\n") {
        let Some(char) = page.lines().flat_map(|x| x.strip_prefix(":char: ")).next() else {continue};
        let Some(escape) = page.lines().flat_map(|x| x.strip_prefix(":ascii: ")).next() else {continue};
        for e in escape.split(' ') {
            let re = crate::rewrite(
                &iter::once(b'"').chain(e.bytes()).chain(iter::once(b'"')).collect::<Vec<u8>>()[..]
            );
            let th = tobyte(char.chars().next().unwrap()).unwrap();
            if re[..] != [b'"', th, b'"'] {
                trolls.push(format!("{} != \"{}\"", tochars(&re), tochar(th)));
            }
        }
    }
    if !trolls.is_empty() { panic!("{}", trolls.join("\n")) }
}

#[allow(clippy::type_complexity)]
struct IO<'io> (&'io RefCell<(VecDeque<u8>, VecDeque<u8>, Vec<u8>, Vec<u8>)>);
impl<'io> crate::Interface<'io> for IO<'io> {
    // we should implement read_to_end and read_line, but i want to see if the default defs work
    fn read(&mut self, stm: usize, slice: &mut [u8]) -> Option<usize> {
        if stm == 0 {
            io_result(std::io::Read::read(&mut self.0.borrow_mut().0, slice))
        } else if stm == 1 {
            io_result(std::io::Read::read(&mut self.0.borrow_mut().1, slice))
        } else { None }
    }
    fn write(&mut self, stm: usize, slice: &[u8]) -> Option<usize> {
             if stm == 0 { self.0.borrow_mut().2.extend(slice); Some(slice.len()) }
        else if stm == 1 { self.0.borrow_mut().3.extend(slice); Some(slice.len()) }
        else { None }
    }
}

#[test]
fn input_output() -> Result<(), ()> {
    const PROGRAM: &str = r#"
        "Hello, World!!"☻·"print"☺"ln"☻ ' basic printing
        "line of file: ",(_1Ü)Ö0· ' line reading
        5Ü;"5 bytes: "Ö· ' byte reading
        ∞Ü;"the rest: "☺· ' all reading
        "errors",ΦÖ1=7*1☺· ' writing to other stream
        "thís fïl"ë≡(ΦÜ1)*2☺· ' reading from other stream also WOW chars != bytes
        "e c",("FFFD"º_l)≡(4Ü1)*3☺· ' U+FFFDing when illegal sequence
        "╕nt"≡(3_Ü1)*4ⁿ_Ö· ' binary data, even when invalid utf-8
        "ains"≡(4Ü1)*5☻· ' for ascii, utf-8 and cp437 are  the same
        7_Ü1_Ö· ' pass bytes without modifying
        ∞_Ü1+'11_Ö· ' outputting raw bytes
    "#;
    let refcell = RefCell::new((
        VecDeque::from(*b"this is a file.\nit has content"),
        VecDeque::from("thís fïle cøntains ∨alid ∪⊤ƒ—八".as_bytes().to_vec()),
        Vec::new(),
        Vec::new()));

    let mut env = crate::Env::new(bx(rngs::mock::StepRng::new(0, 0)));
    env.interface = bx(IO(&refcell));
    env.include_stdlib();
    env.include_string(PROGRAM);

    let (_, _, o, e) = refcell.take();
    if o[..] == b"\
        Hello, World!!\n\
        println\n\
        line of file: this is a file.\n\
        5 bytes: it ha\
        the rest: s content\
        12345\n \
        \xe2\x88\xa8ali\
        o+\xed\x93\xb5\xed\x95\xaf\xd1\x9d\xed\x8b\x9f\xf0\x90\xb6\
    "[..] && e[..] == b"errors\n"[..] { Ok(()) } else {
        println!("o=\"{}\" e=\"{}\"", dispbytes(&o), dispbytes(&e));
        Err(())
    }
}

fn dispbytes(a: &[u8]) -> String { a.iter().map(|x| match x {
    b'\\' => r"\\".to_string(), b'\"' => "\\\"".to_string(), b'\n' => "\\n".to_string(),
    b'\0'..=b'\x1F' | b'\x7F'..=b'\xFF' => format!("\\x{x:02x}"),
    _ => char::from(*x).to_string()
}).collect() }

#[test]
fn scripts() {
    let mut env = crate::Env::new(bx(rngs::mock::StepRng::new(0, 0)));
    env.include_stdlib();
    assert_eq!(&env.include_string(include_str!("../scripts/pascal.vemf")).display_string(), &r#"
                                               1                                          
                                            1     1                                       
                                         1     2     1                                    
                                      1     3     3     1                                 
                                   1     4     6     4     1                              
                                1     5    10    10     5     1                           
                             1     6    15    20    15     6     1                        
                          1     7    21    35    35    21     7     1                     
                       1     8    28    56    70    56    28     8     1                  
                    1     9    36    84   126   126    84    36     9     1               
                 1    10    45   120   210   252   210   120    45    10     1            
              1    11    55   165   330   462   462   330   165    55    11     1         
           1    12    66   220   495   792   924   792   495   220    66    12     1      
        1    13    78   286   715  1287  1716  1716  1287   715   286    78    13     1   
     1    14    91   364  1001  2002  3003  3432  3003  2002  1001   364    91    14     1"#[1..]);
    
    assert_eq!(&env.include_string(include_str!("../scripts/fizzbuzz.vemf")).display_string(),
        "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n16\n17\nFizz\n19\
        \nBuzz\nFizz\n22\n23\nFizz\nBuzz\n26\nFizz\n28\n29\nFizzBuzz\n31\n32\nFizz\n34\nBuzz\nFizz\
        \n37\n38\nFizz\nBuzz\n41\nFizz\n43\n44\nFizzBuzz\n46\n47\nFizz\n49\nBuzz\nFizz\n52\n53\nFi\
        zz\nBuzz\n56\nFizz\n58\n59\nFizzBuzz\n61\n62\nFizz\n64\nBuzz\nFizz\n67\n68\nFizz\nBuzz\n71\
        \nFizz\n73\n74\nFizzBuzz\n76\n77\nFizz\n79\nBuzz\nFizz\n82\n83\nFizz\nBuzz\n86\nFizz\n88\n\
        89\nFizzBuzz\n91\n92\nFizz\n94\nBuzz\nFizz\n97\n98\nFizz\nBuzz");
    assert_eq!(&env.include_string(include_str!("../scripts/rule90.vemf")).display_string(), r#"
█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░░░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░░░░░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████░░░░████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░█░░░█░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░██░░██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░█░█░█░█░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░░░░░░░░░░░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░░░░░░░░░░░░░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████░░░░░░░░░░░░████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░█░░░░░░░░░░░█░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░██░░░░░░░░░░██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░█░█░░░░░░░░░█░█░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████████░░░░░░░░████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░░░░░█░░░░░░░█░░░░░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░░░░░██░░░░░░██░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░░░░░█░█░░░░░█░█░░░░░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████░░░░████░░░░████░░░░████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░██░░██░░██░░██░░██░░██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████████████████████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░██░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░█░█░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
████░░░░░░░░░░░░░░░░░░░░░░░░░░░░████░░░░░░░░░░░░░░░░░░░░░░░░░░░░
█░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░█░░░█░░░░░░░░░░░░░░░░░░░░░░░░░░░
██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░██░░██░░░░░░░░░░░░░░░░░░░░░░░░░░
█░█░█░█░░░░░░░░░░░░░░░░░░░░░░░░░█░█░█░█░░░░░░░░░░░░░░░░░░░░░░░░░
████████░░░░░░░░░░░░░░░░░░░░░░░░████████░░░░░░░░░░░░░░░░░░░░░░░░
█░░░░░░░█░░░░░░░░░░░░░░░░░░░░░░░█░░░░░░░█░░░░░░░░░░░░░░░░░░░░░░░
██░░░░░░██░░░░░░░░░░░░░░░░░░░░░░██░░░░░░██░░░░░░░░░░░░░░░░░░░░░░
█░█░░░░░█░█░░░░░░░░░░░░░░░░░░░░░█░█░░░░░█░█░░░░░░░░░░░░░░░░░░░░░
████░░░░████░░░░░░░░░░░░░░░░░░░░████░░░░████░░░░░░░░░░░░░░░░░░░░
█░░░█░░░█░░░█░░░░░░░░░░░░░░░░░░░█░░░█░░░█░░░█░░░░░░░░░░░░░░░░░░░
██░░██░░██░░██░░░░░░░░░░░░░░░░░░██░░██░░██░░██░░░░░░░░░░░░░░░░░░
█░█░█░█░█░█░█░█░░░░░░░░░░░░░░░░░█░█░█░█░█░█░█░█░░░░░░░░░░░░░░░░░
████████████████░░░░░░░░░░░░░░░░████████████████░░░░░░░░░░░░░░░░
█░░░░░░░░░░░░░░░█░░░░░░░░░░░░░░░█░░░░░░░░░░░░░░░█░░░░░░░░░░░░░░░
██░░░░░░░░░░░░░░██░░░░░░░░░░░░░░██░░░░░░░░░░░░░░██░░░░░░░░░░░░░░
█░█░░░░░░░░░░░░░█░█░░░░░░░░░░░░░█░█░░░░░░░░░░░░░█░█░░░░░░░░░░░░░
████░░░░░░░░░░░░████░░░░░░░░░░░░████░░░░░░░░░░░░████░░░░░░░░░░░░
█░░░█░░░░░░░░░░░█░░░█░░░░░░░░░░░█░░░█░░░░░░░░░░░█░░░█░░░░░░░░░░░
██░░██░░░░░░░░░░██░░██░░░░░░░░░░██░░██░░░░░░░░░░██░░██░░░░░░░░░░
█░█░█░█░░░░░░░░░█░█░█░█░░░░░░░░░█░█░█░█░░░░░░░░░█░█░█░█░░░░░░░░░
████████░░░░░░░░████████░░░░░░░░████████░░░░░░░░████████░░░░░░░░
█░░░░░░░█░░░░░░░█░░░░░░░█░░░░░░░█░░░░░░░█░░░░░░░█░░░░░░░█░░░░░░░
██░░░░░░██░░░░░░██░░░░░░██░░░░░░██░░░░░░██░░░░░░██░░░░░░██░░░░░░
█░█░░░░░█░█░░░░░█░█░░░░░█░█░░░░░█░█░░░░░█░█░░░░░█░█░░░░░█░█░░░░░
████░░░░████░░░░████░░░░████░░░░████░░░░████░░░░████░░░░████░░░░
█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░█░░░
██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░
█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░█░
████████████████████████████████████████████████████████████████"#.trim());
}
