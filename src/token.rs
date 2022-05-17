#[derive(Debug, Clone)]
pub enum Tok {
    Just(u8), Cst(u8), Stmt(u8),
    VarVal(Vec<u8>), VarFun(Vec<u8>), VarAv1(Vec<u8>), VarAv2(Vec<u8>), VarSet(Vec<u8>),
    Chr(u8), Chr2(u8, u8), Num2(u8, u8), Num3(u8, u8, u8),
    Num(Vec<u8>), HNum(Vec<u8>), Str(Vec<u8>),
}

pub fn tokenize(bytes: &[u8]) -> Vec<Tok> {
    let mut iter = bytes.iter().cloned().peekable(); // .cloned() should noop
    let mut toks = Vec::new();
    'outer: loop {toks.push(match iter.next() {
        Some(b'"') => {
            let mut buf = Vec::new();
            loop { match iter.next() {
                Some(b'"') | None => break,
                Some(c) => buf.push(c),
            }}
            Tok::Str(buf)
        }
        Some(b!('■')) => {
            let mut buf = Vec::new();
            loop { match iter.next() {
                Some(b!('■')) | None => break,
                Some(c) => buf.push(c),
            }}
            Tok::Num(buf)
        }
        Some(b'`') => Tok::Chr(iter.next().unwrap_or(0)),
        Some(b'_') => Tok::Cst(iter.next().unwrap_or(b'_')),
        Some(b!('♥')) => Tok::Chr2(
            iter.next().unwrap_or(  0), iter.next().unwrap_or(  0),
        ),
        Some(b!('░')) => Tok::Num2(
            iter.next().unwrap_or(253), iter.next().unwrap_or(253),
        ),
        Some(b!('▒')) => Tok::Num3(
            iter.next().unwrap_or(253), iter.next().unwrap_or(253), iter.next().unwrap_or(253),
        ),
        Some(b!('˙')) => match iter.next() {
            Some(b' ') => loop { match iter.next() {
                Some(b'\n') | None => continue 'outer,
                _ => continue,
            }},
            Some(first @ (b'0'..=b'9' | b'-')) => {
                let mut buf = vec![first];
                while let Some(&x @ (b'0'..=b'9' | b'.')) = iter.peek() {
                    buf.push(x); iter.next();
                }
                Tok::HNum(buf)
            },
            _ => unimplemented!(),
        },
        Some(typ @ b!('.' ':' '•' '○' '→')) => {
            let variant = match typ {
                b'.'    => Tok::VarVal,
                b':'    => Tok::VarFun,
                b!('•') => Tok::VarAv1,
                b!('○') => Tok::VarAv2,
                b!('→') => Tok::VarSet,
                _ => unreachable!(),
            };
            variant(match iter.next() {
                Some(first @ b'a'..=b'z') => {
                    let mut buf = vec![first];
                    loop { match iter.peek() {
                        Some(&ltr @ b'a'..=b'z') => {
                            iter.next(); buf.push(ltr);
                        }
                        Some(&ltr @ b'A'..=b'Z') => {
                            iter.next(); buf.push(ltr.to_ascii_lowercase()); break;
                        }
                        _ => break,
                    }}
                    buf
                }
                Some(chr) => vec![chr],
                None => panic!(),
            })
        }
        Some(b' ' | b'\n') => continue,
        Some(c @ (
            b!('♣''♠''♂''♀''♫' '►''◄''↕''‼''¶''§''▬''↨''←''↑''↓''∟''▲''▼'
               '!''#''$''%''&''\'''*''+'',''-''/''<''=''>''@''[''\\'']''^''~')
            | 0x80..=0xAF // ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒáíóúñÑªº¿⌐¬½¼¡«»
            | b!('▄''▌''▐''▀''≡''±''≥''≤''⌠''⌡''÷''≈''°''√''ⁿ''²')
        )) => Tok::VarFun(vec![c]),
        Some(c @ (b'A'..=b'Z' | b!('☺''☻'))) => Tok::Stmt(c),
        Some(x @ (b'a'..=b'z' | b!('α''β''σ''μ''τ'))) => Tok::VarVal(vec![x]),
        Some(x) => Tok::Just(x),
        None => break,
    })};
    toks
}
