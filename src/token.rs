use crate::Bstr;
use smallvec::smallvec;

#[derive(Debug, Clone)]
pub enum Tok {
    Just(u8), White(u8), Comment(Bstr),
    VarNoun(Bstr), VarVerb(Bstr), VarAv1(Bstr), VarAv2(Bstr),
    VarSet(Bstr), VarCng(Bstr), VarSetStmt(Bstr), VarCngStmt(Bstr),
    Chr(u8), Chr2(u8, u8), Num2(u8, u8), Num3(u8, u8, u8),
    Num(Bstr), HNum(Bstr), Str(Bstr),
}

#[inline]
fn step(code: &mut &[u8]) -> Option<u8> {
    let a = code.first().copied();
    if a.is_some() {*code = &code[1..]};
    a
}

macro_rules! short_av1 { () => { b!('┼''╪''┴''┬''╧''╤''╕''╒''╛''╘') } }
macro_rules! short_av2 { () => { b!('╬''╫''╩''╦''╨''╥''╖''╓''╜''╙''╝''╚''╗''╔') }; }
macro_rules! short_noun { () => { b'a'..=b'z' | b!('α''β''π''τ''Ω''Σ''δ') }; }
macro_rules! short_verb { () => {
    b!('☺''☻''♦''♣''♠''♂''♀''♫''►''◄''↕''‼''¶''§''▬''↨''↑''↓''←''∟''▲''▼'
       '!''#''$''%''&''*''+'',''-''/'';''<''=''>''@''\\''^''|''~''⌂')
    | 0x80..=0xAF // ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒáíóúñÑªº¿⌐¬½¼¡«»
    | b!('▄''▌''▐''▀''ε''∩''≡''±''≥''≤''⌠''⌡''÷''≈''°''√''ⁿ''²')
}; }

fn do_escape(a: u8, bytes: &mut&[u8]) -> Option<u8> {
    if let Some(c) = onechar_abbr(a) {
        Some(c)
    } else if let Some(c) = twochar_abbr([a, *bytes.first().unwrap_or(&0u8)]) {
        step(bytes); Some(c)
    } else {
        None
    }
}

fn string(bytes: &mut &[u8]) -> Bstr {
    let mut buf = Bstr::new();
    loop { match step(bytes) {
        Some(b'"') | None => break,
        Some(b'\'') => {
            let a = step(bytes).unwrap_or(b'\'');
            buf.push(do_escape(a, bytes).unwrap_or(a));
        },
        Some(b!('¨')) => buf.push(b'"'),
        Some(b!('·')) => buf.push(b'\''),
        Some(c) => buf.push(c),
    }}
    buf
}

fn identifier(bytes: &mut&[u8]) -> Bstr {
    match step(bytes) {
        Some(first @ b'a'..=b'z') => {
            let mut buf = smallvec![first];
            loop { match bytes.first() {
                Some(&ltr @ b'a'..=b'z') => {
                    step(bytes); buf.push(ltr);
                }
                Some(&ltr @ b'A'..=b'Z') => {
                    step(bytes); buf.push(ltr + 32); break;
                }
                _ => break,
            }}
            buf
        }
        Some(b'_') => smallvec![b'_', step(bytes).unwrap_or(b'_')],
        Some(b'"') => string(bytes),
        Some(b'\'') => {
            let a = step(bytes).unwrap_or(b'\'');
            smallvec![do_escape(a, bytes).unwrap_or(a)]
        }
        Some(c @ (b']' | b!('['))) => {
            [c].into_iter().chain(identifier(bytes)).collect()
        }
        Some(chr) => smallvec![chr],
        None => smallvec![],
    }
}

fn token(first: Option<u8>, bytes: &mut &[u8]) -> Option<Tok> {
    Some(match first {
        Some(b'"') => Tok::Str(string(bytes)),
        Some(b!('■')) => {
            let mut buf = Bstr::new();
            loop { match step(bytes) {
                Some(b!('■')) | None => break,
                Some(c) => buf.push(c),
            }}
            Tok::Num(buf)
        }
        Some(b'`') => Tok::Chr(step(bytes).unwrap_or(0x20)),
        Some(b'_') => {
            let mut c = step(bytes).unwrap_or(b'_');
            if let b'\'' = c {
                let a = step(bytes).unwrap_or(b'\'');
                c = do_escape(a, bytes).unwrap_or(a);
            }
            match c {
                c @ short_verb!() => Tok::VarVerb(smallvec![b'_', c]),
                c => Tok::VarNoun(smallvec![b'_', c]),
            }
        }
        Some(b!('♥')) => Tok::Chr2(
            step(bytes).unwrap_or(  0), step(bytes).unwrap_or(  0),
        ),
        Some(b!('░')) => Tok::Num2(
            step(bytes).unwrap_or(253), step(bytes).unwrap_or(253),
        ),
        Some(b!('▒')) => Tok::Num3(
            step(bytes).unwrap_or(253), step(bytes).unwrap_or(253), step(bytes).unwrap_or(253),
        ),
        Some(b!('\'')) => match step(bytes) {
            Some(b' ') | None => {
                let mut buf = Bstr::new();
                loop { match step(bytes) {
                    Some(b'\n') | None => break,
                    Some(a) => buf.push(a),
                }}
                Tok::Comment(buf)
            },
            Some(first @ (b'0'..=b'9' | b'-')) => {
                let mut buf = smallvec![first];
                while let Some(&x @ (b'0'..=b'9' | b'.')) = bytes.first() {
                    buf.push(x); step(bytes);
                }
                Tok::HNum(buf)
            },
            Some(a) => return token(do_escape(a, bytes).or_else(|| step(bytes)), bytes),
        },
        Some(b!('.')) => Tok::VarNoun(identifier(bytes)),
        Some(b!(':')) => Tok::VarVerb(identifier(bytes)),
        Some(b!('•')) => Tok::VarAv1 (identifier(bytes)),
        Some(b!('○')) => Tok::VarAv2 (identifier(bytes)),
        Some(b!('→')) => {
            let ident = identifier(bytes);
            (if let Some(b!('·')) | None = bytes.first() {
                step(bytes); Tok::VarSetStmt
            } else {
                Tok::VarSet
            })(ident)
        },
        Some(b!('↔')) => {
            let ident = identifier(bytes);
            (if let Some(b!('·')) | None = bytes.first() {
                step(bytes); Tok::VarCngStmt
            } else {
                Tok::VarCng
            })(ident)
        },
        Some(b!('¨')) => Tok::Str    (identifier(bytes)),
        Some(c @ (b' ' | b'\n')) => Tok::White(c),
        Some(c @ short_verb!())  => Tok::VarVerb(smallvec![c]),
        Some(c @ short_av1!())   => Tok::VarAv1(smallvec![c]),
        Some(c @ short_av2!())   => Tok::VarAv2(smallvec![c]),
        Some(c @ b'A'..=b'Z')    => Tok::VarVerb(smallvec![c]),
        Some(c @ short_noun!())  => Tok::VarNoun(smallvec![c]),
        Some(b!('σ')) => Tok::VarNoun(smallvec![b!('['), b!('α')]),
        Some(b!('μ')) => Tok::VarNoun(smallvec![b!('['), b!('β')]),
        Some(x) => Tok::Just(x),
        None => return None,
    })
}

pub fn tokenize(mut bytes: &[u8]) -> Vec<Tok> {
    let mut toks = Vec::new();
    while let Some(tok) = token(step(&mut bytes), &mut bytes) {
        if let Tok::White(_) | Tok::Comment(_) = tok { continue; }
        toks.push(tok);
    }
    toks
}


fn rewrite_token(first: Option<u8>, bytes: &mut &[u8]) -> Option<Bstr> {
    let first = first?;
    Some(match first {
        b'"' => rewrite_string(bytes),
        b!('■') => {
            let mut buf = smallvec![b!('■')];
            loop { match step(bytes) {
                Some(b!('■')) | None => break,
                Some(c) => buf.push(c),
            }}
            buf
        }
        b'`' => smallvec![b'`', step(bytes).unwrap_or(0x20)],
        b'_' => {
            let mut c = step(bytes).unwrap_or(b'_');
            if let b'\'' = c {
                let a = step(bytes).unwrap_or(b'\'');
                c = do_escape(a, bytes).unwrap_or(a);
            }
            smallvec![b'_', c]
        }
        b!('♥') => smallvec![
            b!('♥'), step(bytes).unwrap_or(  0), step(bytes).unwrap_or(  0),
        ],
        b!('░') => smallvec![
            b!('░'), step(bytes).unwrap_or(253), step(bytes).unwrap_or(253),
        ],
        b!('▒') => smallvec![
            b!('▒'), step(bytes).unwrap_or(253), step(bytes).unwrap_or(253), step(bytes).unwrap_or(253),
        ],
        b!('\'') => match step(bytes) {
            Some(b' ') | None => {
                let mut buf = smallvec![b'\'', b' '];
                loop { match step(bytes) {
                    Some(b'\n') | None => break,
                    Some(a) => buf.push(a),
                }}
                buf.push(b'\n');
                buf
            },
            Some(first @ (b'0'..=b'9' | b'-')) => {
                let mut buf = smallvec![b'\'', first];
                while let Some(&x @ (b'0'..=b'9' | b'.')) = bytes.first() {
                    buf.push(x); step(bytes);
                }
                buf
            },
            Some(a) => return rewrite_token(do_escape(a, bytes).or_else(|| step(bytes)), bytes),
        },
        c @ b!('.'':''•''○''→''↔''¨') => {
            let mut i = rewrite_identifier(bytes);
            i.insert(0, c);
            i
        }
        c => smallvec![c]
    })
}


fn rewrite_identifier(bytes: &mut&[u8]) -> Bstr {
    match step(bytes) {
        Some(first @ b'a'..=b'z') => {
            let mut buf = smallvec![first];
            while let Some(&ltr @ (b'A'..=b'Z' | b'a'..=b'z')) = bytes.first() {
                buf.push(ltr); step(bytes);
                if let b'A'..=b'Z' = ltr { break }
            }
            buf
        }
        Some(b'_') => smallvec![b'_', step(bytes).unwrap_or(b'_')],
        Some(b'"') => rewrite_string(bytes),
        Some(b'\'') => {
            let a = step(bytes).unwrap_or(b'\'');
            smallvec![do_escape(a, bytes).unwrap_or(a)]
        }
        Some(c @ (b']' | b!('['))) => {
            [c].into_iter().chain(rewrite_identifier(bytes)).collect()
        }
        Some(chr) => smallvec![chr],
        None => panic!(),
    }
}

fn rewrite_string(bytes: &mut &[u8]) -> Bstr {
    let mut buf = smallvec![b'"'];
    loop { match step(bytes) {
        c @ (Some(b'"') | None) => {
            if let Some(c) = c { buf.push(c) }
            break
        },
        Some(b'\'') => {
            let a = step(bytes).unwrap_or(b'\'');
            buf.push(do_escape(a, bytes).unwrap_or(a));
        },
        Some(c) => buf.push(c),
    }}
    buf
}


pub fn rewrite(mut bytes: &[u8]) -> Vec<u8> {
    let mut string = Vec::with_capacity(bytes.len());
    
    while let Some(str) = rewrite_token(step(&mut bytes), &mut bytes) {
        string.extend_from_slice(&str);
    }
    string
}

// !"#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`{|}~
// ‼╕☻¶÷⌐'╙╜↔±♪ •╧○♫≤≡≥¿¡αβ¢δɛƒ↑↓↕↨∟◄μ█◘╨Θ►στ╩√♠Φ¥φ╓╤╖→¬┴╘·╛≈
fn onechar_abbr(c: u8) -> Option<u8> { Some(match c {
    b'!' =>b!('‼'), b'"' =>b!('╕'), b'#' =>b!('☻'), b'$' =>b!('¶'), b'%' =>b!('÷'), b'&' =>b!('⌐'), 
    b'\''=>b!('\''),b'(' =>b!('╙'), b')' =>b!('╜'), b'*' =>b!('↔'), b'+' =>b!('±'), b',' =>b!('♪'), 
    b'.' =>b!('•'), b'/' =>b!('╧'), b':' =>b!('○'), b';' =>b!('♫'), b'<' =>b!('≤'), b'=' =>b!('≡'), 
    b'>' =>b!('≥'), b'?' =>b!('¿'), b'@' =>b!('¡'), b'A' =>b!('α'), b'B' =>b!('β'), b'C' =>b!('¢'), 
    b'D' =>b!('δ'), b'E' =>b!('ε'), b'F' =>b!('ƒ'), b'G' =>b!('↑'), b'H' =>b!('↓'), b'I' =>b!('↕'), 
    b'J' =>b!('↨'), b'K' =>b!('∟'), b'L' =>b!('╜'), b'M' =>b!('μ'), b'N' =>b!('█'), b'O' =>b!('◘'), 
    b'P' =>b!('╨'), b'Q' =>b!('Θ'), b'R' =>b!('╙'), b'S' =>b!('σ'), b'T' =>b!('τ'), b'U' =>b!('╩'), 
    b'V' =>b!('√'), b'W' =>b!('♠'), b'X' =>b!('Φ'), b'Y' =>b!('¥'), b'Z' =>b!('φ'), b'[' =>b!('╓'),
    b'\\'=>b!('╤'), b']' =>b!('╖'), b'^' =>b!('→'), b'_' =>b!('¬'), b'`' =>b!('┴'), b'{' =>b!('╘'),
    b'|' =>b!('·'), b'}' =>b!('╛'), b'~' =>b!('≈'), 
    _ => return None
})}

fn twochar_abbr(c: [u8; 2]) -> Option<u8> {
    Some(match &c[..] {
        b"a-"=>b!('á'), b"a`"=>b!('à'), b"a\""=>b!('ä'), b"a^"=>b!('â'), b"a+"=>b!('Ä'),
        b"e-"=>b!('é'), b"e`"=>b!('è'), b"e\""=>b!('ë'), b"e^"=>b!('ê'), b"e+"=>b!('É'),
        b"i-"=>b!('í'), b"i`"=>b!('ì'), b"i\""=>b!('ï'), b"i^"=>b!('î'),
        b"o-"=>b!('ó'), b"o`"=>b!('ò'), b"o\""=>b!('ö'), b"o^"=>b!('ô'), b"o+"=>b!('Ö'),
        b"u-"=>b!('ú'), b"u`"=>b!('ù'), b"u\""=>b!('ü'), b"u^"=>b!('û'), b"u+"=>b!('Ü'),
        b"c+"=>b!('Ç'), b"c,"=>b!('ç'), b"y\""=>b!('ÿ'), b"n~"=>b!('ñ'), b"n+"=>b!('Ñ'),
        b"g+"=>b!('Γ'), b"s+"=>b!('Σ'), b"w+"=>b!('Ω'), 
        b"aO"=>b!('Å'), b"ao"=>b!('å'), b"aE"=>b!('Æ'), b"ae"=>b!('æ'), b"pi"=>b!('π'),
        b"a,"=>b!('ª'), b"o,"=>b!('º'), b"z,"=>b!('²'), b"n,"=>b!('ⁿ'),
        b"pr"=>b!('☺'), b"pl"=>b!('☻'), b"tp"=>b!('♦'), b"c2"=>b!('♥'), b"dm"=>b!('♣'),
        b"dl"=>b!('♂'), b"dr"=>b!('♀'), b"sn"=>b!('☼'), b"ss"=>b!('§'), b"sh"=>b!('▬'),
        b"bu"=>b!('▲'), b"bd"=>b!('▼'), b"iq"=>b!('¿'), b"hf"=>b!('½'), b"db"=>b!('¼'),
        b"rl"=>b!('«'), b"rr"=>b!('»'), b"cl"=>b!('⌠'), b"fl"=>b!('⌡'), b"gb"=>b!('£'),
        b"hl"=>b!('▌'), b"hu"=>b!('▀'), b"hd"=>b!('▄'), b"hr"=>b!('▐'), b"pt"=>b!('₧'),
        b"is"=>b!('∩'), b"if"=>b!('∞'), b"dg"=>b!('°'), b"nm"=>b!('¨'), b"vr"=>b!('←'),
        b"w2"=>b!('┐'), b"m2"=>b!('┌'), b"w3"=>b!('┤'), b"m3"=>b!('├'),
        b"w4"=>b!('╡'), b"m4"=>b!('╞'), b"w5"=>b!('╢'), b"m5"=>b!('╟'),
        b"w6"=>b!('╣'), b"m6"=>b!('╠'), b"wf"=>b!('┘'), b"mf"=>b!('└'),
        b"vl"=>b!('╬'), b"wn"=>b!('╫'), b"sc"=>b!('┼'), b"s2"=>b!('╪'),
        b"su"=>b!('╦'), b"sp"=>b!('╥'), b"mo"=>b!('┬'), b"co"=>b!('╒'), b"vb"=>b!('│'),
        b"ov"=>b!('╝'), b"dp"=>b!('╗'),
        // ↓ most of these will be changed probably
        b"x1"=>b!('─'), b"x2"=>b!('═'), b"x3"=>b!('╚'), b"x4"=>b!('╔'),
        _ => return None,
    })
}
