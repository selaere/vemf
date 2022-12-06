use crate::prelude::*;

#[derive(Debug, Clone)]
pub enum Tok {
    Just(u8), White(u8), Comment(Bstr),
    VarNoun(Bstr), VarVerb(Bstr), VarAv1(Bstr), VarAv2(Bstr),
    VarSet(Bstr), VarCng(Bstr), VarSetStmt(Bstr), VarCngStmt(Bstr),
    Chr(u8), Chr2(u8, u8), Num2(u8, u8), Num3(u8, u8, u8),
    Num(Bstr), HNum(Bstr), Str(Bstr),
}

#[inline]
fn step(t: &mut&[u8]) -> Option<u8> {
    let a = t.first().copied();
    if a.is_some() {*t = &t[1..]};
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

fn do_escape(a: u8, t: &mut&[u8]) -> Option<u8> {
    if let Some(c) = escape_1c(a) {
        Some(c)
    } else if let Some(c) = escape_2c([a, *t.first().unwrap_or(&0u8)]) {
        step(t); Some(c)
    } else { None }
}

fn string(t: &mut &[u8]) -> Bstr {
    let mut buf = Bstr::new();
    loop { match step(t) {
        Some(b'"') | None => break,
        Some(b'\'') => {
            let a = step(t).unwrap_or(b'\'');
            buf.push(do_escape(a, t).unwrap_or(a));
        },
        Some(b!('¨')) => buf.push(b'"'), Some(b!('·')) => buf.push(b'\''),
        Some(c) => buf.push(c),
    }}
    buf
}

fn ident(t: &mut&[u8]) -> Bstr {
    match step(t) {
        Some(first @ b'a'..=b'z') => {
            let mut buf = smallvec![first];
            loop { match t.first() {
                Some(&ltr @ b'a'..=b'z') => { step(t); buf.push(ltr); }
                Some(&ltr @ b'A'..=b'Z') => { step(t); buf.push(ltr + 32); break; }
                _ => break,
            }}
            buf
        }
        Some(b'_') => smallvec![b'_', step(t).unwrap_or(b'_')],
        Some(b'"') => string(t),
        Some(b'\'') => {
            let a = step(t).unwrap_or(b'\'');
            smallvec![do_escape(a, t).unwrap_or(a)]
        }
        Some(c @ (b']' | b!('['))) => [c].into_iter().chain(ident(t)).collect(),
        Some(chr) => smallvec![chr],
        None => smallvec![],
    }
}

fn token(first: Option<u8>, t: &mut &[u8]) -> Option<Tok> {
    Some(match first {
        Some(b'"') => Tok::Str(string(t)),
        Some(b!('■')) => {
            let mut buf = Bstr::new();
            loop { match step(t) {
                Some(b!('■')) | None => break,
                Some(c) => buf.push(c),
            }}
            Tok::Num(buf)
        }
        Some(b'`') => Tok::Chr(step(t).unwrap_or(0x20)),
        Some(b'_') => {
            let mut c = step(t).unwrap_or(b'_');
            if let b'\'' = c {
                let a = step(t).unwrap_or(b'\'');
                c = do_escape(a, t).unwrap_or(a);
            }
            match c {
                c @ short_verb!() => Tok::VarVerb(smallvec![b'_', c]),
                c => Tok::VarNoun(smallvec![b'_', c]),
            }
        }
        Some(b!('♥')) => Tok::Chr2(
            step(t).unwrap_or(  0), step(t).unwrap_or(  0),
        ),
        Some(b!('░')) => Tok::Num2(
            step(t).unwrap_or(253), step(t).unwrap_or(253),
        ),
        Some(b!('▒')) => Tok::Num3(
            step(t).unwrap_or(253), step(t).unwrap_or(253), step(t).unwrap_or(253),
        ),
        Some(b!('\'')) => match step(t) {
            Some(b' ') | None => {
                let mut buf = Bstr::new();
                loop { match step(t) {
                    Some(b'\n') | None => break,
                    Some(a) => buf.push(a),
                }}
                Tok::Comment(buf)
            },
            Some(first @ (b'0'..=b'9' | b'-')) => {
                let mut buf = smallvec![first];
                while let Some(&x @ (b'0'..=b'9' | b'.')) = t.first() { buf.push(x); step(t); }
                Tok::HNum(buf)
            },
            Some(a) => return token(do_escape(a, t).or_else(|| step(t)), t),
        },
        Some(b!('.')) => Tok::VarNoun   (ident(t)),
        Some(b!(':')) => Tok::VarVerb   (ident(t)),
        Some(b!('•')) => Tok::VarAv1    (ident(t)),
        Some(b!('○')) => Tok::VarAv2    (ident(t)),
        Some(b!('─')) => Tok::VarSetStmt(ident(t)),
        Some(b!('═')) => Tok::VarCngStmt(ident(t)),
        Some(b!('→')) => Tok::VarSet    (ident(t)),
        Some(b!('↔')) => Tok::VarCng    (ident(t)),
        Some(b!('¨')) => Tok::Str       (ident(t)),
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

pub fn tokenize(mut t: &[u8]) -> Vec<Tok> {
    let mut toks = Vec::new();
    while let Some(tok) = token(step(&mut t), &mut t) {
        if let Tok::White(_) | Tok::Comment(_) = tok { continue; }
        toks.push(tok);
    }
    toks
}


fn rewrite_token(first: Option<u8>, t: &mut &[u8]) -> Option<Bstr> {
    let first = first?;
    Some(match first {
        b'"' => rewrite_string(t),
        b!('■') => {
            let mut buf = smallvec![b!('■')];
            loop { match step(t) {
                Some(b!('■')) | None => break,
                Some(c) => buf.push(c),
            }}
            buf
        }
        b'`' => smallvec![b'`', step(t).unwrap_or(0x20)],
        b'_' => {
            let mut c = step(t).unwrap_or(b'_');
            if let b'\'' = c {
                let a = step(t).unwrap_or(b'\'');
                c = do_escape(a, t).unwrap_or(a);
            }
            smallvec![b'_', c]
        }
        b!('♥') => smallvec![
            b!('♥'), step(t).unwrap_or(  0), step(t).unwrap_or(  0),
        ],
        b!('░') => smallvec![
            b!('░'), step(t).unwrap_or(253), step(t).unwrap_or(253),
        ],
        b!('▒') => smallvec![
            b!('▒'), step(t).unwrap_or(253), step(t).unwrap_or(253), step(t).unwrap_or(253),
        ],
        b!('\'') => match step(t) {
            Some(b' ') | None => {
                let mut buf = smallvec![b'\'', b' '];
                loop { match step(t) {
                    Some(b'\n') | None => break,
                    Some(a) => buf.push(a),
                }}
                buf.push(b'\n');
                buf
            },
            Some(first @ (b'0'..=b'9' | b'-')) => {
                let mut buf = smallvec![b'\'', first];
                while let Some(&x @ (b'0'..=b'9' | b'.')) = t.first() {
                    buf.push(x); step(t);
                }
                buf
            },
            Some(a) => return rewrite_token(do_escape(a, t).or_else(|| step(t)), t),
        },
        c @ b!('.'':''•''○''→''↔''¨''─''═') => {
            let mut i = rewrite_identifier(t);
            i.insert(0, c);
            i
        }
        c => smallvec![c]
    })
}


fn rewrite_identifier(t: &mut&[u8]) -> Bstr {
    match step(t) {
        Some(first @ b'a'..=b'z') => {
            let mut buf = smallvec![first];
            while let Some(&ltr @ (b'A'..=b'Z' | b'a'..=b'z')) = t.first() {
                buf.push(ltr); step(t);
                if let b'A'..=b'Z' = ltr { break }
            }
            buf
        }
        Some(b'_') => smallvec![b'_', step(t).unwrap_or(b'_')],
        Some(b'"') => rewrite_string(t),
        Some(b'\'') => {
            let a = step(t).unwrap_or(b'\'');
            smallvec![do_escape(a, t).unwrap_or(a)]
        }
        Some(c @ (b']' | b!('['))) => {
            [c].into_iter().chain(rewrite_identifier(t)).collect()
        }
        Some(chr) => smallvec![chr],
        None => unreachable!(),
    }
}

fn rewrite_string(t: &mut &[u8]) -> Bstr {
    let mut buf = smallvec![b'"'];
    loop { match step(t) {
        c @ (Some(b'"') | None) => {
            if let Some(c) = c { buf.push(c) }
            break
        },
        Some(b'\'') => {
            let a = step(t).unwrap_or(b'\'');
            buf.push(do_escape(a, t).unwrap_or(a));
        },
        Some(c) => buf.push(c),
    }}
    buf
}


pub fn rewrite(mut t: &[u8]) -> Vec<u8> {
    let mut string = Vec::with_capacity(t.len());
    while let Some(str) = rewrite_token(step(&mut t), &mut t) {
        string.extend_from_slice(&str);
    }
    string
}

// !"#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`{|}~
// ‼═☻¶÷·─╙╜↔±○ •╧♪♫≤≡≥¿¡αβ¢♀♂ƒ◄►↕↨└∟▬█◘╨Θ♠↓♦╩√↑Φ¥φ╓╤╖→¬┴╘╕╛≈
fn escape_1c(c: u8) -> Option<u8> {
    macro_rules! bee { ([$($f:tt $t:tt),*], _ => $else:expr) => {
        match c as char { $($f => b!($t),)* _ => $else } 
    }; }
    Some(bee!([
        '!''‼','"''═','#''☻','$''¶','%''÷','&''·','\'''─','(''╙',')''╜','*''↔','+''±',',''○',
               '.''•','/''╧',':''♪',';''♫','<''≤','=''≡','>''≥','?''¿','@''¡','A''α','B''β',
        'C''¢','D''♀','E''♂','F''ƒ','G''◄','H''►','I''↕','J''↨','K''└','L''∟','M''▬','N''█',
        'O''◘','P''╨','Q''Θ','R''♠','S''↓','T''♦','U''╩','V''√','W''↑','X''Φ','Y''¥','Z''φ',
        '[''╓','\\''╤',']''╖','^''→','_''¬','`''┴','{''╘','|''╕','}''╛','~''≈'
    ], _ => return None))
}

fn escape_2c(c: [u8; 2]) -> Option<u8> {
    Some(match &c[..] {
        b"a'"=>b!('á'), b"a`"=>b!('à'), b"a\""=>b!('ä'), b"a^"=>b!('â'), b"a+"=>b!('Ä'),
        b"e'"=>b!('é'), b"e`"=>b!('è'), b"e\""=>b!('ë'), b"e^"=>b!('ê'), b"e+"=>b!('É'),
        b"i'"=>b!('í'), b"i`"=>b!('ì'), b"i\""=>b!('ï'), b"i^"=>b!('î'),
        b"o'"=>b!('ó'), b"o`"=>b!('ò'), b"o\""=>b!('ö'), b"o^"=>b!('ô'), b"o+"=>b!('Ö'),
        b"u'"=>b!('ú'), b"u`"=>b!('ù'), b"u\""=>b!('ü'), b"u^"=>b!('û'), b"u+"=>b!('Ü'),
        b"c+"=>b!('Ç'), b"c,"=>b!('ç'), b"y\""=>b!('ÿ'), b"n~"=>b!('ñ'), b"n+"=>b!('Ñ'),
        b"g+"=>b!('Γ'), b"s+"=>b!('Σ'), b"w+"=>b!('Ω'), b"d-"=>b!('δ'), b"e-"=>b!('ε'),
        b"m-"=>b!('μ'), b"s-"=>b!('σ'), b"t-"=>b!('τ'), b"p-"=>b!('π'), b"f-"=>b!('φ'),
        b"f+"=>b!('Φ'), b"a-"=>b!('α'), b"b-"=>b!('β'), b"f,"=>b!('ƒ'), b"n-"=>b!('ñ'),
        b"a,"=>b!('ª'), b"o,"=>b!('º'), b"z,"=>b!('²'), b"n,"=>b!('ⁿ'), b"t+"=>b!('Θ'),
        b"aO"=>b!('Å'), b"ao"=>b!('å'), b"aE"=>b!('Æ'), b"ae"=>b!('æ'), b"ex"=>b!('⌂'),
        b"pr"=>b!('☺'), b"pl"=>b!('☻'), b"tp"=>b!('♦'), b"c2"=>b!('♥'), b"dm"=>b!('♣'),
        b"sn"=>b!('☼'), b"ss"=>b!('§'), b"sh"=>b!('▬'), b"bl"=>b!('⌐'), b"nl"=>b!('¤'),
        b"bu"=>b!('▲'), b"bd"=>b!('▼'), b"iq"=>b!('¿'), b"hf"=>b!('½'), b"db"=>b!('¼'),
        b"rl"=>b!('«'), b"rr"=>b!('»'), b"cl"=>b!('⌠'), b"fl"=>b!('⌡'), b"fd"=>b!('£'),
        b"hl"=>b!('▌'), b"hu"=>b!('▀'), b"hd"=>b!('▄'), b"hr"=>b!('▐'), b"pt"=>b!('₧'),
        b"is"=>b!('∩'), b"if"=>b!('∞'), b"dg"=>b!('°'), b"nm"=>b!('¨'), b"vr"=>b!('←'),
        b"g1"=>b!('┌'), b"g2"=>b!('│'), b"g3"=>b!('├'), b"g4"=>b!('╞'), b"g5"=>b!('╟'),
        b"g6"=>b!('╠'), b"g7"=>b!('┤'), b"g8"=>b!('╡'), b"g9"=>b!('╢'), b"g0"=>b!('╣'),
        b"vl"=>b!('╬'), b"wn"=>b!('╫'), b"sc"=>b!('┼'), b"s2"=>b!('╪'), b"qd"=>b!('⎕'),
        b"su"=>b!('╦'), b"sp"=>b!('╥'), b"mo"=>b!('┬'), b"co"=>b!('╒'), b"il"=>b!('■'),
        b"ov"=>b!('╝'), b"dr"=>b!('╗'), b"in"=>b!('ε'), b"sq"=>b!('²'), b"fm"=>b!('ⁿ'),
        // ↓ most of these will be changed probably
        b"x1"=>b!('╚'), b"x2"=>b!('╔'), b"x3"=>b!('┐'), b"x4"=>b!('┘'),
        _ => return None,
    })
}
