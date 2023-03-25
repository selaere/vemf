use crate::prelude::*;
use crate::c64;

#[derive(Debug, Clone)]
pub enum Tok {
    Just(u8), White(u8), Comment(Bstr),
    VNoun(Bstr), VVerb(Bstr), VAv1(Bstr), VAv2(Vec<Bstr>, Bstr),
    VSet(Bstr), VMut(Bstr), VSetS(Bstr), VMutS(Bstr),
    Chr(u8), Chr2(u8, u8), Num(i64), Flt(c64), Str(Bstr)
}
use Tok::*;

#[inline]
fn step(t: &mut&[u8]) -> Option<u8> {
    let a = t.first().copied();
    if a.is_some() {*t = &t[1..]};
    a
}

macro_rules! short_av1 { () => { b!('┼''╪''┴''┬''╧''╤''╕''╒''╛''╘''┐''┌') } }
macro_rules! short_av2 { () => { b!('╬''╫''╩''╦''╨''╥''╖''╓''╜''╙''╝''╚''╗''╔''║') }; }
macro_rules! short_noun { () => { b'a'..=b'z' | b!('α''β''τ''Ω''Σ''δ') }; }
macro_rules! short_verb { () => {
    b!('☺''☻''♥''♦''♣''♠''♂''♀''♫''►''◄''↕''‼''¶''§''▬''↨''↑''↓''←''∟''▲''▼'
       '!''#''$''%''&''*''+'',''-''/'';''<''=''>''@''\\''^''|''~''⌂')
    | 0x80..=0xAF // ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒáíóúñÑªº¿⌐¬½¼¡«»
    | b!('▄''▌''▐''▀''ε''∩''≡''±''≥''≤''⌠''⌡''÷''≈''°''√''ⁿ''²')
}; }

fn do_escape(a: u8, t: &mut&[u8]) -> Option<u8> {
    if let Some(c) = escape_1c(a) {
        Some(c)
    } else if let Some(c) = escape_2c([a, *t.first().unwrap_or(&0)]) {
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
            let mut buf = bstr![first];
            loop { match t.first() {
                Some(&ltr @ b'a'..=b'z') => { step(t); buf.push(ltr); }
                Some(&ltr @ b'A'..=b'Z') => { step(t); buf.push(ltr + 32); break; }
                _ => break,
            }}
        buf }
        Some(b'_') => bstr![b'_', step(t).unwrap_or(b'_')],
        Some(b'"') => string(t),
        Some(b'\'') => {
            let a = step(t).unwrap_or(b'\'');
            bstr![do_escape(a, t).unwrap_or(a)]
        }
        Some(c @ b!('←''→')) => [c].into_iter().chain(ident(t)).collect(),
        Some(chr) => bstr![chr],
        None => bstr![],
    }
}

fn byte_lit(t: &mut &[u8], n: u8) -> Tok {
    let f = step(t).unwrap_or(0);
    let mut num = i64::from(f) - 256 * i64::from(f > 127);
    for _ in 1..n {
        num = (num << 8) + i64::from(step(t).unwrap_or(0));
    }
    Num(num)
}

fn hnum(t: &mut &[u8], first: u8, is_neg: bool) -> Tok {
    let b: i64 = match *t.first().unwrap_or(&0) {
        b'x' => 16, b'b' =>  2, b'o' => 8,
        b's' => 6,  b'z' => 12, b'n' => 36,
        _ => 0
    };
    if first == b'0' && b != 0 {
        let mut num = 0i64;
        loop {
            step(t);
            let a = match t.first() {
                Some(c@b'0'..=b'9') => i64::from(c - b'0'),
                Some(c@b'a'..=b'z') => i64::from(c - b'a' + 10),
                _ => break
            };
            if a > b { break }
            num = num * b + a;
        }
        return Num(num * if is_neg {-1} else {1})
    }
    let mut buf = String::from(first as char);
    loop {
        let Some(x) = t.first().copied().map(char::from) else { break };
        if x.is_ascii_digit()
        || x == '.' && !buf.contains('e') && !buf.contains('.')
        || x == 'e' && !buf.contains('e') && matches!(t.get(1), Some(b'0'..=b'9'|b'-'))
        || x == '-' && buf.bytes().last() == Some(b'e') {
            buf.push(x); step(t);
        } else { break }
    }
    if buf.ends_with("e-") { buf.pop(); buf.pop(); }
    buf.parse::<i64>().map_or_else(
        |_| Flt(c64::new(buf.parse::<f64>().unwrap() * if is_neg {-1.} else {1.}, 0.)),
        |x| Num(x * if is_neg {-1} else {1}),
    )
}

fn token(first: Option<u8>, t: &mut &[u8]) -> Option<Tok> {
    Some(match first {
        Some(b'"') => Str(string(t)),
        Some(b!('█')) => {
            let Some(n @ b'1'..=b'8') = step(t) else { return None }; // rethink this
            byte_lit(t, n - b'0')
        }
        Some(b'`') => Chr(step(t).unwrap_or(0x20)),
        Some(b'_') => {
            let mut c = step(t).unwrap_or(b'_');
            if let b'\'' = c {
                let a = step(t).unwrap_or(b'\'');
                c = do_escape(a, t).unwrap_or(a);
            }
            match c {
                c @ short_verb!() => VVerb(bstr![b'_', c]),
                c @ short_av1!()  => VAv1(bstr![b'_', c]),
                c @ short_av2!()  => VAv2(vec![], bstr![b'_', c]),
                c => VNoun(bstr![b'_', c]),
            }
        }
        Some(b!('░')) => byte_lit(t, 2),
        Some(b!('▒')) => byte_lit(t, 3),
        Some(b!('▓')) => Chr2(step(t).unwrap_or(0), step(t).unwrap_or(0)),
        Some(b!('\'')) => match step(t) {
            Some(b' ') | None => {
                let mut buf = Bstr::new();
                loop { match step(t) {
                    Some(b'\n') | None => break,
                    Some(a) => buf.push(a),
                }}
            Comment(buf) },
            Some(a) => return token(do_escape(a, t).or_else(|| step(t)), t),
        },
        Some(b!('.')) => VNoun(ident(t)), Some(b!('¨')) => Str  (ident(t)),
        Some(b!('•')) => VAv1 (ident(t)), Some(b!('○')) => VAv2 (vec![], ident(t)),
        Some(b!('→')) => VSetS(ident(t)), Some(b!('↔')) => VMutS(ident(t)),
        Some(b!('─')) => VSet (ident(t)), Some(b!('═')) => VMut (ident(t)),
        Some(b!(':')) => if let Some(first @ (b'0'..=b'9' | b'-')) = t.first() {
            step(t);
            if *first == b'-' {
                if let Some(first @ (b'0'..=b'9')) = t.first() {
                    step(t);
                    hnum(t, *first, true)
                } else {
                    VVerb(bstr![b'-'])
                }   
            } else {
                hnum(t, *first, false)
            }
            
        } else { VVerb(ident(t)) },
        Some(c @ (b' ' | b'\n')) => White(c),
        Some(c @ (short_verb!() | b'A'..=b'Z')) => VVerb(bstr![c]),
        Some(c @ short_av1!())   => VAv1 (bstr![c]),
        Some(c @ short_av2!())   => VAv2 (vec![], bstr![c]),
        Some(c @ short_noun!())  => VNoun(bstr![c]),
        Some(b!('σ')) => VNoun(bstr![b!('→'), b!('α')]),
        Some(b!('μ')) => VNoun(bstr![b!('→'), b!('β')]),
        Some(x) => Just(x),
        None => return None,
    })
}

pub fn tokenize(mut t: &[u8]) -> Vec<Tok> {
    let mut toks = Vec::new();
    while let Some(mut tok) = token(step(&mut t), &mut t) {
        if let White(_) | Comment(_) = tok { continue; }
        if let VAv2(v, _) = &mut tok {
            while let Some(VAv1(_)) = toks.last() {
                let Some(VAv1(l)) = toks.pop() else { unreachable!() };
                v.push(l);
            }
        }
        toks.push(tok);
    }
    toks
}


fn rewrite_token(first: Option<u8>, t: &mut &[u8]) -> Option<Bstr> {
    Some(match first? {
        b'"' => rewrite_string(t),
        b!('█') => {
            let Some(n @ b'1'..=b'8') = step(t) else { return None };
            let mut buf = bstr![b!('█')];
            for _ in 0..(n - b'0') {
                buf.push(step(t).unwrap_or(0));
            }
        buf }
        b'`' => bstr![b'`', step(t).unwrap_or(0x20)],
        b'_' => {
            let mut c = step(t).unwrap_or(b'_');
            if let b'\'' = c {
                let a = step(t).unwrap_or(b'\'');
                c = do_escape(a, t).unwrap_or(a);
            }
            bstr![b'_', c]
        }
        b!('░') => bstr![b!('░'), step(t).unwrap_or(0), step(t).unwrap_or(0)],
        b!('▒') => bstr![b!('▒'), step(t).unwrap_or(0), step(t).unwrap_or(0), step(t).unwrap_or(0)],
        b!('▓') => bstr![b!('▓'), step(t).unwrap_or(0), step(t).unwrap_or(0)],
        b!('\'') => match step(t) {
            Some(b' ') | None => {
                let mut buf = bstr![b'\'', b' '];
                loop { match step(t) {
                    Some(b'\n') | None => break,
                    Some(a) => buf.push(a),
                }}
                buf.push(b'\n');
            buf },
            Some(first @ (b'0'..=b'9' | b'-')) => {
                let mut buf = bstr![b'\'', first];
                while let Some(&x @ (b'0'..=b'9' | b'.')) = t.first() {
                    buf.push(x); step(t);
                }
            buf },
            Some(a) => return rewrite_token(do_escape(a, t).or_else(|| step(t)), t),
        },
        c @ b!('.'':''•''○''→''↔''¨''─''═') => {
            let mut i = rewrite_identifier(t);
            i.insert(0, c);
        i }
        c => bstr![c]
    })
}


fn rewrite_identifier(t: &mut&[u8]) -> Bstr {
    match step(t) {
        Some(first @ b'a'..=b'z') => {
            let mut buf = bstr![first];
            while let Some(&ltr @ (b'A'..=b'Z' | b'a'..=b'z')) = t.first() {
                buf.push(ltr); step(t);
                if let b'A'..=b'Z' = ltr { break }
            }
        buf }
        Some(b'_') => bstr![b'_', step(t).unwrap_or(b'_')],
        Some(b'"') => rewrite_string(t),
        Some(b'\'') => {
            let a = step(t).unwrap_or(b'\'');
            bstr![do_escape(a, t).unwrap_or(a)]
        }
        Some(c @ b!('←''→')) => {
            [c].into_iter().chain(rewrite_identifier(t)).collect()
        }
        Some(chr) => bstr![chr],
        None => unreachable!(),
    }
}

fn rewrite_string(t: &mut &[u8]) -> Bstr {
    let mut buf = bstr![b'"'];
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

// !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`{|}~
// ‼╕☻¶÷·'╙╜↔±○¬•╧╣┌│├╞╟╠┤╡╢♪♫≤≡≥¿¡αβ¢♀♂ƒ◄►↕↨└∟▬■◘╨Θ♠↓♦╩√↑Φ¥φ╓╤╖→⌐┴╘♥╛≈
pub fn escape_1c(c: u8) -> Option<u8> {
    macro_rules! bee { ([$($f:tt $t:tt),*], _ => $else:expr) => {
        match c as char { $($f => b!($t),)* _ => $else } 
    }; }
    Some(bee!([
        '!''‼','"''╕','#''☻','$''¶','%''÷','&''·','\'''\'','(''╙',')''╜','*''↔','+''±',',''○',
        '-''¬','.''•','/''╧','0''╣','1''┘','2''│','3''├','4''╞','5''╟','6''╠','7''┤','8''╡',
        '9''╢',':''♪',';''♫','<''≤','=''≡','>''≥','?''¿','@''¡','A''α','B''β','C''¢','D''♀',
        'E''♂','F''ƒ','G''◄','H''►','I''↕','J''↨','K''└','L''∟','M''▬','N''■','O''◘','P''╨',
        'Q''Θ','R''♠','S''↓','T''♦','U''╩','V''√','W''↑','X''Φ','Y''¥','Z''φ','[''╓','\\''╤',
        ']''╖','^''→','_''⌐','`''┴','{''╘','|''┬','}''╛','~''≈'
    ], _ => return None))
}

pub fn escape_2c(c: [u8; 2]) -> Option<u8> {
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
        b"pr"=>b!('☺'), b"pl"=>b!('☻'), b"tp"=>b!('♦'), b"dm"=>b!('♣'), b"cx"=>b!('♥'),
        b"sn"=>b!('☼'), b"ss"=>b!('§'), b"sh"=>b!('▬'), b"bl"=>b!('⌐'), b"nl"=>b!('¤'),
        b"bu"=>b!('▲'), b"bd"=>b!('▼'), b"iq"=>b!('¿'), b"hf"=>b!('½'), b"db"=>b!('¼'),
        b"rl"=>b!('«'), b"rr"=>b!('»'), b"cl"=>b!('⌠'), b"fl"=>b!('⌡'), b"fd"=>b!('£'),
        b"hl"=>b!('▌'), b"hu"=>b!('▀'), b"hd"=>b!('▄'), b"hr"=>b!('▐'), b"pt"=>b!('₧'),
        b"is"=>b!('∩'), b"if"=>b!('∞'), b"dg"=>b!('°'), b"nm"=>b!('¨'), b"vr"=>b!('←'),
        b"vl"=>b!('╬'), b"wn"=>b!('╫'), b"cf"=>b!('┼'), b"s2"=>b!('╪'), b"am"=>b!('╔'),
        b"su"=>b!('╦'), b"sp"=>b!('╥'), b"mo"=>b!('┬'), b"et"=>b!('╒'), b"il"=>b!('█'),
        b"ov"=>b!('║'), b"dr"=>b!('╗'), b"in"=>b!('ε'), b"sq"=>b!('²'), b"ft"=>b!('ⁿ'),
        b"l2"=>b!('░'), b"l3"=>b!('▒'), b"l4"=>b!('▓'), b"c2"=>b!('▓'), b"pi"=>b!('π'), 
        b"qd"=>b!('⎕'), b"st"=>b!('─'), b"mt"=>b!('═'), b"tl"=>b!('┐'), b"tr"=>b!('┌'),
        // ↓ these will be changed probably
        b"x1"=>b!('╚'), b"x2"=>b!('╝'),
        _ => return None,
    })
}
