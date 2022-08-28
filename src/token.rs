use crate::Bstr;
use smallvec::smallvec;

#[derive(Debug, Clone)]
pub enum Tok {
    Just(u8), Conj(u8), White(u8), Comment(Bstr),
    VarNoun(Bstr), VarVerb(Bstr), VarAv1(Bstr), VarAv2(Bstr), VarSet(Bstr), VarCng(Bstr),
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
    b!('♦''♣''♠''♂''♀''♫''►''◄''↕''‼''¶''§''▬''↨''↑''↓''←''∟''▲''▼'
       '!''#''$''%''&''*''+'',''-''/'';''<''=''>''@''\\''^''|''~')
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
        Some(b'\'') => if let Some(a) = step(bytes) {
            buf.push(do_escape(a, bytes).unwrap_or(a));
        } else { break },
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
        None => panic!(),
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
        Some(b'_') => match step(bytes).unwrap_or(b'_') {
            c @ short_verb!() => Tok::VarVerb(smallvec![b'_', c]),
            c => Tok::VarNoun(smallvec![b'_', c]),
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
            Some(b' ' | b'\n') | None => {
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
        Some(b!('→')) => Tok::VarSet (identifier(bytes)),
        Some(b!('↔')) => Tok::VarCng (identifier(bytes)),
        Some(b!('¨')) => Tok::Str    (identifier(bytes)),
        Some(c @ (b' ' | b'\n')) => Tok::White(c),
        Some(c @ short_verb!())  => Tok::VarVerb(smallvec![c]),
        Some(c @ short_av1!())   => Tok::VarAv1(smallvec![c]),
        Some(c @ short_av2!())   => Tok::VarAv2(smallvec![c]),
        Some(c @ b'A'..=b'Z')    => Tok::VarSet(smallvec![c + 32]),
        Some(c @ b!('☺''☻''⌂'))  => Tok::Conj(c),
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

pub fn rewrite(mut bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    while let Some(tok) = token(step(&mut bytes), &mut bytes) {
        match tok {
            Tok::Just(c) | Tok::Conj(c) | Tok::White(c) => out.push(c),
            Tok::Comment(s) => { out.extend(b"' "); out.extend(s.into_iter()); out.push(10); }
            Tok::VarNoun(s) => match s[..] {
                [c @ short_noun!()] => out.push(c),
                [b!('['), b!('α')] => out.push(b!('σ')),
                [b!('['), b!('β')] => out.push(b!('μ')),
                [b!('_'), c] => { out.push(b'_'); out.push(c) },
                ref s => { out.push(b'.'); out.extend(s) },
            },
            Tok::VarVerb(s) => match s[..] {
                [c @ short_verb!()] => out.push(c),
                ref s => { out.push(b':'); out.extend(s) },
            },
            Tok::VarAv1(s) => match s[..] {
                [c @ short_av1!()] => out.push(c),
                ref s => { out.push(b!('•')); out.extend(s) },
            },
            Tok::VarAv2(s) => match s[..] {
                [c @ short_av2!()] => out.push(c),
                ref s => { out.push(b!('○')); out.extend(s) },
            },
            Tok::VarSet(s) => match s[..] {
                [c @ b'a'..=b'z'] => out.push(c - 32),
                ref s => { out.push(b!('→')); out.extend(s) },
            },
            Tok::VarCng(s) => { out.push(b!('↔')); out.extend(s); },
            Tok::Chr(a) => { out.push(b'`'); out.push(a); },
            Tok::Chr2(a, b) => { out.push(b!('♥')); out.push(a); out.push(b); },
            Tok::Num2(a, b) => { out.push(b!('░')); out.push(a); out.push(b); },
            Tok::Num3(a, b, c) => { out.push(b!('▓')); out.push(a); out.push(b); out.push(c); },
            Tok::Num(n) => { out.push(b!('■')); out.extend(n); out.push(b!('■')); },
            Tok::HNum(n) => { out.push(b!('\'')); out.extend(n); },
            Tok::Str(s) => {
                out.push(b!('"'));
                for i in s { match i { 
                    b'\'' => out.extend(b"\'\'"),
                    i => out.push(i),
                }}
                out.push(b!('"'));
            },
        }
    }
    out
}

// !"#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`{|}~
// ‼╕☻¶÷⌐'╙╜↔±♪ •╧○♫≤≡≥¿¡αβ¢δɛƒ↑↓↕↨∟◄μ█◘╨Θ►στ╩√♠Φ¥ɸ╓╤╖→¬┴╘·╛≈
fn onechar_abbr(c: u8) -> Option<u8> { Some(match c {
    b'!' =>b!('‼'), b'"' =>b!('╕'), b'#' =>b!('☻'), b'$' =>b!('¶'), b'%' =>b!('÷'), b'&' =>b!('⌐'), 
    b'\''=>b!('\''),b'(' =>b!('╙'), b')' =>b!('╜'), b'*' =>b!('↔'), b'+' =>b!('±'), b',' =>b!('♪'), 
    b'.' =>b!('•'), b'/' =>b!('╧'), b':' =>b!('○'), b';' =>b!('♫'), b'<' =>b!('≤'), b'=' =>b!('≡'), 
    b'>' =>b!('≥'), b'?' =>b!('¿'), b'@' =>b!('¡'), b'A' =>b!('α'), b'B' =>b!('β'), b'C' =>b!('¢'), 
    b'D' =>b!('δ'), b'E' =>b!('ε'), b'F' =>b!('ƒ'), b'G' =>b!('↑'), b'H' =>b!('↓'), b'I' =>b!('↕'), 
    b'J' =>b!('↨'), b'K' =>b!('∟'), b'L' =>b!('╜'), b'M' =>b!('μ'), b'N' =>b!('█'), b'O' =>b!('◘'), 
    b'P' =>b!('╨'), b'Q' =>b!('Θ'), b'R' =>b!('╙'), b'S' =>b!('σ'), b'T' =>b!('τ'), b'U' =>b!('╩'), 
    b'V' =>b!('√'), b'W' =>b!('♠'), b'X' =>b!('Φ'), b'Y' =>b!('¥'), b'Z' =>b!('ϕ'), b'[' =>b!('╓'),
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
        b"aO"=>b!('Å'), b"ao"=>b!('å'), b"aE"=>b!('Æ'), b"ae"=>b!('æ'),
        b"a,"=>b!('ª'), b"o,"=>b!('º'), b"z,"=>b!('²'), b"n,"=>b!('ⁿ'),
        b"pr"=>b!('☺'), b"pl"=>b!('☻'), b"tp"=>b!('♦'), b"c2"=>b!('♥'), b"dm"=>b!('♣'),
        b"dl"=>b!('♂'), b"dr"=>b!('♀'), b"sn"=>b!('☼'), b"ss"=>b!('§'), b"sh"=>b!('▬'),
        b"bu"=>b!('▲'), b"bd"=>b!('▼'), b"iq"=>b!('¿'), b"hf"=>b!('½'), b"db"=>b!('¼'),
        b"sl"=>b!('«'), b"sr"=>b!('»'), b"cl"=>b!('⌠'), b"fl"=>b!('⌡'), b"gb"=>b!('£'),
        b"hl"=>b!('▌'), b"hu"=>b!('▀'), b"hd"=>b!('▄'), b"hr"=>b!('▐'), b"pt"=>b!('₧'),
        b"is"=>b!('∩'), b"if"=>b!('∞'), b"dg"=>b!('°'), b"nm"=>b!('¨'), b"vr"=>b!('←'),
        b"w2"=>b!('┐'), b"m2"=>b!('┌'), b"w3"=>b!('┤'), b"m3"=>b!('├'),
        b"w4"=>b!('╡'), b"m4"=>b!('╞'), b"w5"=>b!('╢'), b"m5"=>b!('╟'),
        b"w6"=>b!('╣'), b"m6"=>b!('╠'), b"wf"=>b!('┘'), b"mf"=>b!('└'),
        // ↓ most of these will be changed probably
        b"vl"=>b!('╬'), b"wn"=>b!('╫'), b"sc"=>b!('┼'), b"s2"=>b!('╪'),
        b"su"=>b!('╦'), b"sp"=>b!('╥'), b"mo"=>b!('┬'), b"co"=>b!('╒'), b"vb"=>b!('│'),
        b"ov"=>b!('╝'), b"dp"=>b!('╗'),
        b"x1"=>b!('─'), b"x2"=>b!('═'), b"x3"=>b!('╚'), b"x4"=>b!('╔'),
        _ => return None,
    })
}
