use crate::codepage;
use crate::prelude::*;
use crate::c64;

#[derive(Debug, Clone)]
pub enum Tok {
    Just(u8), White(u8), Comment(Bstr),
    VNoun(Bstr), VVerb(Bstr), VAv1(Bstr), VAv2(Vec<Bstr>, Bstr),
    VSet(Bstr), VMut(Bstr), VSetS(Bstr), VMutS(Bstr),
    Chr(u8), Chr2(u8, u8), Num(i64), Flt(c64), Str(Vec<i64>)
}
use Tok::*;

pub trait TokenInput<'a> {
    fn step(&mut self) -> Option<u8>;
    fn step_or(&mut self, default: u8) -> u8 { self.step().unwrap_or(default) }
    fn peek(&self) -> Option<u8>;
    fn peek_or(&self, default: u8) -> u8 { self.peek().unwrap_or(default) }
    fn escape_step(&mut self) -> EscapableU8 {
        let a = self.step();
        if a != Some(b'\'') { return a.map_or(EscapableU8::None, Bare) };
        let b = self.step_or(0);
        if let Some(c) = escape_1c(b) {
            Bare(c)
        } else if let Some(c) = escape_2c([b, self.peek_or(0)]) {
            self.step(); Bare(c)
        } else if let 0x80..=0x91 = b {
            let char = char::from_u32(u32::from_be_bytes(
                [0, b - 0x80, self.step_or(0), self.step_or(0)]
            )).unwrap_or('�');
            match codepage::tobyte(char) {
                Some(c) => Bare(c),
                None => Unicode(char),
            }
        } else {
            Quoted(b)
        }
    }
}

pub enum EscapableU8 { None, Bare(u8), Quoted(u8), Unicode(char) }
use EscapableU8::{Bare, Quoted, Unicode};

struct FromByteSlice<'a>(&'a [u8]);
impl<'a> TokenInput<'a> for FromByteSlice<'a> {
    fn step(&mut self) -> Option<u8> {
        let a = self.0.first().copied();
        if a.is_some() {self.0 = &self.0[1..]};
        a
    }
    fn peek(&self) -> Option<u8> {
        self.0.first().copied()
    }
}

struct FromString<'a>(&'a str, u8);
impl<'a> TokenInput<'a> for FromString<'a> {
    fn step(&mut self) -> Option<u8> {
        let a = self.0.chars().next()?;
        if let Some(b) = codepage::tobyte(a) {
            self.0 = &self.0[a.len_utf8()..];
            Some(b)
        } else {
            let c = u32::from(a).to_be_bytes()[self.1 as usize] + [b'\'', 0x80, 0, 0][self.1 as usize];
            self.1 += 1;
            if self.1 == 4 { self.0 = &self.0[a.len_utf8()..]; self.1 = 0; }
            Some(c)
        }
    }
    fn peek(&self) -> Option<u8> {
        let a = self.0.chars().next()?;
        Some(codepage::tobyte(a).unwrap_or_else(||
            u32::from(a).to_be_bytes()[self.1 as usize] + [b'\'', 0x80, 0, 0][self.1 as usize]))
    }
}

struct Rewrite<'a> {
    pub inner: &'a mut dyn TokenInput<'a>,
    pub buf: String
}
impl<'a> Rewrite<'a> {
    fn new<T: TokenInput<'a>>(inner: &'a mut T) -> Rewrite<'a> {
        Rewrite { inner, buf: String::new() }
    }
}

impl<'a> TokenInput<'a> for Rewrite<'a> {
    fn step(&mut self) -> Option<u8> {
        let a = self.inner.step();
        if let Some(c) = a { self.buf.push(codepage::tochar(c)) }
        a
    }
    fn peek(&self) -> Option<u8> { self.inner.peek() }
    fn escape_step(&mut self) -> EscapableU8 {
        let a = self.inner.escape_step();
        match a {
            Bare(c) => self.buf.push(codepage::tochar_ln(c)),
            Quoted(c) => {
                self.buf.push('\'');
                self.buf.push(codepage::tochar(c));
            },
            Unicode(c) => self.buf.push(c),
            EscapableU8::None => ()
        }
        a
    }
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
macro_rules! c { ($a:tt $($b:tt)+) => { $a $( | $b )+}; }
macro_rules! char_noun { () => { c!(
    'ø''œ''þ''ð''ı''ĳ''ĸ''ŋ''ł''ŧ''ſ''ɐ''ɑ''ɒ''ɓ''ɔ''ɕ''ɖ''ɗ''ɘ''ə''ɚ''ɛ''ɜ''ɝ''ɞ''ɟ''ɠ''ɡ''ɢ''ɣ''ɤ'
    'ɥ''ɦ''ɧ''ɨ''ɩ''ɪ''ɫ''ɬ''ɭ''ɮ''ɯ''ɰ''ɱ''ɲ''ɳ''ɴ''ɵ''ɶ''ɷ''ɸ''ɹ''ɺ''ɻ''ɼ''ɽ''ɾ''ɿ''ʀ''ʁ''ʂ''ʃ''ʄ'
    'ʅ''ʆ''ʇ''ʈ''ʉ''ʊ''ʋ''ʌ''ʍ''ʎ''ʏ''ʐ''ʑ''ʒ''ʓ''ʔ''ʕ''ʖ''ʗ''ʘ''ʙ''ʚ''ʛ''ʜ''ʝ''ʞ''ʟ''ʠ''ʡ''ʢ''ʣ''ʤ'
    'ʥ''ʦ''ʧ''ʨ''ʩ''ʪ''ʫ''ʬ''ʭ''ʮ''ʯ''ƀ''ƃ''ƅ''ƈ''ƌ''ƍ''ƕ''ƙ''ƚ''ƛ''ƞ''ƣ''ƥ''ƨ''ƪ''ƭ''ƴ''ƶ''ƹ''ƺ''ƽ'
    'ƾ''ƿ''ǉ''ǌ''ǝ''ǳ''ȝ''ȡ''ȴ''ȵ''ȶ''ȷ''ȸ''ȹ''ȣ''ȥ''ȼ''ȿ''ɀ''ɂ''ɇ''ɉ''ɋ''ɍ''ɏ''γ''ζ''η''θ''ι''κ''λ'
    'ν''ξ''ο''ρ''ς''υ''χ''ψ''ω''ϐ''ϑ''ϕ''ϖ''ϗ''ϙ''ϛ''ϝ''ϟ''ϡ''ͻ''ͼ''ͽ''ͱ''ͳ''ͷ''ϰ''ϱ''ϲ''ϳ''ϵ''ϸ''ϻ'
    'ϼ''Ϗ''а''б''в''г''д''е''ж''з''и''к''л''м''н''о''п''р''с''т''у''ф''х''ц''ч''ш''щ''ъ''ы''ь''э''ю'
    'я''ђ''є''ѕ''і''ј''љ''њ''ћ''џ''ѡ''ѣ''ѥ''ѧ''ѩ''ѫ''ѭ''ѯ''ѱ''ѳ''ѵ''ѹ''ѻ''ҁ''ҍ''ҏ'
)}}
macro_rules! char_av1 { () => {
    c!('┍''┑''┕''┙''┝''┥''┭''┮''┯''┵''┶''┷''┽''┾''┿''╎''┆''┊''╭''╮''╯''╰''╱''╲''╳''╴''╵''╶''╷''╸''╺')
    | '𝐀'..='𝐙' | '𝐚'..='𝐳' | '𝟎'..='𝟗'
}}
macro_rules! char_av2 { () => {
    c!('┎''┒''┖''┚''┠''┨''┰''┱''┲''┸''┹''┺''╉''╊''╂''╏''┇''┋''┏''┓''┗''┛''┣''┫''┳''┻''╋''╹''╻'
       '𝔸''𝔹''ℂ''𝔻''𝔼''𝔽''𝔾''ℍ''𝕀''𝕁''𝕂''𝕃''𝕄''ℕ''𝕆''ℙ''ℚ''ℝ''𝕊''𝕋''𝕌''𝕍''𝕎''𝕏''𝕐''ℤ')
    | '𝕒'..='𝕫' | '𝟘'..='𝟡' | c!('ℼ''ℽ''ℾ''ℿ''⅀''ⅅ''ⅆ''ⅇ''ⅈ''ⅉ''⟦''⟧''⟬''⟭')
}}


fn bstring<'a, T: TokenInput<'a>>(t: &mut T) -> Bstr {
    let mut buf = Bstr::new();
    loop { match t.escape_step() {
        Bare(b'"') | EscapableU8::None => break,
        Bare(b!('¨')) => buf.push(b'"'),
        Bare(b!('·')) => buf.push(b'\''),
        Bare(c) | Quoted(c) => buf.push(c),
        Unicode(c) => buf.extend(c.to_string().bytes()),
    }}
    buf
}

fn string<'a, T: TokenInput<'a>>(t: &mut T) -> Vec<i64> {
    let mut buf = Vec::new();
    loop { match t.escape_step() {
        Bare(b'"') | EscapableU8::None => break,
        Bare(b!('¨')) => buf.push('"' as i64),
        Bare(b!('·')) => buf.push('\'' as i64),
        Bare(c) | Quoted(c) => buf.push(i64::from(c)),
        Unicode(c) => buf.push(i64::from(u32::from(c))),
    }}
    buf
}

fn ident<'a, T: TokenInput<'a>>(t: &mut T) -> Bstr {
    match t.escape_step() {
        Bare(first @ b'a'..=b'z') => {
            let mut buf = bstr![first];
            loop { match t.peek() {
                Some(ltr @ b'a'..=b'z') => { t.step(); buf.push(ltr); }
                Some(ltr @ b'A'..=b'Z') => { t.step(); buf.push(ltr + 32); break; }
                _ => break,
            }}
        buf }
        Bare(b'_') => bstr![b'_', t.step_or(b'_')],
        Bare(b'"') => bstring(t),
        Bare(c @ b!('←''→')) => [c].into_iter().chain(ident(t)).collect(),
        Bare(chr) | Quoted(chr) => bstr![chr],
        Unicode(c) => codepage::tobstr(c),
        EscapableU8::None => bstr![],
    }
}

fn byte_lit<'a, T: TokenInput<'a>>(t: &mut T, n: u8) -> Tok {
    let f = t.step_or(0);
    let mut num = i64::from(f) - 256 * i64::from(f > 127);
    for _ in 1..n {
        num = (num << 8) + i64::from(t.step_or(0));
    }
    Num(num)
}

fn hnum<'a, T: TokenInput<'a>>(t: &mut T, first: u8, is_neg: bool) -> Tok {
    let b: i64 = match t.peek_or(0) {
        b'x' => 16, b'b' =>  2, b'o' => 8,
        b's' => 6,  b'z' => 12, b'n' => 36,
        _ => 0
    };
    if first == b'0' && b != 0 {
        let mut num = 0i64;
        loop {
            t.step();
            let a = match t.peek() {
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
        let Some(x) = t.peek().map(char::from) else { break };
        if x.is_ascii_digit()
        || x == '.' && !buf.contains('e') && !buf.contains('.')
        || x == 'e' && !buf.contains('e') //&& matches!(t.get(1), Some(b'0'..=b'9'|b'-'))
        || x == '-' && buf.bytes().last() == Some(b'e') {
            buf.push(x); t.step();
        } else { break }
    }
    if buf.ends_with('-') { buf.pop(); }
    if buf.ends_with('e') { buf.pop(); }
    buf.parse::<i64>().map_or_else(
        |_| Flt(c64::new(buf.parse::<f64>().unwrap() * if is_neg {-1.} else {1.}, 0.)),
        |x| Num(x * if is_neg {-1} else {1}),
    )
}

fn token<'a, T: TokenInput<'a>>(t: &mut T) -> Option<Tok> {
    Some(match t.escape_step() {
        Bare(b'"') => Str(string(t)),
        Bare(b'`') => Chr(t.step_or(0x20)),
        Bare(b'_') => {
            let c = match t.escape_step() {
                Bare(c) | Quoted(c) => c,
                Unicode(x) => {
                    let mut buf = Vec::with_capacity(5);
                    buf.push(b'_');
                    codepage::tobyte_write(x, &mut buf);
                    return Some(match x {
                        char_noun!()=> VNoun(buf.into()),
                        char_av1!() => VAv1(buf.into()),
                        char_av2!() => VAv2(vec![], buf.into()),
                        _ => VVerb(buf.into()),
                    })
                },
                EscapableU8::None => b'_',
            };
            match c {
                c @ short_verb!() => VVerb(bstr![b'_', c]),
                c @ short_av1!()  => VAv1(bstr![b'_', c]),
                c @ short_av2!()  => VAv2(vec![], bstr![b'_', c]),
                c => VNoun(bstr![b'_', c]),
            }
        }
        Bare  (b!('░')) | Quoted(b!('│')) => byte_lit(t, 2),
        Bare  (b!('▒')) | Quoted(b!('├')) => byte_lit(t, 3),
        Quoted(b!('╞')) => byte_lit(t, 4),
        Quoted(b!('╟')) => byte_lit(t, 5),
        Quoted(b!('╠')) => byte_lit(t, 6),
        Quoted(b!('┤')) => byte_lit(t, 7),
        Quoted(b!('╡')) => byte_lit(t, 8),
        Bare(b!('▓')) => Chr2(t.step_or(0), t.step_or(0)),
        Bare(b!('█')) => {
            let mut buf = Vec::new();
            loop { match t.escape_step() {
                Bare(b!('█')) | EscapableU8::None => break,
                Bare(b!('¨')) => buf.push('"' as i64),
                Bare(b!('·')) => buf.push('\'' as i64),
                Bare(c) => buf.push(codepage::tochar_ln(c) as i64),
                Quoted(c) => { buf.push('"' as i64); buf.push(codepage::tochar_ln(c) as i64); }
                Unicode(c) => buf.push(c as i64),
            }}
            Str(buf)
        }
        Bare(b!('.')) => VNoun(ident(t)),
        Bare(b!('•')) => VAv1 (ident(t)), Bare(b!('○')) => VAv2 (vec![], ident(t)),
        Bare(b!('→')) => VSetS(ident(t)), Bare(b!('↔')) => VMutS(ident(t)),
        Bare(b!('─')) => VSet (ident(t)), Bare(b!('═')) => VMut (ident(t)),
        Bare(b!('¨')) => Str(ident(t).into_iter().map(i64::from).collect()),
        Bare(b!(':')) =>
            if let Some(first @ (b'0'..=b'9' | b'-')) = t.peek() {
                t.step();
                if first != b'-' {
                    hnum(t, first, false)
                } else if let Some(first @ (b'0'..=b'9')) = t.peek() {
                    t.step();
                    hnum(t, first, true)
                } else {
                    VVerb(bstr![b'-'])
                }
            } else { VVerb(ident(t)) },
        Bare(c @ (b' ' | b'\n')) => White(c),
        Bare(c @ (short_verb!() | b'A'..=b'Z')) => VVerb(bstr![c]),
        Bare(c @ short_av1! ()) => VAv1 (bstr![c]),
        Bare(c @ short_av2! ()) => VAv2 (vec![], bstr![c]),
        Bare(c @ short_noun!()) => VNoun(bstr![c]),
        Bare(b!('σ')) => VNoun(bstr![b!('→'), b!('α')]),
        Bare(b!('μ')) => VNoun(bstr![b!('→'), b!('β')]),
        Quoted(b' ') => {
            let mut buf = Bstr::new();
            loop { match t.step() {
                Some(b'\n') | None => break,
                Some(a) => buf.push(a)
            }}
            Comment(buf)
        }
        Unicode(x) => {
            let chars = codepage::tobstr(x);
            match x {
                char_noun!() => VNoun(chars),
                char_av1!()  => VAv1(chars),
                char_av2!()  => VAv2(vec![], chars),
                _ => VVerb(chars),
            }
        }
        Bare(x) | Quoted(x) => Just(x),
        EscapableU8::None => return None,
    })
}

pub fn tokenize_bytes(t: &[u8]) -> Vec<Tok> { tokenize(&mut FromByteSlice(t)) }
pub fn tokenize_str(t: &str)    -> Vec<Tok> { tokenize(&mut FromString(t, 0)) }

fn tokenize<'a, T: TokenInput<'a>>(t: &mut T) -> Vec<Tok> {
    let mut toks = Vec::new();
    while let Some(mut tok) = token(t) {
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

pub fn rewrite(t: &[u8]) -> String {
    let mut t = FromByteSlice(t);
    let mut t = Rewrite::new(&mut t);
    while token(&mut t).is_some() {}
    t.buf
}

macro_rules! matchb { ( $in:expr ; {$($f:tt $t:tt),*}, _ => $else:expr) => {
    match $in { $($f => b!($t),)* _ => $else } 
}; }

// !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`{|}~
// ‼╕☻¶÷·'╙╜↔±○¬•╧╣┌│├╞╟╠┤╡╢♪♫≤≡≥¿¡αβ¢♀♂ƒ◄►↕↨└∟▬■◘╨Θ♠↓♦╩√↑Φ¥φ╓╤╖→⌐┴╘♥╛≈
pub fn escape_1c(c: u8) -> Option<u8> {
    Some(matchb!(c as char; {
        '!''‼','"''╕','#''☻','$''¶','%''÷','&''·','\'''\'','(''╙',')''╜','*''↔','+''±',',''○',
        '-''¬','.''•','/''╧','0''╣','1''┘','2''│','3''├','4''╞','5''╟','6''╠','7''┤','8''╡',
        '9''╢',':''♪',';''♫','<''≤','=''≡','>''≥','?''¿','@''¡','A''α','B''β','C''¢','D''♀',
        'E''♂','F''ƒ','G''◄','H''►','I''↕','J''↨','K''└','L''∟','M''▬','N''■','O''◘','P''╨',
        'Q''Θ','R''♠','S''↓','T''♦','U''╩','V''√','W''↑','X''Φ','Y''¥','Z''φ','[''╓','\\''╤',
        ']''╖','^''→','_''⌐','`''┴','{''╘','|''┬','}''╛','~''≈'
    }, _ => return None))
}

pub fn escape_2c(c: [u8; 2]) -> Option<u8> {
    Some(matchb!(&c[..]; {
        b"a'"'á', b"a`"'à', b"a\""'ä', b"a^"'â', b"a+"'Ä', b"a-"'α', b"a,"'ª',
        b"e'"'é', b"e`"'è', b"e\""'ë', b"e^"'ê', b"e+"'É', b"e-"'ε',
        b"i'"'í', b"i`"'ì', b"i\""'ï', b"i^"'î',
        b"o'"'ó', b"o`"'ò', b"o\""'ö', b"o^"'ô', b"o+"'Ö', b"o,"'º',
        b"u'"'ú', b"u`"'ù', b"u\""'ü', b"u^"'û', b"u+"'Ü',
        b"c+"'Ç', b"c,"'ç', b"y\""'ÿ', b"n~"'ñ', b"n+"'Ñ', b"n-"'ñ',
        b"aO"'Å', b"ao"'å', b"aE"'Æ', b"ae"'æ', b"pt"'₧', b"ss"'§', 
        b"s+"'Σ', b"t+"'Θ', b"f+"'Φ', b"w+"'Ω', b"g+"'Γ',
        b"s-"'σ', b"t-"'τ', b"f-"'φ', b"p-"'π', b"b-"'β', b"m-"'μ', b"d-"'δ',
        b"z,"'²', b"n,"'ⁿ', b"f,"'ƒ',
        b"pr"'☺', b"pl"'☻', b"tp"'♦', b"dm"'♣', b"cx"'♥', b"ex"'⌂', b"sh"'▬',
        b"hl"'▌', b"hr"'▐', b"hu"'▀', b"hd"'▄',
        b"rl"'«', b"rr"'»', b"bu"'▲', b"bd"'▼',
        b"l2"'░', b"l3"'▒', b"c2"'▓', b"us"'█',
        b"iq"'¿', b"ie"'¡', b"cl"'⌠', b"fl"'⌡', b"sn"'☼', b"bl"'⌐', b"nl"'¤', b"fd"'£',
        b"hf"'½', b"db"'¼', b"sq"'²', b"pi"'π', b"in"'ε', b"ft"'ⁿ',
        b"is"'∩', b"if"'∞', b"dg"'°', b"nm"'¨', b"vr"'←',
        b"mo"'┬', b"fp"'┼', b"s2"'╪', b"et"'╒', b"tl"'┐', b"tr"'┌',
        b"vl"'╬', b"wn"'╫', b"am"'╔', b"su"'╦', b"sp"'╥', b"ov"'║', b"dr"'╗',
        b"st"'─', b"mt"'═', 
        b"x1"'╚', b"x2"'╝' // ← these will be changed probably
    }, _ => return None))
}
