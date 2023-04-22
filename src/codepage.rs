use crate::prelude::*;

macro_rules! chars { ($func:ident $($other:tt)?) => { $func! { $($other)*
    '¤' '☺' '☻' '♥' '♦' '♣' '♠' '•' '◘' '○' '◙' '♂' '♀' '♪' '♫' '☼'
    '►' '◄' '↕' '‼' '¶' '§' '▬' '↨' '↑' '↓' '←' '→' '∟' '↔' '▲' '▼'
    ' ' '!' '"' '#' '$' '%' '&' '\'''(' ')' '*' '+' ',' '-' '.' '/'
    '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' ':' ';' '<' '=' '>' '?'
    '@' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O'
    'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' '[' '\\'']' '^' '_'
    '`' 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o'
    'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' '{' '|' '}' '~' '⌂'
    'Ç' 'ü' 'é' 'â' 'ä' 'à' 'å' 'ç' 'ê' 'ë' 'è' 'ï' 'î' 'ì' 'Ä' 'Å'
    'É' 'æ' 'Æ' 'ô' 'ö' 'ò' 'û' 'ù' 'ÿ' 'Ö' 'Ü' '¢' '£' '¥' '₧' 'ƒ'
    'á' 'í' 'ó' 'ú' 'ñ' 'Ñ' 'ª' 'º' '¿' '⌐' '¬' '½' '¼' '¡' '«' '»'
    '░' '▒' '▓' '│' '┤' '╡' '╢' '╖' '╕' '╣' '║' '╗' '╝' '╜' '╛' '┐'
    '└' '┴' '┬' '├' '─' '┼' '╞' '╟' '╚' '╔' '╩' '╦' '╠' '═' '╬' '╧'
    '╨' '╤' '╥' '╙' '╘' '╒' '╓' '╫' '╪' '┘' '┌' '█' '▄' '▌' '▐' '▀'
    'α' 'β' 'Γ' 'π' 'Σ' 'σ' 'μ' 'τ' 'Φ' 'Θ' 'Ω' 'δ' '∞' 'φ' 'ε' '∩'
    '≡' '±' '≥' '≤' '⌠' '⌡' '÷' '≈' '°' '¨' '·' '√' 'ⁿ' '²' '■' '⎕'
} } }

macro_rules! array { ($($x:tt)*) => { [$($x),*] }}

const CHARS: [char; 256] = chars!(array);

macro_rules! byte_maker { ([ $($n:tt),* ] $($c:tt)*) => {
    macro_rules! byte {
        $( ($c) => {$n} );*
    }
}}

chars!( byte_maker //lol
    [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255]
);

#[macro_export]
macro_rules! b {
    ($a:tt) => {byte!($a)};
    ($a:tt $($b:tt)+) => {byte!($a) $( | byte!($b))+};
}

pub fn tochar(x: u8) -> char { CHARS[x as usize] }
pub fn tochar_ln(x: u8) -> char { if x == 10 {'\n'} else {tochar(x)} }

pub fn tochars_ln(bytes: &[u8]) -> String { bytes.iter().map(|&x| tochar_ln(x)).collect() }
pub fn tochars   (bytes: &[u8]) -> String { bytes.iter().map(|&x| tochar   (x)).collect() }

pub const fn tobyte(x: char) -> Option<u8> {
    macro_rules! bee { ( $($c:tt)* ) => {
        match x {
            $($c => Some(b!($c)),)*
            _ => None,
        }
    } }
    Some(match x {
        '\0'..=' ' => x as u8,
        '−'|'–'=>b'-', '⎮'=>b!('│'), '⍺'=>b!('α'), 'ß'=>b!('β'), '∑'=>b!('Σ'), 'µ'=>b!('μ'),
        'Ω'=>b!('Ω'), 'ð'=>b!('δ'), 'ϕ'|'ɸ'=>b!('φ'), 'ɛ'=>b!('ε'), '∙'=>b!('¨'),
        _ => return chars!(bee)
    })
}

pub fn tobyte_wide(x: char) -> Result<u8, [u8; 4]> {
    tobyte(x).ok_or_else(|| {
        let [_, a, b, c] = u32::from(x).to_be_bytes();
        [b'\'', 0x80+a, b, c]
    })
}

pub fn tobyte_write(x: char, buf: &mut Vec<u8>) {
    match tobyte_wide(x) {
        Ok(a) => buf.push(a),
        Err(a) => buf.extend(a.into_iter()),
    }
}

pub fn tobstr(x: char) -> Bstr {
    match tobyte_wide(x) {
        Ok(a) => bstr![a],
        Err(a) => Bstr::from(&a[..]),
    }
}

pub fn tobytes(string: &str) -> Vec<u8> {
    let mut vector = Vec::with_capacity(string.len() / 2);
    for c in string.chars() {
        if let '\r' | '\u{200E}' | '\u{FE0E}' | '\u{FE0F}' = c { continue } // crlf moment
        tobyte_write(c, &mut vector);
    }
    vector
}
