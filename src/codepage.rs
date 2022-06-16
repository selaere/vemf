//! im sorry
use crate::Bstr;

const CHARS: [char; 256] = [
    '⍁', '☺', '☻', '♥', '♦', '♣', '♠', '•', '◘', '○', '◙', '♂', '♀', '♪', '♫', '☼',
    '►', '◄', '↕', '‼', '¶', '§', '▬', '↨', '↑', '↓', '←', '→', '∟', '↔', '▲', '▼',
    ' ', '!', '"', '#', '$', '%', '&', '\'','(', ')', '*', '+', ',', '-', '.', '/',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
    '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\',']', '^', '_',
    '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', '⌂',
    'Ç', 'ü', 'é', 'â', 'ä', 'à', 'å', 'ç', 'ê', 'ë', 'è', 'ï', 'î', 'ì', 'Ä', 'Å',
    'É', 'æ', 'Æ', 'ô', 'ö', 'ò', 'û', 'ù', 'ÿ', 'Ö', 'Ü', '¢', '£', '¥', '₧', 'ƒ',
    'á', 'í', 'ó', 'ú', 'ñ', 'Ñ', 'ª', 'º', '¿', '⌐', '¬', '½', '¼', '¡', '«', '»',
    '░', '▒', '▓', '│', '┤', '╡', '╢', '╖', '╕', '╣', '║', '╗', '╝', '╜', '╛', '┐',
    '└', '┴', '┬', '├', '─', '┼', '╞', '╟', '╚', '╔', '╩', '╦', '╠', '═', '╬', '╧',
    '╨', '╤', '╥', '╙', '╘', '╒', '╓', '╫', '╪', '┘', '┌', '█', '▄', '▌', '▐', '▀',
    'α', 'β', 'Γ', 'π', 'Σ', 'σ', 'µ', 'τ', 'Φ', 'Θ', 'Ω', 'δ', '∞', 'ϕ', 'ε', '∩',
    '≡', '±', '≥', '≤', '⌠', '⌡', '÷', '≈', '°', '¨', '·', '√', 'ⁿ', '²', '■', '⎕',
];

pub fn tochar(x: u8) -> char {
    CHARS[x as usize]
}

pub fn tochars(bytes: &[u8]) -> String {
    bytes.iter().map(|&x| tochar(x)).collect()
}
#[macro_export]
macro_rules! b {
('⍁')=>{0x00};('☺')=>{0x01};('☻')=>{0x02};('♥')=>{0x03};('♦')=>{0x04};('♣')=>{0x05};('♠')=>{0x06};('•')=>{0x07};
('◘')=>{0x08};('○')=>{0x09};('◙')=>{0x0A};('♂')=>{0x0B};('♀')=>{0x0C};('♪')=>{0x0D};('♫')=>{0x0E};('☼')=>{0x0F};
('►')=>{0x10};('◄')=>{0x11};('↕')=>{0x12};('‼')=>{0x13};('¶')=>{0x14};('§')=>{0x15};('▬')=>{0x16};('↨')=>{0x17};
('↑')=>{0x18};('↓')=>{0x19};('←')=>{0x1A};('→')=>{0x1B};('∟')=>{0x1C};('↔')=>{0x1D};('▲')=>{0x1E};('▼')=>{0x1F};
(' ')=>{b' '};('!')=>{b'!'};('"')=>{b'"'};('#')=>{b'#'};('$')=>{b'$'};('%')=>{b'%'};('&')=>{b'&'};('\'')=>{b'\''};
('(')=>{b'('};(')')=>{b')'};('*')=>{b'*'};('+')=>{b'+'};(',')=>{b','};('-')=>{b'-'};('.')=>{b'.'};('/')=>{b'/'};
('0')=>{b'0'};('1')=>{b'1'};('2')=>{b'2'};('3')=>{b'3'};('4')=>{b'4'};('5')=>{b'5'};('6')=>{b'6'};('7')=>{b'7'};
('8')=>{b'8'};('9')=>{b'9'};(':')=>{b':'};(';')=>{b';'};('<')=>{b'<'};('=')=>{b'='};('>')=>{b'>'};('?')=>{b'?'};
('@')=>{b'@'};('A')=>{b'A'};('B')=>{b'B'};('C')=>{b'C'};('D')=>{b'D'};('E')=>{b'E'};('F')=>{b'F'};('G')=>{b'G'};
('H')=>{b'H'};('I')=>{b'I'};('J')=>{b'J'};('K')=>{b'K'};('L')=>{b'L'};('M')=>{b'M'};('N')=>{b'N'};('O')=>{b'O'};
('P')=>{b'P'};('Q')=>{b'Q'};('R')=>{b'R'};('S')=>{b'S'};('T')=>{b'T'};('U')=>{b'U'};('V')=>{b'V'};('W')=>{b'W'};
('X')=>{b'X'};('Y')=>{b'Y'};('Z')=>{b'Z'};('[')=>{b'['};('\\')=>{b'\\'};(']')=>{b']'};('^')=>{b'^'};('_')=>{b'_'};
('`')=>{b'`'};('a')=>{b'a'};('b')=>{b'b'};('c')=>{b'c'};('d')=>{b'd'};('e')=>{b'e'};('f')=>{b'f'};('g')=>{b'g'};
('h')=>{b'h'};('i')=>{b'i'};('j')=>{b'j'};('k')=>{b'k'};('l')=>{b'l'};('m')=>{b'm'};('n')=>{b'n'};('o')=>{b'o'};
('p')=>{b'p'};('q')=>{b'q'};('r')=>{b'r'};('s')=>{b's'};('t')=>{b't'};('u')=>{b'u'};('v')=>{b'v'};('w')=>{b'w'};
('x')=>{b'x'};('y')=>{b'y'};('z')=>{b'z'};('{')=>{b'{'};('|')=>{b'|'};('}')=>{b'}'};('~')=>{b'~'};('⌂')=>{0x7F};
('Ç')=>{0x80};('ü')=>{0x81};('é')=>{0x82};('â')=>{0x83};('ä')=>{0x84};('à')=>{0x85};('å')=>{0x86};('ç')=>{0x87};
('ê')=>{0x88};('ë')=>{0x89};('è')=>{0x8A};('ï')=>{0x8B};('î')=>{0x8C};('ì')=>{0x8D};('Ä')=>{0x8E};('Å')=>{0x8F};
('É')=>{0x90};('æ')=>{0x91};('Æ')=>{0x92};('ô')=>{0x93};('ö')=>{0x94};('ò')=>{0x95};('û')=>{0x96};('ù')=>{0x97};
('ÿ')=>{0x98};('Ö')=>{0x99};('Ü')=>{0x9A};('¢')=>{0x9B};('£')=>{0x9C};('¥')=>{0x9D};('₧')=>{0x9E};('ƒ')=>{0x9F};
('á')=>{0xA0};('í')=>{0xA1};('ó')=>{0xA2};('ú')=>{0xA3};('ñ')=>{0xA4};('Ñ')=>{0xA5};('ª')=>{0xA6};('º')=>{0xA7};
('¿')=>{0xA8};('⌐')=>{0xA9};('¬')=>{0xAA};('½')=>{0xAB};('¼')=>{0xAC};('¡')=>{0xAD};('«')=>{0xAE};('»')=>{0xAF};
('░')=>{0xB0};('▒')=>{0xB1};('▓')=>{0xB2};('│')=>{0xB3};('┤')=>{0xB4};('╡')=>{0xB5};('╢')=>{0xB6};('╖')=>{0xB7};
('╕')=>{0xB8};('╣')=>{0xB9};('║')=>{0xBA};('╗')=>{0xBB};('╝')=>{0xBC};('╜')=>{0xBD};('╛')=>{0xBE};('┐')=>{0xBF};
('└')=>{0xC0};('┴')=>{0xC1};('┬')=>{0xC2};('├')=>{0xC3};('─')=>{0xC4};('┼')=>{0xC5};('╞')=>{0xC6};('╟')=>{0xC7};
('╚')=>{0xC8};('╔')=>{0xC9};('╩')=>{0xCA};('╦')=>{0xCB};('╠')=>{0xCC};('═')=>{0xCD};('╬')=>{0xCE};('╧')=>{0xCF};
('╨')=>{0xD0};('╤')=>{0xD1};('╥')=>{0xD2};('╙')=>{0xD3};('╘')=>{0xD4};('╒')=>{0xD5};('╓')=>{0xD6};('╫')=>{0xD7};
('╪')=>{0xD8};('┘')=>{0xD9};('┌')=>{0xDA};('█')=>{0xDB};('▄')=>{0xDC};('▌')=>{0xDD};('▐')=>{0xDE};('▀')=>{0xDF};
('α')=>{0xE0};('β')=>{0xE1};('Γ')=>{0xE2};('π')=>{0xE3};('Σ')=>{0xE4};('σ')=>{0xE5};('μ')=>{0xE6};('τ')=>{0xE7};
('Φ')=>{0xE8};('Θ')=>{0xE9};('Ω')=>{0xEA};('δ')=>{0xEB};('∞')=>{0xEC};('ϕ')=>{0xED};('ε')=>{0xEE};('∩')=>{0xEF};
('≡')=>{0xF0};('±')=>{0xF1};('≥')=>{0xF2};('≤')=>{0xF3};('⌠')=>{0xF4};('⌡')=>{0xF5};('÷')=>{0xF6};('≈')=>{0xF7};
('°')=>{0xF8};('¨')=>{0xF9};('·')=>{0xFA};('√')=>{0xFB};('ⁿ')=>{0xFC};('²')=>{0xFD};('■')=>{0xFE};('⎕')=>{0xFF};
($a:tt $($b:tt)+) => {$crate::b!($a) $( | $crate::b!($b))+};
}

pub const fn tobyte(x: char) -> u8 {
    match x {
        '⍁'=>b!('⍁'), '☺'=>b!('☺'), '☻'=>b!('☻'), '♥'=>b!('♥'), '♦'=>b!('♦'), '♣'=>b!('♣'), '♠'=>b!('♠'), '•'=>b!('•'),
        '◘'=>b!('◘'), '○'=>b!('○'), '◙'=>b!('◙'), '♂'=>b!('♂'), '♀'=>b!('♀'), '♪'=>b!('♪'), '♫'=>b!('♫'), '☼'=>b!('☼'),
        '►'=>b!('►'), '◄'=>b!('◄'), '↕'=>b!('↕'), '‼'=>b!('‼'), '¶'=>b!('¶'), '§'=>b!('§'), '▬'=>b!('▬'), '↨'=>b!('↨'),
        '↑'=>b!('↑'), '↓'=>b!('↓'), '←'=>b!('←'), '→'=>b!('→'), '∟'=>b!('∟'), '↔'=>b!('↔'), '▲'=>b!('▲'), '▼'=>b!('▼'),
        '\0'..='~' => x as u8,                                                                            '⌂'=>b!('⌂'),
        'Ç'=>b!('Ç'), 'ü'=>b!('ü'), 'é'=>b!('é'), 'â'=>b!('â'), 'ä'=>b!('ä'), 'à'=>b!('à'), 'å'=>b!('å'), 'ç'=>b!('ç'),
        'ê'=>b!('ê'), 'ë'=>b!('ë'), 'è'=>b!('è'), 'ï'=>b!('ï'), 'î'=>b!('î'), 'ì'=>b!('ì'), 'Ä'=>b!('Ä'), 'Å'=>b!('Å'),
        'É'=>b!('É'), 'æ'=>b!('æ'), 'Æ'=>b!('Æ'), 'ô'=>b!('ô'), 'ö'=>b!('ö'), 'ò'=>b!('ò'), 'û'=>b!('û'), 'ù'=>b!('ù'),
        'ÿ'=>b!('ÿ'), 'Ö'=>b!('Ö'), 'Ü'=>b!('Ü'), '¢'=>b!('¢'), '£'=>b!('£'), '¥'=>b!('¥'), '₧'=>b!('₧'), 'ƒ'=>b!('ƒ'),
        'á'=>b!('á'), 'í'=>b!('í'), 'ó'=>b!('ó'), 'ú'=>b!('ú'), 'ñ'=>b!('ñ'), 'Ñ'=>b!('Ñ'), 'ª'=>b!('ª'), 'º'=>b!('º'),
        '¿'=>b!('¿'), '⌐'=>b!('⌐'), '¬'=>b!('¬'), '½'=>b!('½'), '¼'=>b!('¼'), '¡'=>b!('¡'), '«'=>b!('«'), '»'=>b!('»'),
        '░'=>b!('░'), '▒'=>b!('▒'), '▓'=>b!('▓'), '│'=>b!('│'), '┤'=>b!('┤'), '╡'=>b!('╡'), '╢'=>b!('╢'), '╖'=>b!('╖'),
        '╕'=>b!('╕'), '╣'=>b!('╣'), '║'=>b!('║'), '╗'=>b!('╗'), '╝'=>b!('╝'), '╜'=>b!('╜'), '╛'=>b!('╛'), '┐'=>b!('┐'),
        '└'=>b!('└'), '┴'=>b!('┴'), '┬'=>b!('┬'), '├'=>b!('├'), '─'=>b!('─'), '┼'=>b!('┼'), '╞'=>b!('╞'), '╟'=>b!('╟'),
        '╚'=>b!('╚'), '╔'=>b!('╔'), '╩'=>b!('╩'), '╦'=>b!('╦'), '╠'=>b!('╠'), '═'=>b!('═'), '╬'=>b!('╬'), '╧'=>b!('╧'),
        '╨'=>b!('╨'), '╤'=>b!('╤'), '╥'=>b!('╥'), '╙'=>b!('╙'), '╘'=>b!('╘'), '╒'=>b!('╒'), '╓'=>b!('╓'), '╫'=>b!('╫'),
        '╪'=>b!('╪'), '┘'=>b!('┘'), '┌'=>b!('┌'), '█'=>b!('█'), '▄'=>b!('▄'), '▌'=>b!('▌'), '▐'=>b!('▐'), '▀'=>b!('▀'),
        'α'=>b!('α'), 'β'=>b!('β'), 'Γ'=>b!('Γ'), 'π'=>b!('π'), 'Σ'=>b!('Σ'), 'σ'=>b!('σ'), 'μ'=>b!('μ'), 'τ'=>b!('τ'),
        'Φ'=>b!('Φ'), 'Θ'=>b!('Θ'), 'Ω'=>b!('Ω'), 'δ'=>b!('δ'), '∞'=>b!('∞'), 'ϕ'=>b!('ϕ'), 'ε'=>b!('ε'), '∩'=>b!('∩'),
        '≡'=>b!('≡'), '±'=>b!('±'), '≥'=>b!('≥'), '≤'=>b!('≤'), '⌠'=>b!('⌠'), '⌡'=>b!('⌡'), '÷'=>b!('÷'), '≈'=>b!('≈'),
        '°'=>b!('°'), '¨'=>b!('¨'), '·'=>b!('·'), '√'=>b!('√'), 'ⁿ'=>b!('ⁿ'), '²'=>b!('²'), '■'=>b!('■'), '⎕'=>b!('⎕'),
        '␀'=>0, '‘'|'’'|'′'|'ʹ'|'ʻ'|'ʼ'|'ʽ'=>b'\'', '−'|'–'=>b'-', '“'|'”'|'″'|'ʺ'=>b'"', '≠'=>b'~',
        ';'=>b';', '♬'=>b!('♫'), '¦'=>b'|', 'Δ'|'∆'=>b!('⌂'), '⎮'=>b!('│'), '⍺'|'ɑ'=>b!('α'),
        'ß'|'ϐ'=>b!('β'), 'Π'|'∏'|'ϖ'=>b!('π'), '∑'=>b!('Σ'), 'µ'=>b!('μ'), 'θ'|'ϑ'=>b!('Θ'),
        'Ω'=>b!('Ω'), 'ð'|'∂'=>b!('δ'), 'φ'|'ɸ'|'∅'|'Ø'|'ø'=>b!('ϕ'), '∈'|'€'|'ɛ'=>b!('ε'),
        '≢'=>b!('≈'), '∙'|'˙'=>b!('¨'), '⌼'=>b!('◘'), '⍽'=>b!('⎕'),
        _ => 0
    }
}

pub fn tobytes(string: &str) -> Option<Bstr> {
    let mut vector = Bstr::with_capacity(string.len() / 2);
    for c in string.chars() {
        if c == '\r' { continue } // crlf moment
        match tobyte(c) {
            0 => return None,
            b => vector.push(b),
        }
    }
    Some(vector)
}
