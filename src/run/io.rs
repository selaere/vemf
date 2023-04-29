use crate::prelude::*;

pub trait Interface<'io> {
    /// read bytes from input stream `stm`. used by `αÜβ` and `α_Üβ` when α is negative and finite.
    /// corresponds to `Read::read(_, buf)`. this method is required, but
    /// feel free to return None if you don't have output capabilities
    fn read_bytes(&mut self, stm: usize, size: isize) -> Option<Vec<u8>>;
    /// write to output stream `stm` a slice of bytes `slice`. used by `Ö` `_Ö` and `☻` `☺` (to 
    /// stream 0). corresponds to `Write::write(_, buf)`. this method is required, but
    /// feel free to return None if you don't have input capabilities
    fn write(&mut self, stm: usize, slice: &[u8]) -> Option<usize>;
    /// read until a delimiter from input stream `stm`. used by `αÜβ` and `α_Üβ` when α is positive.
    /// corresponds to like `ReadBuf::read_until(_, delim, buf)`
    /// the default definition is inefficient but it's the best way of doing this generically,
    /// implementers SHOULD implement this better.
    fn read_until(&mut self, stm: usize, delim: u8) -> Option<Vec<u8>> {
        let mut buf = self.read_bytes(stm, 1)?;
        if buf[0] == b'\n' { return None }
        while let Some(&[c]) = self.read_bytes(stm, 1).as_ref().map(|x| &x[..]) {
            buf.push(c);
            if c == delim { break; }
        }
        Some(buf)
    }
    /// read all from input stream `stm`. used by `ΘÖβ` and `Θ_Öβ`.
    /// corresponds to `Read::read_to_end(_, buf)`.
    fn read_to_end(&mut self, stm: usize) -> Option<Vec<u8>> {
        let mut buf = self.read_bytes(stm, 1024)?;
        if buf.len() < 1024 { return Some(buf); }
        while let Some(a) = self.read_bytes(stm, 1024) {
            let len = a.len();
            buf.extend(a.into_iter());
            if len < 1024 { return Some(buf)}
        }
        Some(buf)
    }
    fn flush(&mut self, _stm: usize) -> Option<()> { None }
}

pub struct NoIO;

impl<'io> Interface<'io> for NoIO {
    fn read_bytes (&mut self, _: usize, _: isize) -> Option<Vec<u8>> { None }
    fn read_until (&mut self, _: usize, _: u8)    -> Option<Vec<u8>> { None }
    fn read_to_end(&mut self, _: usize)           -> Option<Vec<u8>> { None }
    fn write      (&mut self, _: usize, _: &[u8]) -> Option<usize> { None }
}

pub struct FromInterface<'a, 'io>(pub &'a mut dyn Interface<'io>);
impl<'a> core::fmt::Write for FromInterface<'a, '_> {
    fn write_str (&mut self, s: &str) -> core::fmt::Result {
        self.0.write(0, s.as_bytes()).map(|_| ()).ok_or(core::fmt::Error)
    }
}

#[cfg(any(feature = "std", test))]
mod standard {
    use std::io::{Write, Read, BufRead};
    use core::fmt::{Result as FResult, Error as FError};
        
    pub fn io_result(ioresult: std::io::Result<usize>) -> Option<usize> { match ioresult {
        Ok(n) => Some(n),
        Err(e) if e.kind() == std::io::ErrorKind::Interrupted => Some(0),
        Err(_) => None,
    }}

    pub struct StdIO {}

    impl super::Interface<'_> for StdIO {
        fn read_bytes(&mut self, stm: usize, size: isize) -> Option<Vec<u8>> {
            if stm == 0 {
                let mut buf = vec![0; size as usize];
                let size = io_result(std::io::stdin().read(&mut buf))?;
                buf.truncate(size);
                Some(buf)
            } else { None }
        }
        fn read_until(&mut self, stm: usize, delim: u8) -> Option<Vec<u8>> {
            if stm == 0 {
                let mut buf = Vec::new();
                io_result(std::io::stdin().lock().read_until(delim, &mut buf))?;
                Some(buf)
            } else { None }
        }
        fn read_to_end(&mut self, stm: usize) -> Option<Vec<u8>> {
            if stm == 0 {
                let mut buf = Vec::new();
                io_result(std::io::stdin().lock().read_to_end(&mut buf))?;
                Some(buf)
            } else { None }
        }
        fn write(&mut self, stm: usize, slice: &[u8]) -> Option<usize> { match stm {
            0 => io_result(std::io::stdout().write(slice)),
            1 => io_result(std::io::stderr().write(slice)),
            _ => None,
        }}
        fn flush(&mut self, _stm: usize) -> Option<()> {
            std::io::stdout().flush().ok()
        }
    }

    pub struct FromIoWrite<T: std::io::Write>(pub T);
    impl<T: std::io::Write> std::fmt::Write for FromIoWrite<T> {
        fn write_str (&mut self, s: &str) -> FResult { write!(self.0, "{s}").map_err(|_| FError) }
        fn write_char(&mut self, c: char) -> FResult { write!(self.0, "{c}").map_err(|_| FError) }
        fn write_fmt(&mut self, args: core::fmt::Arguments<'_>) -> FResult {
            self.0.write_fmt(args).map_err(|_| FError)
        }
    }

}

#[cfg(any(feature = "std", test))]
pub use standard::*;