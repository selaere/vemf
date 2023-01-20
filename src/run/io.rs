use crate::prelude::*;

pub trait Interface<'io> {
    /// read from input stream `stm` into a slice `buf`. used by `αÜβ` and `α_Üβ` when α is 
    /// non-negative and finite. corresponds to `Read::read(_, buf)`. this method is required, but
    /// feel free to return None if you don't have output capabilities
    fn read(&mut self, stm: usize, buf: &mut [u8])    -> Option<usize>;
    /// write to output stream `stm` a slice of bytes `slice`. used by `É` `_É` and `☻` `☺` (to 
    /// stream 0). corresponds to `Write::write(_, buf)`. this method is required, but
    /// feel free to return None if you don't have input capabilities
    fn write(&mut self, stm: usize, slice: &[u8])      -> Option<usize>;
    /// read a line from input stream `stm`. used by `αÜβ` and `α_Üβ` when α is negative.
    /// corresponds to like `Read::read_until(_, b'\n', buf)`
    /// the default definition is inefficient but it's the best way of doing this generically,
    /// implementers SHOULD implement this better.
    fn read_line(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        let mut a: [u8; 1] = [0];
        let original_len = buf.len();
        while let Some(1) = self.read(stm, &mut a[..]) {
            buf.push(a[0]);
            if a[0] == b'\n' { break; }
        }
        Some(buf.len() - original_len)
    }
    /// read all from input stream `stm`. used by `∞Üβ` and `∞_Üβ`.
    /// corresponds to `Read::read_to_end(_, buf)`.
    fn read_to_end(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        let mut tmp = [0; 256];
        let original_len = buf.len();
        loop { match self.read(stm, &mut tmp[..]) {
            Some(0) => break,
            Some(n) => buf.extend(&tmp[..n]),
            None => if buf.len() > original_len { break } else { return None }
        }}
        Some(buf.len() - original_len)
    }
}

pub struct NoIO;

impl<'io> Interface<'io> for NoIO {
    fn read       (&mut self, _: usize, _: &mut [u8])    -> Option<usize> { None }
    fn read_line  (&mut self, _: usize, _: &mut Vec<u8>) -> Option<usize> { None }
    fn write      (&mut self, _: usize, _: &[u8])        -> Option<usize> { None }
    fn read_to_end(&mut self, _: usize, _: &mut Vec<u8>) -> Option<usize> { None }
}

#[cfg(any(feature = "std", test))]
pub fn io_result(ioresult: std::io::Result<usize>) -> Option<usize> { match ioresult {
    Ok(n) => Some(n),
    Err(e) if e.kind() == std::io::ErrorKind::Interrupted => Some(0),
    Err(_) => None,
}}

#[cfg(feature="std")] use std::io::{Write, Read, BufRead};
#[cfg(feature="std")] pub struct StdIO {}

#[cfg(feature="std")] impl Interface<'_> for StdIO {
    fn read(&mut self, stm: usize, buf: &mut [u8]) -> Option<usize> {
        if stm == 0 { io_result(std::io::stdin().read(buf)) } else { None }
    }
    fn read_line(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        if stm == 0 { io_result(std::io::stdin().lock().read_until(b'\n', buf)) } else { None }
    }
    fn read_to_end(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        if stm == 0 { io_result(std::io::stdin().read_to_end(buf)) } else { None }
    }
    fn write(&mut self, stm: usize, slice: &[u8]) -> Option<usize> { match stm {
        0 => io_result(std::io::stdout().write(slice)),
        1 => io_result(std::io::stderr().write(slice)),
        _ => None,
    }}
}

#[cfg(feature="std")] use core::fmt::{Result as FResult, Error as FError};
#[cfg(feature="std")] pub struct FromIoWrite<T: std::io::Write>(pub T);
#[cfg(feature="std")] impl<T: std::io::Write> std::fmt::Write for FromIoWrite<T> {
    fn write_str (&mut self, s: &str) -> FResult { write!(self.0, "{s}").map_err(|_| FError) }
    fn write_char(&mut self, c: char) -> FResult { write!(self.0, "{c}").map_err(|_| FError) }
    fn write_fmt(&mut self, args: core::fmt::Arguments<'_>) -> FResult {
        self.0.write_fmt(args).map_err(|_| FError)
    }
}