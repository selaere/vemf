use crate::prelude::*;

pub trait Interface<'io> {
    fn read     (&mut self, stm: usize, buf: &mut [u8])    -> Option<usize>;
    fn write    (&mut self, stm: usize, slice: &[u8])      -> Option<usize>;
    fn read_line(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        let mut a: [u8; 1] = [0];
        let original_len = buf.len();
        while let Some(1) = self.read(stm, &mut a[..]) {
            buf.push(a[0]);
            if a[0] == b'\n' { break; }
        }
        Some(buf.len() - original_len)
    }
    fn read_to_end(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        let mut tmp = [0; 256];
        let original_len = buf.len();
        loop {
            match self.read(stm, &mut tmp[..]) {
                Some(0) => break,
                Some(n) => buf.extend(&tmp[..n]),
                None => if buf.len() > original_len { break } else { return None }
            }
        }
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

#[cfg(feature = "std")]
pub fn io_result(ioresult: std::io::Result<usize>) -> Option<usize> {
    match ioresult {
        Ok(n) => Some(n),
        Err(e) if e.kind() == std::io::ErrorKind::Interrupted => Some(0),
        Err(_) => None,
    }
}

#[cfg(feature="std")] use std::io::{Write, Read, BufRead};
#[cfg(feature="std")] pub struct StdIO {}

#[cfg(feature="std")]
impl Interface<'_> for StdIO {
    fn read(&mut self, stm: usize, buf: &mut [u8]) -> Option<usize> {
        if stm == 0 {
            io_result(std::io::stdin().read(buf))
        } else { None }
    }
    fn read_line(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        if stm == 0 {
            io_result(std::io::stdin().lock().read_until(b'\n', buf))
        } else { None }
    }
    fn read_to_end(&mut self, stm: usize, buf: &mut Vec<u8>) -> Option<usize> {
        if stm == 0 {
            io_result(std::io::stdin().read_to_end(buf))
        } else { None }
    }
    fn write(&mut self, stm: usize, slice: &[u8]) -> Option<usize> {
        match stm {
            0 => io_result(std::io::stdout().write(slice)),
            1 => io_result(std::io::stderr().write(slice)),
            _ => None,
        }
    }
}
