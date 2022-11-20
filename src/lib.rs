#![warn(clippy::cast_lossless)]
#![warn(clippy::map_unwrap_or)]
#![warn(clippy::semicolon_if_nothing_returned)]

#[macro_use] pub mod codepage;
mod token; mod parse; mod run; mod test;

pub use run::{Env, Val, c64, Interface, NoIO, io_result};

pub use token::rewrite;

/// owned byte string type. length will be the same as a Vec in 64bit archs
pub type Bstr = smallvec::SmallVec<[u8; 16]>;
