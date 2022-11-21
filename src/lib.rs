#![cfg_attr(not(feature="std"), no_std)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::map_unwrap_or)]
#![warn(clippy::semicolon_if_nothing_returned)]

extern crate core; extern crate alloc;

#[macro_use] pub mod codepage;
#[cfg(all(test, feature="std"))] mod test;
mod token; mod parse; mod run;

pub use run::{Env, Val, c64, io::{Interface, NoIO, StdIO}};
#[cfg(feature="std")] pub use run::io::io_result;

/// owned byte string type. length will be the same as a Vec in 64bit archs
pub type Bstr = smallvec::SmallVec<[u8; 16]>;
pub use token::rewrite;

mod prelude {
    pub use alloc::vec::Vec;
    pub use alloc::string::{String, ToString};
    pub use alloc::boxed::Box;
    pub use alloc::rc::Rc;
    pub use core::iter;

    pub use b;
    pub use alloc::{vec, format};

    pub use crate::Bstr;

    #[cfg(feature="std")] pub use std::collections::HashMap;
    #[cfg(feature="std")] pub use std::collections::HashSet;

    #[cfg(not(feature="std"))] pub use hashbrown::HashMap;
    #[cfg(not(feature="std"))] pub use hashbrown::HashSet;
}