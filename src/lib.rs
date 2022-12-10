#![cfg_attr(not(any(feature="std", test)), no_std)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::map_unwrap_or)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::todo)]

extern crate core; extern crate alloc;

#[macro_use] pub mod codepage;
#[cfg(test)] mod test;
mod token; mod parse; #[macro_use] mod run;

pub use run::{Env, Val, c64, io::{Interface, NoIO}};
#[cfg(feature="std")] pub use run::io::{io_result, StdIO, FromIoWrite};

/// owned byte string type. length will be the same as a Vec in 64bit archs
pub type Bstr = smallvec::SmallVec<[u8; 16]>;
pub use token::rewrite;

pub fn bx<T>(x: T) -> alloc::boxed::Box<T> { alloc::boxed::Box::new(x) }

mod prelude {
    pub use alloc::vec::Vec;
    pub use alloc::string::{String, ToString};
    pub use alloc::boxed::Box;
    pub use alloc::rc::Rc;
    pub use core::iter;
    
    pub use {b, or_nan, func};
    pub use alloc::{vec, format};
    pub use smallvec::smallvec as bstr;
    pub use crate::{Bstr, bx};

    // vemf does a lot of cloning. i got tired of the same 8 characters appearing so often
    pub trait ShorterClone: Clone { fn c(&self) -> Self { self.clone() } }
    impl<T> ShorterClone for T where T: Clone {}

    #[cfg(feature="std")] pub use std::collections::{HashMap, HashSet};

    #[cfg(not(feature="std"))] pub use hashbrown::{HashMap, HashSet};
}