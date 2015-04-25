#![crate_type="rlib"]

pub trait Grammar {
    type Error: Error;
}

pub trait Error {
    fn offset(&self) -> usize;
}

#[derive(Copy,Clone)]
pub struct Input<'a> {
    text: &'a str,
    offset: usize,
}

pub type PegResult<'a,G,O> = Result<(O, Input<'a>), PegError<'a, G>>;

pub trait Peg<G:Grammar> {
    type Output;

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>) -> PegResult<'a, G, Self::Output>;
}

pub type Parser<G,O> = Box<Peg<G,Output=O>>;

pub enum PegError<'a, G:Grammar> {
    ExpectedText(&'a str),
    User(G::Error),
}

mod util;

#[cfg(test)] mod test;

pub use util::And;
pub use util::Or;
pub use util::Literal;
