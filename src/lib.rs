#![crate_type="rlib"]

pub trait Grammar {
    type Error: Error;

    fn expected_text(&self, input: Input, expected: &str) -> Self::Error;
    fn expected_whitespace(&self, input: Input) -> Self::Error;
}

pub trait Error {
    fn offset(&self) -> usize;
}

#[derive(Copy,Clone)]
pub struct Input<'a> {
    text: &'a str,
    offset: usize,
}

pub type PegResult<'a,G,O> = Result<(O, Input<'a>), <G as Grammar>::Error>;

pub trait Peg<G:Grammar> {
    type Output;

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>) -> PegResult<'a, G, Self::Output>;
}

pub type Parser<G,O> = Box<Peg<G,Output=O>>;

pub mod util;
pub mod tree;

#[cfg(test)] mod test;
