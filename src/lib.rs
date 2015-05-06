#![crate_type="rlib"]

extern crate regex;

use tree::ParseTree;

pub struct Grammar<NT> {
    nonterminals: Vec<NonterminalDef<NT>>
}

pub type NonterminalDef<NT> = Vec<Box<Parser<NT>>>;

pub trait NonterminalId: Clone {
    fn to_usize(&self) -> usize;
}

#[derive(Copy,Clone)]
pub struct Input<'a> {
    text: &'a str,
    offset: usize,
}

pub trait Parser<NT> {
    fn parse<'a>(&'a self, grammar: &'a Grammar<NT>, input: Input<'a>) -> ParseResult<'a,NT>;
}

pub type ParseResult<'a,NT> = Result<(Input<'a>, ParseTree<Kind<NT>>), Error<'a>>;

pub enum Kind<NT> {
    Text,
    Option,
    Repeat,
    Elem,
    Group,
    Nonterminal(NT)
}

#[derive(Clone)]
pub struct Error<'a> {
    kind: ErrorKind<'a>,
    offset: usize
}

#[derive(Clone)]
pub enum ErrorKind<'a> {
    Expected(&'a str),
}

pub mod util;
pub mod tree;

impl<'a> Input<'a> {
    fn offset_by(&self, amount: usize) -> Input<'a> {
        Input { text: self.text, offset: self.offset + amount }
    }
}

#[cfg(test)] mod test;
