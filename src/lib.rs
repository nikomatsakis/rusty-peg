#![crate_type="rlib"]
#![allow(dead_code)] // for now

extern crate regex;

pub trait Grammar {
}

#[derive(Copy,Clone)]
pub struct Input<'a> {
    text: &'a str,
    offset: usize,
}

pub trait Parser<G:Grammar> {
    type Output;

    fn pretty_print(&self) -> String;

    fn parse_prefix<'a>(&self, grammar: &'a mut G, text: &'a str)
                     -> ParseResult<'a,Self::Output>
    {
        let input = Input { text: text, offset: 0 };
        self.parse(grammar, input)
    }

    fn parse<'a>(&self, grammar: &mut G, input: Input<'a>)
                 -> ParseResult<'a,Self::Output>;
}

pub type ParseResult<'a,O> = Result<(Input<'a>, O), Error<'a>>;

pub enum Kind<NT> {
    Text,
    Option,
    Repeat,
    Elem,
    Group,
    Nonterminal(NT)
}

#[derive(Clone, Debug)]
pub struct Error<'a> {
    expected: &'a str,
    offset: usize
}

#[macro_use]
pub mod macros;
pub mod util;
pub mod tree;

impl<'a> Input<'a> {
    fn offset_by(&self, amount: usize) -> Input<'a> {
        Input { text: self.text, offset: self.offset + amount }
    }
}

mod test;
