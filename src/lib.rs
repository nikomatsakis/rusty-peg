#![crate_type="rlib"]
#![allow(dead_code)] // for now

extern crate regex;

use std::collections::HashMap;

#[derive(Copy,Clone)]
pub struct Input<'a> {
    pub text: &'a str,
    pub offset: usize,
}

pub trait Symbol<'input, G> {
    type Output;

    fn parse_complete(&self, grammar: &mut G, text: &'input str)
                      -> Result<Self::Output, Error<'input>>
    {
        let start = Input { text: text, offset: 0 };
        let start = util::skip_whitespace(start);
        let (mid, result) = try!(self.parse(grammar, start));
        let end = util::skip_whitespace(mid);
        if end.offset == text.len() {
            Ok(result)
        } else {
            Err(Error { expected: "end of input",
                        offset: end.offset })
        }
    }

    fn parse_prefix(&self, grammar: &mut G, text: &'input str)
                     -> ParseResult<'input,Self::Output>
    {
        let input = Input { text: text, offset: 0 };
        self.parse(grammar, input)
    }

    fn parse(&self, grammar: &mut G, input: Input<'input>)
                 -> ParseResult<'input,Self::Output>;
}

pub type Cache<'input,T> = HashMap<usize, ParseResult<'input,T>>;

pub type ParseResult<'input,O> = Result<(Input<'input>, O), Error<'input>>;

pub enum Kind<NT> {
    Text,
    Option,
    Repeat,
    Elem,
    Group,
    Symbol(NT)
}

#[derive(Clone, Debug)]
pub struct Error<'input> {
    pub expected: &'input str,
    pub offset: usize
}

#[macro_use]
pub mod macros;
pub mod util;
pub mod tree;

impl<'input> Input<'input> {
    fn offset_by(&self, amount: usize) -> Input<'input> {
        Input { text: self.text, offset: self.offset + amount }
    }
}

#[cfg(test)]
mod test;
