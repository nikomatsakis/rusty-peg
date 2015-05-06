use super::{Error, ErrorKind, Grammar, Input, Kind, NonterminalId, Parser, ParseResult};
use super::tree::{ParseTree, Span};
use std::rc::Rc;

// ID :=
//     "[a-zA-Z]+"
// FOO :=
//     ( "class" ID "{" {MEMBER} "}" )
// MEMBER :=
//     ( FIELD  ) |
//     ( METHOD )
// FIELD :=
//     ( ID ":" TYPE )
// METHOD :=
//     ("void" ID "{")
// NONTERMINAL := A | B | C ;
// NONTERMINAL == <expr> ;
// ()         ==> grouping
// []         ==> optional
// {}         ==> zero or more
// A B        ==> whitespace separated
// identifier ==> non-terminal
// "xyz"      ==> regular expression
// we build a parse tree that keeps each non-terminal

impl<NT:NonterminalId> Parser<NT> for Box<Parser<NT>> {
    fn parse<'a>(&'a self, grammar: &'a Grammar<NT>, input: Input<'a>) -> ParseResult<'a,NT> {
        let obj: &Parser<NT> = self;
        Parser::parse(obj, grammar, input)
    }
}

pub struct Then<P1,P2> {
    first: P1,
    second: P2
}

impl<P1,P2,NT> Parser<NT> for Then<P1,P2>
    where P1: Parser<NT>, P2: Parser<NT>
{
    fn parse<'a>(&'a self, grammar: &'a Grammar<NT>, start: Input<'a>) -> ParseResult<'a,NT> {
        let (mid, mut first) = try!(self.first.parse(grammar, start));
        let mid = skip_whitespace(mid);
        let (end, second) = try!(self.second.parse(grammar, mid));
        first.sibling = Some(Rc::new(second));
        Ok((end, first))
    }
}

fn skip_whitespace<'a>(mut input: Input<'a>) -> Input<'a> {
    let bytes = input.text.as_bytes();
    while input.offset < input.text.len() && is_space(bytes[input.offset]) {
        input.offset += 1;
    }
    return input;

    fn is_space(c: u8) -> bool {
        match c as char {
            ' ' => true,
            '\n' => true,
            _ => false,
        }
    }
}

pub struct Literal {
    text: String
}

impl Literal {
    pub fn new(s: String) -> Literal {
        Literal { text: s }
    }
}

impl<NT> Parser<NT> for Literal
    where NT: NonterminalId
{
    fn parse<'a>(&'a self, _: &'a Grammar<NT>, start: Input<'a>) -> ParseResult<'a,NT> {
        if start.text[start.offset..].starts_with(&self.text) {
            let end = start.offset_by(self.text.len());
            Ok((end, ParseTree::new(Kind::Text, Span::new(start.offset, end.offset), None)))
        } else {
            Err(Error { kind: ErrorKind::Expected(&self.text),
                        offset: start.offset })
        }
    }
}

pub struct Optional<P> {
    parser: P
}

impl<NT,P> Parser<NT> for Optional<P>
    where P: Parser<NT>
{
    fn parse<'a>(&'a self, grammar: &'a Grammar<NT>, start: Input<'a>) -> ParseResult<'a,NT> {
        let (end, child) = match self.parser.parse(grammar, start) {
            Ok((end, result)) => (end, Some(result)),
            Err(_) => (start, None)
        };
        Ok((end, ParseTree::new(Kind::Option,
                                Span::new(start.offset, end.offset),
                                child)))
    }
}

pub struct Repeat<P> {
    parser: P
}

impl<NT,P> Parser<NT> for Repeat<P>
    where P: Parser<NT>
{
    fn parse<'a>(&'a self, grammar: &'a Grammar<NT>, start: Input<'a>) -> ParseResult<'a,NT> {
        let mut mid = start;
        let mut children = vec![];
        loop {
            match self.parser.parse(grammar, start) {
                Ok((end, result)) => {
                    children.push(ParseTree::new(Kind::Elem,
                                                 Span::new(mid.offset, end.offset),
                                                 Some(result)));
                    mid = end;
                }
                Err(_) => { break; }
            }
        }
        Ok((mid, ParseTree::new(Kind::Repeat,
                                Span::new(start.offset, mid.offset),
                                ParseTree::sibling_chain(children))))
    }
}

pub struct Nonterminal<NT> {
    nonterminal: NT
}

impl<NT> Parser<NT> for Nonterminal<NT>
    where NT: NonterminalId
{
    fn parse<'a>(&'a self, grammar: &'a Grammar<NT>, start: Input<'a>) -> ParseResult<'a,NT> {
        let def = &grammar.nonterminals[self.nonterminal.to_usize()];
        assert!(!def.is_empty());
        let mut opt_old_err: Option<Error> = None;
        for parser in def {
            match parser.parse(grammar, start) {
                Ok((end, result)) => {
                    let tree = ParseTree::new(Kind::Nonterminal(self.nonterminal.clone()),
                                              Span::new(start.offset, end.offset),
                                              Some(result));
                    return Ok((end, tree));
                }

                Err(new_err) => {
                    if let Some(old_err) = opt_old_err.clone() {
                        if old_err.offset <= new_err.offset {
                            opt_old_err = Some(new_err);
                        }
                    } else {
                        opt_old_err = Some(new_err);
                    }
                }
            }
        }
        Err(opt_old_err.unwrap())
    }
}
