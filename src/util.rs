use super::{Cache, Error, Input, Symbol, ParseResult};
use regex::Regex;
use std::collections::HashSet;

// used by macro expansion
pub use std::marker::PhantomData;
pub use std::collections::HashMap;
pub use std::rc::Rc;

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

#[derive(Debug)]
pub struct Or<NT1,P2> {
    pub a: NT1,
    pub b: P2,
}

impl<'input,NT1,P2,R,G> Symbol<'input,G> for Or<NT1,P2>
    where NT1: Symbol<'input,G,Output=R>, P2: Symbol<'input,G,Output=R>
{
    type Output = R;

    fn pretty_print(&self) -> String {
        format!("({} | {})", self.a.pretty_print(), self.b.pretty_print())
    }

    fn parse(&self, grammar: &mut G, start: Input<'input>)
                 -> ParseResult<'input,R>
    {
        match self.a.parse(grammar, start) {
            Ok(success) => { return Ok(success); }
            Err(_) => { }
        }

        self.b.parse(grammar, start)
    }
}

#[derive(Debug)]
pub struct Join<NT1,P2> {
    pub first: NT1,
    pub second: P2,
}

impl<'input,NT1,P2,G> Symbol<'input,G> for Join<NT1,P2>
    where NT1: Symbol<'input,G>, P2: Symbol<'input,G>
{
    type Output = (NT1::Output, P2::Output);

    fn pretty_print(&self) -> String {
        format!("{} {}", self.first.pretty_print(), self.second.pretty_print())
    }

    fn parse(&self, grammar: &mut G, start: Input<'input>)
                 -> ParseResult<'input,(NT1::Output,P2::Output)>
    {
        let (mid, first) = try!(self.first.parse(grammar, start));
        let (sep, ()) = try!(Whitespace.parse(grammar, mid));
        let (end, second) = try!(self.second.parse(grammar, sep));
        Ok((end, (first, second)))
    }
}

#[derive(Debug)]
pub struct Empty;

impl<'input,G> Symbol<'input,G> for Empty {
    type Output = ();

    fn pretty_print(&self) -> String {
        format!("()")
    }

    fn parse(&self, _: &mut G, start: Input<'input>)
                 -> ParseResult<'input,()>
    {
        Ok((start, ()))
    }
}

#[derive(Debug)]
pub struct Whitespace;

impl<'input,G> Symbol<'input,G> for Whitespace {
    type Output = ();

    fn pretty_print(&self) -> String {
        format!("Whitespace")
    }

    fn parse(&self, _: &mut G, start: Input<'input>)
             -> ParseResult<'input,()>
    {
        Ok((skip_whitespace(start), ()))
    }
}

pub fn skip_whitespace<'input>(mut input: Input<'input>) -> Input<'input> {
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

impl<'input,G> Symbol<'input,G> for &'static str {
    type Output = &'static str;

    fn pretty_print(&self) -> String {
        format!("{:?}", self)
    }

    fn parse(&self, _: &mut G, start: Input<'input>) -> ParseResult<'input,&'static str> {
        let text = *self;
        if start.text[start.offset..].starts_with(text) {
            let end = start.offset_by(text.len());
            Ok((end, text))
        } else {
            Err(Error { expected: text, offset: start.offset })
        }
    }
}

#[derive(Debug)]
pub struct Optional<P> {
    pub parser: P
}

impl<'input,G,P> Symbol<'input,G> for Optional<P>
    where P: Symbol<'input,G>
{
    type Output = Option<P::Output>;

    fn pretty_print(&self) -> String {
        format!("[{}]", self.parser.pretty_print())
    }

    fn parse(&self, grammar: &mut G, start: Input<'input>)
                 -> ParseResult<'input,Option<P::Output>>
    {
        match self.parser.parse(grammar, start) {
            Ok((end, result)) => Ok((end, Some(result))),
            Err(_) => Ok((start, None))
        }
    }
}

#[derive(Debug)]
pub struct Repeat<P,S> {
    pub parser: P,
    pub separator: S,
    pub min: usize,
}

impl<'input,G,P,S> Symbol<'input,G> for Repeat<P,S>
    where P: Symbol<'input,G>, S: Symbol<'input,G>
{
    type Output = Vec<P::Output>;

    fn pretty_print(&self) -> String {
        match self.min {
            0 => format!("{{{}}}", self.parser.pretty_print()),
            1 => format!("{{+ {}}}", self.parser.pretty_print()),
            _ => format!("{{#{} {}}}", self.min, self.parser.pretty_print()),
        }
    }

    fn parse(&self, grammar: &mut G, start: Input<'input>)
             -> ParseResult<'input,Vec<P::Output>>
    {
        let mut mid = start;
        let mut children = vec![];
        let mut err;
        loop {
            match self.parser.parse(grammar, mid) {
                Ok((end, result)) => {
                    children.push(result);

                    match self.separator.parse(grammar, end) {
                        Ok((sep_end, _)) => {
                            mid = sep_end;
                        }

                        Err(e) => {
                            err = e;
                            break;
                        }
                    }
                }
                Err(e) => {
                    err = e;
                    break;
                }
            }
        }
        if children.len() >= self.min {
            return Ok((mid, children));
        } else {
            return Err(err);
        }
    }
}

#[derive(Debug)]
pub struct RegexNt {
    regex: Regex,
    exceptions: HashSet<String>,
}

impl RegexNt {
    pub fn new(text: &str, exceptions: HashSet<String>) -> RegexNt {
        // we always want the regex anchored to the start of the string
        let text = format!("^{}", text);
        RegexNt {regex: Regex::new(&text).unwrap(), exceptions: exceptions}
    }
}

impl<'input,G> Symbol<'input,G> for RegexNt {
    type Output = &'input str;

    fn pretty_print(&self) -> String {
        format!("{:?}", self)
    }

    fn parse(&self,
             _: &mut G,
             start: ::Input<'input>)
             -> ::ParseResult<'input,&'input str>
    {
        match self.regex.find(&start.text[start.offset..]) {
            Some((_, offset)) => {
                let end = start.offset_by(offset);
                let matched = &start.text[start.offset..end.offset];
                if !self.exceptions.contains(matched) {
                    return Ok((end, matched));
                }
            }
            None => { }
        }

        Err(::Error { expected: "regex", offset: start.offset })
    }
}

pub fn memoize<'input,P,T:Clone,ComputeFn,CacheFn>(
    parser: &mut P,
    mut cache_fn: CacheFn,
    offset: usize,
    compute_fn: ComputeFn)
    -> ParseResult<'input,T>
    where
    CacheFn: FnMut(&mut P) -> &mut Cache<'input,T>,
    ComputeFn: FnOnce(&mut P) -> ParseResult<'input,T>,
{
    {
        let cache = cache_fn(parser);
        match cache.get(&offset) {
            Some(p) => { return p.clone(); }
            None => { }
        }
    }

    let result = compute_fn(parser);

    let cache = cache_fn(parser);
    cache.insert(offset, result.clone());

    result
}
