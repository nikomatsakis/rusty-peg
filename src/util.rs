use super::{Error, Grammar, Input, Parser, ParseResult};

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
pub struct Or<P1,P2> {
    pub a: P1,
    pub b: P2,
}

impl<P1,P2,R,G> Parser<G> for Or<P1,P2>
    where P1: Parser<G,Output=R>, P2: Parser<G,Output=R>, G: Grammar
{
    type Output = R;

    fn pretty_print(&self) -> String {
        format!("({} | {})", self.a.pretty_print(), self.b.pretty_print())
    }

    fn parse<'a>(&'a self, grammar: &'a G, start: Input<'a>)
                 -> ParseResult<'a,R>
    {
        match self.a.parse(grammar, start) {
            Ok(success) => Ok(success),
            Err(_) => self.b.parse(grammar, start)
        }
    }
}

#[derive(Debug)]
pub struct Join<P1,P2> {
    pub first: P1,
    pub second: P2,
}

impl<P1,P2,G> Parser<G> for Join<P1,P2>
    where P1: Parser<G>, P2: Parser<G>, G: Grammar
{
    type Output = (P1::Output, P2::Output);

    fn pretty_print(&self) -> String {
        format!("{} {}", self.first.pretty_print(), self.second.pretty_print())
    }

    fn parse<'a>(&'a self, grammar: &'a G, start: Input<'a>)
                 -> ParseResult<'a,(P1::Output,P2::Output)>
    {
        let (mid, first) = try!(self.first.parse(grammar, start));
        let (sep, ()) = try!(Whitespace.parse(grammar, mid));
        let (end, second) = try!(self.second.parse(grammar, sep));
        Ok((end, (first, second)))
    }
}

#[derive(Debug)]
pub struct Empty;

impl<G> Parser<G> for Empty
    where G: Grammar
{
    type Output = ();

    fn pretty_print(&self) -> String {
        format!("()")
    }

    fn parse<'a>(&'a self, _: &'a G, start: Input<'a>)
                 -> ParseResult<'a,()>
    {
        Ok((start, ()))
    }
}

#[derive(Debug)]
pub struct Whitespace;

impl<G> Parser<G> for Whitespace
    where G: Grammar
{
    type Output = ();

    fn pretty_print(&self) -> String {
        format!("Whitespace")
    }

    fn parse<'a>(&'a self, _: &'a G, start: Input<'a>)
                 -> ParseResult<'a,()>
    {
        Ok((skip_whitespace(start), ()))
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

#[derive(Debug)]
pub struct Literal {
    text: String
}

impl Literal {
    pub fn new(s: String) -> Literal {
        Literal { text: s }
    }
}

impl<G> Parser<G> for Literal
    where G: Grammar
{
    type Output = ();

    fn pretty_print(&self) -> String {
        format!("\"{}\"", self.text)
    }

    fn parse<'a>(&'a self, _: &'a G, start: Input<'a>) -> ParseResult<'a,()> {
        if start.text[start.offset..].starts_with(&self.text) {
            let end = start.offset_by(self.text.len());
            Ok((end, ()))
        } else {
            Err(Error { expected: &self.text,
                        offset: start.offset })
        }
    }
}

#[derive(Debug)]
pub struct Optional<P> {
    parser: P
}

impl<G,P> Parser<G> for Optional<P>
    where P: Parser<G>, G: Grammar
{
    type Output = Option<P::Output>;

    fn pretty_print(&self) -> String {
        format!("[{}]", self.parser.pretty_print())
    }

    fn parse<'a>(&'a self, grammar: &'a G, start: Input<'a>)
                 -> ParseResult<'a,Option<P::Output>>
    {
        match self.parser.parse(grammar, start) {
            Ok((end, result)) => Ok((end, Some(result))),
            Err(_) => Ok((start, None))
        }
    }
}

#[derive(Debug)]
pub struct Repeat<P> {
    parser: P,
    min: usize,
}

impl<G,P> Parser<G> for Repeat<P>
    where P: Parser<G>, G: Grammar
{
    type Output = Vec<P::Output>;

    fn pretty_print(&self) -> String {
        match self.min {
            0 => format!("{{{}}}", self.parser.pretty_print()),
            1 => format!("{{+ {}}}", self.parser.pretty_print()),
            _ => format!("{{#{} {}}}", self.min, self.parser.pretty_print()),
        }
    }

    fn parse<'a>(&'a self, grammar: &'a G, start: Input<'a>)
                 -> ParseResult<'a,Vec<P::Output>>
    {
        let mut mid = start;
        let mut children = vec![];
        loop {
            match self.parser.parse(grammar, start) {
                Ok((end, result)) => {
                    children.push(result);
                    mid = end;
                }
                Err(e) => {
                    if children.len() >= self.min {
                        return Ok((mid, children));
                    } else {
                        return Err(e);
                    }
                }
            }
        }
    }
}
