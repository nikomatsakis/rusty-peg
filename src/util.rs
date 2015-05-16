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

impl<'input,P1,P2,R,G> Parser<'input,G> for Or<P1,P2>
    where P1: Parser<'input,G,Output=R>, P2: Parser<'input,G,Output=R>, G: Grammar
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
pub struct Join<P1,P2> {
    pub first: P1,
    pub second: P2,
}

impl<'input,P1,P2,G> Parser<'input,G> for Join<P1,P2>
    where P1: Parser<'input,G>, P2: Parser<'input,G>, G: Grammar
{
    type Output = (P1::Output, P2::Output);

    fn pretty_print(&self) -> String {
        format!("{} {}", self.first.pretty_print(), self.second.pretty_print())
    }

    fn parse(&self, grammar: &mut G, start: Input<'input>)
                 -> ParseResult<'input,(P1::Output,P2::Output)>
    {
        let (mid, first) = try!(self.first.parse(grammar, start));
        let (sep, ()) = try!(Whitespace.parse(grammar, mid));
        let (end, second) = try!(self.second.parse(grammar, sep));
        Ok((end, (first, second)))
    }
}

#[derive(Debug)]
pub struct Empty;

impl<'input,G> Parser<'input,G> for Empty
    where G: Grammar
{
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

impl<'input,G> Parser<'input,G> for Whitespace
    where G: Grammar
{
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

fn skip_whitespace<'input>(mut input: Input<'input>) -> Input<'input> {
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

impl<'input,G> Parser<'input,G> for &'static str
    where G: Grammar
{
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
    parser: P
}

impl<'input,G,P> Parser<'input,G> for Optional<P>
    where P: Parser<'input,G>, G: Grammar
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

impl<'input,G,P,S> Parser<'input,G> for Repeat<P,S>
    where P: Parser<'input,G>, G: Grammar, S: Parser<'input,G>
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
