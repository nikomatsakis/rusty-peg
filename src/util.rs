use super::{Grammar, Input, Parser, Peg, PegError, PegResult};

impl<G,O> Peg<G> for Parser<G,O>
    where G: Grammar
{
    type Output = O;

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>) -> PegResult<'a, G, O> {
        let obj: &Peg<G,Output=O> = self;
        Peg::parse(obj, grammar, input)
    }
}

pub struct Literal
{
    text: String
}

impl<G:Grammar> Peg<G> for Literal {
    type Output = ();

    fn parse<'a>(&'a self, grammar: &G, mut input: Input<'a>) -> PegResult<'a, G, ()> {
        if input.text[input.offset..].starts_with(&self.text) {
            input.offset += self.text.len();
            Ok(((), input))
        } else {
            Err(PegError::ExpectedText(&self.text))
        }
    }
}

pub struct Or<P1, P2>
{
    peg1: P1,
    peg2: P2,
}

impl<G,P1,P2> Peg<G> for Or<P1, P2>
    where G: Grammar, P1: Peg<G>, P2: Peg<G,Output=P1::Output>
{
    type Output = P1::Output;

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>) -> PegResult<'a, G, P1::Output> {
        self.peg1.parse(grammar, input)
                 .or_else(|_| self.peg2.parse(grammar, input))
    }
}

pub struct And<P1, P2>
{
    peg1: P1,
    peg2: P2,
}

impl<G,P1,P2> Peg<G> for And<P1, P2>
    where G: Grammar, P1: Peg<G>, P2: Peg<G>,
{
    type Output = (P1::Output, P2::Output);

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>)
                 -> PegResult<'a, G, (P1::Output, P2::Output)>
    {
        let (output1, input) = try!(self.peg1.parse(grammar, input));
        let (output2, input) = try!(self.peg2.parse(grammar, input));
        Ok(((output1, output2), input))
    }
}
