use super::{Grammar, Input, Parser, Peg, PegError, PegResult};

trait PegUtil {
    fn or<P2>(self, peg2: P2) -> Or<Self, P2>;
    fn and<P2>(self, peg2: P2) -> And<Self, P2>;
    fn andl<P2>(self, peg2: P2) -> First<And<Self, P2>>;
    fn andr<P2>(self, peg2: P2) -> Second<And<Self, P2>>;
    fn first(self) -> First<Self>;
    fn second(self) -> Second<Self>;
    fn map<F>(self, f: F) -> Map<Self, F>;
}

impl<P1> PegUtil for P1 {
    fn or<P2>(self, peg2: P2) -> Or<P1, P2> {
        Or { peg1: self, peg2: peg2 }
    }
    fn and<P2>(self, peg2: P2) -> And<P1, P2> {
        And { peg1: self, peg2: peg2 }
    }
    fn andl<P2>(self, peg2: P2) -> First<And<P1, P2>> {
        self.and(peg2).first()
    }
    fn andr<P2>(self, peg2: P2) -> Second<And<P1, P2>> {
        self.and(peg2).second()
    }
    fn first(self) -> First<P1> {
        First { peg1: self }
    }
    fn second(self) -> Second<P1> {
        Second { peg1: self }
    }
    fn map<F>(self, f: F) -> Map<P1, F> {
        Map { peg1: self, func: f }
    }
}

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

pub struct First<P1>
{
    peg1: P1,
}

impl<A,B,G,P1> Peg<G> for First<P1>
    where G: Grammar, P1: Peg<G,Output=(A,B)>
{
    type Output = A;

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>)
                 -> PegResult<'a, G, A>
    {
        let ((a, b), input) = try!(self.peg1.parse(grammar, input));
        Ok((a, input))
    }
}

pub struct Second<P1>
{
    peg1: P1,
}

impl<A,B,G,P1> Peg<G> for Second<P1>
    where G: Grammar, P1: Peg<G,Output=(A,B)>
{
    type Output = B;

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>)
                 -> PegResult<'a, G, B>
    {
        let ((a, b), input) = try!(self.peg1.parse(grammar, input));
        Ok((b, input))
    }
}

pub struct Map<P1,F>
{
    peg1: P1,
    func: F,
}

impl<A,B,G,P1,F> Peg<G> for Map<P1, F>
    where G: Grammar, P1: Peg<G,Output=A>, F: Fn(A) -> B
{
    type Output = B;

    fn parse<'a>(&'a self, grammar: &G, input: Input<'a>)
                 -> PegResult<'a, G, B>
    {
        let (a, input) = try!(self.peg1.parse(grammar, input));
        let b = (self.func)(a);
        Ok((b, input))
    }
}
