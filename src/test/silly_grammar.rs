use Symbol;

pub struct Foo;

rusty_peg! {
    parser Parser<'input>: Foo {
        Hi: u32 = ("Hi") => 1;
        Ho: u32 = "Ho" => 2;

        HiOrHo: u32 = (Hi / Ho);

        Sum: u32 = (Sum1 / HiOrHo);
        Sum1: u32 = (<x:HiOrHo>, "+", <y:Sum>) => {x + y*10};

        HiHo: () = (Hi, Ho) => ();

        Rep: Vec<u32> = {HiOrHo};
    }
}

fn should_parse_prefix<'input,P:?Sized>(
    symbol: &P,
    text: &'input str)
    -> P::Output
    where P: Symbol<'input,Parser<'input>>
{
    let mut parser = Parser::new(Foo);
    symbol.parse_prefix(&mut parser, text).unwrap().1
}

#[test]
fn parse_hi_from_hi() {
    assert_eq!(1, should_parse_prefix(&Hi, "Hi"));
}

#[test]
#[should_panic]
fn parse_hi_from_ho() {
    assert_eq!(2, should_parse_prefix(&Hi, "Ho"));
}

#[test]
fn parse_hiorho_from_hi() {
    assert_eq!(1, should_parse_prefix(&HiOrHo, "Hi"));
}

#[test]
fn parse_hiorho_from_ho() {
    assert_eq!(2, should_parse_prefix(&HiOrHo, "Ho"));
}

#[test]
fn parse_hiho_from_ho() {
    assert_eq!((), should_parse_prefix(&HiHo, "Hi Ho"));
}

#[test]
fn parse_sum_from_ho() {
    assert_eq!(1221, should_parse_prefix(&Sum, "Hi + Ho + Ho + Hi"));
}

#[test]
fn parse_repeat() {
    assert_eq!(vec![1, 2, 2, 1, 2], should_parse_prefix(&Rep, "Hi Ho Ho Hi Ho"));
}
