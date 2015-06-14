/*!
 * Just make sure that the regular expression accepts the sorts of things we want to
 * accept. Not everything in this grammar is actually EXECUTED.
 */

use Symbol;

rusty_peg! {
    parser Parser<'input> {
        StrHi: &'input str = "Hi";
        StrHi1: &'input str = ("Hi");
        StrHiHo: (&'input str, &'input str) = ("Hi" "Ho");

        IntHi: u32 = "Hi" => 1;
        IntHi1: u32 = ("Hi") => 1;
        IntHiHo: (u32, u32) = ("Hi" "Ho") => (1,1);

        VecHi: Vec<&'input str> = {"Hi"};
        VecHiHo: Vec<(&'input str, &'input str)> = {"Hi" "Ho"};

        OptHi: Option<&'input str> = ["Hi"];
        OptHiHo: Option<(&'input str, &'input str)> = ["Hi" "Ho"];

        MapHi: &'input str = (<n:"Hi">) => n;
        MapHi1: &'input str = (<n:("Hi")>) => n;
        MapHiHo: (&'input str, &'input str) = (<n:("Hi" "Ho")>) => n;

        PosHiHo: (usize, (&'input str, (usize, (&'input str, usize)))) =
            (POS~"Hi"~POS "Ho"~POS);
    }
}

#[test]
fn test_position() {
    let input = "    Hi        Ho      ";
    //           01234567890123456789012
    //           0         1         2

    let (start, (_, (mid, (_, end)))) =
        PosHiHo.parse_complete(&mut Parser::new(()), input).unwrap();
    assert_eq!((start, mid, end), (4, 6, 16));
}
