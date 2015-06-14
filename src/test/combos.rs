/*!
 * Just make sure that the regular expression accepts the sorts of things we want to
 * accept. Don't test what this grammar parses just yet.
 */

rusty_peg! {
    parser Parser<'input> {
        StrHi: &'input str = "Hi";
        StrHi1: &'input str = ("Hi");
        StrHiHo: (&'input str, &'input str) = ("Hi" "Ho");

        IntHi: u32 = "Hi" => 1;
        IntHi1: u32 = ("Hi") => 1;
        IntHiHo: (u32, u32) = ("Hi" "Ho") => (1,1);
    }
}


