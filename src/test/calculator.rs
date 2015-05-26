use std::rc::Rc;
use std::str::FromStr;
use Symbol;

rusty_peg! {
    parser Calculator<'input> {
        EXPR: u32 =
            ADD_SUB_EXPR;

        PAREN_EXPR: u32 =
            ("(", <e:EXPR>, ")") => e;

        ADD_SUB_EXPR: u32 =
            fold(<lhs:MUL_DIV_EXPR>,
                 ("+", <rhs:MUL_DIV_EXPR>) => { lhs + rhs },
                 ("-", <rhs:MUL_DIV_EXPR>) => { lhs - rhs });

        MUL_DIV_EXPR: u32 =
            fold(<lhs:ATOM_EXPR>,
                 ("*", <rhs:ATOM_EXPR>) => { lhs * rhs },
                 ("/", <rhs:ATOM_EXPR>) => { lhs / rhs });

        ATOM_EXPR: u32 =
            (NUMBER / PAREN_EXPR);

        NUMBER: u32 =
            (<s:NUMBER_STRING>) => u32::from_str(s).unwrap();

        NUMBER_STRING: &'input str =
            regex(r"[0-9]+");
    }
}

#[test]
fn parse_expr() {
    let mut classy = Calculator::new(());

    // If we don't get the order of ops right, we'll get the wrong
    // result here. In pairticular:
    //
    // ((((3+5)*11)/3)+1) == 33
    // (3 + 5*(11/3) + 1) == 19
    let result =
        EXPR.parse_complete(&mut classy,
                            "3 + 5*11/3 + 1").unwrap();

    assert_eq!(result, 22);
}

#[test]
fn parse_parens() {
    let mut classy = Calculator::new(());

    let result =
        EXPR.parse_complete(&mut classy,
                            "3 + 5*(11/3) + 1").unwrap();

    assert_eq!(result, 19);
}

#[test]
fn parse_number() {
    let mut classy = Calculator::new(());
    let result = EXPR.parse_complete(&mut classy, "22").unwrap();
    assert_eq!(result, 22);
}

#[test]
fn parse_plus() {
    let mut classy = Calculator::new(());
    let result = EXPR.parse_complete(&mut classy, "19 + 3").unwrap();
    assert_eq!(result, 22);
}

#[test]
fn parse_mul() {
    let mut classy = Calculator::new(());
    let result = EXPR.parse_complete(&mut classy, "11 * 2").unwrap();
    assert_eq!(result, 22);
}
