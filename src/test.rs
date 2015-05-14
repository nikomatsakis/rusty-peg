use super::{Grammar, ParseResult, Parser, Input};
use util::*;

use std::fmt::Debug;

trait ToParser {
    type Parser;

    fn to_parser(self) -> Self::Parser;
}

impl ToParser for &'static str {
    type Parser = Literal;

    fn to_parser(self) -> Literal {
        Literal::new(self.to_string())
    }
}

macro_rules! grammar1 {
    { $name:ident is { $($grammar_defn:tt)* } } => {

    }
}

macro_rules! grammar {
    { $name:ident is { $($nonterminal:ident: $ty:ty = $defn:tt => $body:expr;)* }} => {
        #[allow(non_snake_case)]
        pub struct $name {
            __dummy: (),
            $($nonterminal: Box<Parser<$name,Output=$ty>>),+,
        }

        impl Grammar for $name { }

        $(declare_nonterminal!{
            $name,
            $ty,
            $nonterminal,
            $defn,
            $body,
        })*

        impl $name {
            fn new() -> $name {
                $name {
                    __dummy: (),
                    $($nonterminal: Box::new(item!($defn))),+,
                }
            }

            fn pretty_print(&self) -> String {
                let strings = vec![$(
                    format!("{:?}={}", $nonterminal, self.$nonterminal.pretty_print())
                    ),*];
                strings.connect(", ")
            }
        }
    }
}

macro_rules! declare_nonterminal {
    ($grammar:ident,$ty:ty,$nonterminal:ident,$defn:tt,$body:expr,) => {
        #[derive(Debug)]
        pub struct $nonterminal;

        impl ToParser for $nonterminal {
            type Parser = $nonterminal;
            fn to_parser(self) -> $nonterminal { self }
        }

        impl Parser<$grammar> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                format!("{:?}", self)
            }

            fn parse<'a>(&'a self, grammar: &'a $grammar, start: Input<'a>) -> ParseResult<'a,$ty> {
                let (end, item_pattern!($defn)) =
                    try!(grammar.$nonterminal.parse(grammar, start));
                Ok((end,$body))
            }
        }
    }
}

// "class" name:Id "{" members:{Member} "}" => { .. }

macro_rules! items_pattern {
    // XXX this comma should not be needed
    ( $name:ident : $a:tt, $($bs:tt)* ) => {
        ($name, items_pattern!($($bs)*))
    };
    ( $name:ident : a:tt ) => {
        $name
    };
    ( $a:tt, $($bs:tt)* ) => {
        (item_pattern!($a), items_pattern!($($bs)*))
    };
    ( $a:tt | $($bs:tt)* ) => {
        item_pattern!($a)
    };
    ( $a:tt ) => {
        item_pattern!($a)
    };
    ( ) => {
        ()
    }
}

macro_rules! item_pattern {
    { ( ) } => {
        ()
    };

    { ( $($tt:tt)* ) } => {
        items_pattern!($($tt)*)
    };

    { [ $($tt:tt)* ] } => {
        _
    };

    { { + $($tt:tt)* } } => {
        _
    };

    { { * $($tt:tt)* } } => {
        _
    };

    { { $($tt:tt)* } } => {
        _
    };

    { $name:expr } => {
        _
    };
}

macro_rules! items {
    // XXX this comma should not be needed
    ( $name:ident : $a:tt, $($bs:tt)* ) => {
        Join { first: item!($a), second: items!($($bs)*), }
    };
    ( $name:ident : a:tt ) => {
        item!($a)
    };
    ( $a:tt, $($bs:tt)* ) => {
        Join { first: item!($a), second: items!($($bs)*), }
    };
    ( $a:tt | $($bs:tt)* ) => {
        Or { a: item!($a), b: items!($($bs)*) }
    };
    ( $a:tt ) => {
        item!($a)
    };
    ( ) => {
        Empty
    }
}

macro_rules! item {
    { ( ) } => {
        Empty
    };

    { ( $($tt:tt)* ) } => {
        items!($($tt)*)
    };

    { [ $($tt:tt)* ] } => {
        Optional { parser: items!($($tt)*) }
    };

    { { + $($tt:tt)* } } => {
        Repeat { parser: items!($($tt)*), min: 1 }
    };

    { { * $($tt:tt)* } } => {
        Repeat { parser: items!($($tt)*), min: 0 }
    };

    { { $($tt:tt)* } } => {
        Repeat { parser: items!($($tt)*), min: 0 }
    };

    { $name:expr } => {
        ($name).to_parser()
    };
}

grammar! {
    Foo is {
        Hi: u32 = "Hi" => 1;
        Ho: u32 = "Ho" => 2;

        HiOrHo: u32 = (Hi|Ho) => 0;

        HiHo: () = (
            Hi, Ho
        ) => ();
    }
}

fn should_parse_prefix<G,P:?Sized>(grammar: &G,
                                   parser: &P,
                                   text: &str)
                                   -> P::Output
    where G: Grammar, P: Parser<G>
{
    parser.parse_prefix(grammar, text).unwrap().1
}

//#[test]
//fn parse_hi_from_hi() {
//    let g = Foo::new();
//    assert_eq!((), should_parse_prefix(&g, &*g.Hi, "Hi"));
//}
//
//#[test]
//#[should_panic]
//fn parse_hi_from_ho() {
//    let g = Foo::new();
//    assert_eq!((), should_parse_prefix(&g, &*g.Hi, "Ho"));
//}
//
//#[test]
//fn parse_hiorho_from_hi() {
//    let g = Foo::new();
//    assert_eq!((), should_parse_prefix(&g, &*g.HiOrHo, "Hi"));
//}
//
//#[test]
//fn parse_hiorho_from_ho() {
//    let g = Foo::new();
//    assert_eq!((), should_parse_prefix(&g, &*g.HiOrHo, "Ho"));
//}
//
//#[test]
//fn parse_hiho_from_ho() {
//    let g = Foo::new();
//    assert_eq!((), should_parse_prefix(&g, &*g.HiHo, "Hi Ho"));
//}
