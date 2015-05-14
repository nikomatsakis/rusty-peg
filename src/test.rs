use super::{Grammar, ParseResult, Parser, Input};
use util::*;

use std::fmt::Debug;

macro_rules! grammar {
    { $name:ident is { $($grammar_defn:tt)* } } => {
        #[allow(non_snake_case)]
        pub struct $name {
            __dummy: (),
        }

        impl Grammar for $name { }

        impl $name {
            fn new() -> $name {
                $name {
                    __dummy: (),
                }
            }

            fn pretty_print(&self) -> String {
                format!("XXX")
            }
        }

        declare_nonterminals! { $name, $($grammar_defn)* }
    }
}

macro_rules! declare_nonterminals {
    ( $grammar:ident, $nonterminal:ident: $ty:ty = $defn:tt => $body:expr ;
      $($remainder:tt)* ) => {
        declare_map_nonterminal! { $grammar, $nonterminal, $ty, $defn, $body }
        declare_nonterminals! { $grammar, $($remainder)* }
    };
    ( $grammar:ident, $nonterminal:ident: $ty:ty = $defn:tt ;
      $($remainder:tt)* ) => {
        declare_identity_nonterminal! { $grammar, $nonterminal, $ty, $defn }
        declare_nonterminals! { $grammar, $($remainder)* }
    };
    ( $grammar:ident, ) => {
    };
}

macro_rules! declare_map_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty, $defn:tt, $body:expr) => {
        #[derive(Debug)]
        pub struct $nonterminal;

        impl Parser<$grammar> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                format!("{:?}", self)
            }

            fn parse<'a>(&self, grammar: &'a $grammar, start: Input<'a>) -> ParseResult<'a,$ty> {
                let parser = item!($defn);
                let (end, item_pattern!($defn)) = try!(Parser::parse(&parser, grammar, start));
                Ok((end,$body))
            }
        }
    }
}

macro_rules! declare_identity_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty, $defn:tt) => {
        #[derive(Debug)]
        pub struct $nonterminal;

        impl Parser<$grammar> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                format!("{:?}", self)
            }

            fn parse<'a>(&self, grammar: &'a $grammar, start: Input<'a>) -> ParseResult<'a,$ty> {
                let parser = item!($defn);
                Parser::parse(&parser, grammar, start)
            }
        }
    }
}

macro_rules! items_pattern {
    // XXX this comma should not be needed
    ( < $name:ident : $a:tt >, $($bs:tt)* ) => {
        ($name, items_pattern!($($bs)*))
    };
    ( < $name:ident : $a:tt > ) => {
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

    { ( $tt:tt ) } => {
        item_pattern!($tt)
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
    ( < $name:ident : $a:tt > , $($bs:tt)* ) => {
        Join { first: item!($a), second: items!($($bs)*), }
    };
    ( < $name:ident : $a:tt > ) => {
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

    { ( $tt:tt ) } => {
        item!($tt)
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
        $name
    };
}

grammar! {
    Foo is {
        Hi: u32 = ("Hi") => 1;
        Ho: u32 = "Ho" => 2;

        HiOrHo: u32 = (Hi|Ho);

        Sum1: u32 = (<x:HiOrHo>, "+", <y:Sum>) => {
            x + y*10
        };

        Sum: u32 = (Sum1 | HiOrHo);

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

#[test]
fn parse_hi_from_hi() {
    let g = Foo::new();
    assert_eq!(1, should_parse_prefix(&g, &Hi, "Hi"));
}

#[test]
#[should_panic]
fn parse_hi_from_ho() {
    let g = Foo::new();
    assert_eq!(2, should_parse_prefix(&g, &Hi, "Ho"));
}

#[test]
fn parse_hiorho_from_hi() {
    let g = Foo::new();
    assert_eq!(1, should_parse_prefix(&g, &HiOrHo, "Hi"));
}

#[test]
fn parse_hiorho_from_ho() {
    let g = Foo::new();
    assert_eq!(2, should_parse_prefix(&g, &HiOrHo, "Ho"));
}

#[test]
fn parse_hiho_from_ho() {
    let g = Foo::new();
    assert_eq!((), should_parse_prefix(&g, &HiHo, "Hi Ho"));
}

#[test]
fn parse_sum_from_ho() {
    let g = Foo::new();
    assert_eq!(1221, should_parse_prefix(&g, &Sum, "Hi + Ho + Ho + Hi"));
}
