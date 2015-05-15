macro_rules! grammar {
    { $name:ident is { $($grammar_defn:tt)* } } => {
        #[allow(non_snake_case)]
        pub struct $name {
            __dummy: (),
        }

        impl $crate::Grammar for $name { }

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

        impl $crate::Parser<$grammar> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                format!("{:?}", self)
            }

            fn parse<'a>(&self,
                         grammar: &'a $grammar,
                         start: $crate::Input<'a>)
                         -> $crate::ParseResult<'a,$ty>
            {
                let parser = named_item!($defn);
                let (end, named_item_pat!($defn)) =
                    try!($crate::Parser::parse(&parser, grammar, start));
                Ok((end,$body))
            }
        }
    }
}

macro_rules! declare_identity_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty, $defn:tt) => {
        #[derive(Debug)]
        pub struct $nonterminal;

        impl $crate::Parser<$grammar> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                format!("{:?}", self)
            }

            fn parse<'a>(&self,
                         grammar: &'a $grammar,
                         start: $crate::Input<'a>)
                         -> $crate::ParseResult<'a,$ty> {
                             let parser = item!($defn);
                             $crate::Parser::parse(&parser, grammar, start)
                         }
        }
    }
}

macro_rules! named_item {
    ( ( $($a:tt)* ) ) => {
        named_items!($($a)*)
    };
    ( $a:tt ) => {
        item!($a)
    }
}

macro_rules! named_items {
    ( < $name:ident : $a:tt > , $($bs:tt)* ) => {
        {
            let bs = named_items!($($bs)*);
            items!($a, bs)
        }
    };
    ( < $name:ident : $a:tt > ) => {
        item!($a)
    };
    ( $a:tt, $($bs:tt)* ) => {
        {
            let bs = named_items!($($bs)*);
            items!($a, bs)
        }
    };
    ( $a:tt ) => {
        item!($a)
    };
    ( ) => {
        Empty
    };
}

macro_rules! named_item_pat {
    ( ( $($a:tt)* ) ) => {
        named_items_pat!($($a)*)
    };
    ( $a:tt ) => {
        _
    }
}

macro_rules! named_items_pat {
    ( < $name:ident : $a:tt > , $($bs:tt)* ) => {
        ($name, named_items_pat!($($bs)*))
    };
    ( < $name:ident : $a:tt > ) => {
        $name
    };
    ( $a:tt, $($bs:tt)* ) => {
        (_, named_items_pat!($($bs)*))
    };
    ( $a:tt ) => {
        _
    };
    ( ) => {
        ()
    };
}

macro_rules! items {
    ( $a:tt, $($bs:tt)* ) => {
        $crate::util::Join { first: item!($a), second: items!($($bs)*), }
    };
    ( $a:tt | $($bs:tt)* ) => {
        $crate::util::Or { a: item!($a), b: items!($($bs)*) }
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
        $crate::util::Optional { parser: items!($($tt)*) }
    };

    { { + $($tt:tt)* } } => {
        $crate::util::Repeat { parser: items!($($tt)*), min: 1,
                               separator: $crate::util::Whitespace }
    };

    { { * $($tt:tt)* } } => {
        $crate::util::Repeat { parser: items!($($tt)*), min: 0,
                               separator: $crate::util::Whitespace }
    };

    { { $($tt:tt)* } } => {
        $crate::util::Repeat { parser: items!($($tt)*), min: 0,
                               separator: $crate::util::Whitespace }
    };

    { $name:expr } => {
        $name
    };
}

mod silly_grammar {
    grammar! {
        Foo is {
            Hi: u32 = ("Hi") => 1;
            Ho: u32 = "Ho" => 2;

            HiOrHo: u32 = (Hi|Ho);

            Sum: u32 = (Sum1 | HiOrHo);
            Sum1: u32 = (<x:HiOrHo>, "+", <y:Sum>) => {x + y*10};

            HiHo: () = (Hi, Ho) => ();

            Rep: Vec<u32> = {HiOrHo};
        }
    }

    fn should_parse_prefix<G,P:?Sized>(grammar: &G,
                                       parser: &P,
                                       text: &str)
                                       -> P::Output
        where G: ::Grammar, P: ::Parser<G>
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

    #[test]
    fn parse_repeat() {
        let g = Foo::new();
        assert_eq!(vec![1, 2, 2, 1, 2], should_parse_prefix(&g, &Rep, "Hi Ho Ho Hi Ho"));
    }
}

