// NOTE: The only macro intentionally exported is `rusty_peg`. All
// other macros should be marked `#[doc(hidden)]` and considered
// internal implementation details.

#[macro_export]
macro_rules! rusty_peg {
    { parser $name:ident<'input>: $base:ty { $($grammar_defn:tt)* } } => {
        rusty_peg_parser! {
            parser $name: $base { $($grammar_defn)* }
        }
    };

    { parser $name:ident<'input> { $($grammar_defn:tt)* } } => {
        rusty_peg_parser! {
            parser $name: () { $($grammar_defn)* }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_parser {
    { parser $name:ident: $base:ty { $($grammar_defn:tt)* } } => {
        rusty_peg_parse_grammar_definition! {
            rusty_peg_parser_parsed {
                (arg ($name) ($base))
                    (any)
                    (def)
                    (map)
                    (reg)
            }
            $($grammar_defn)*
        }
    }
}

// This macro is used to parse the user's grammar definition and segregate
// the nonterminals into various distinct categories. Ultimately, it invokes
// `rusty_peg_parser_parsed` with a particular form:
//
// ```
// rusty_peg_parser_parsed! {
//   (arg ...)
//   (any (<ident>, <ty>)...)
//   (def (<ident>, <ty>, (<tt>))...)
//   (map (<ident>, <ty>, (<tt> => <expr>))...)
//   (reg (<ident>, <ty>, (<expr>))...)
// }
// ```
//
// Here:
// -  `...` is used to mean "repeated an arbitrary number of times.
// - `<ident>` is the identifier of some non-terminal.
// - `<ty>` is the type the user declared for that non-terminal.
// - `<tt>` refers to a token tree representing the definition of a nonterminal.
// - `<expr>` refers to a Rust expression.
//
// As you can see, `rusty_peg_parser_parsed` is invoked with five
// lists, each tagged with an identifier. The first list, tagged
// `any`, contains the names and types of ALL nonterminals. Each other
// list contains only those entries of a particular kind:
//
// - `map`: nonterminals defined like `FOO: u32 = ("a" "b") => { 22
//   };`.  Here, `<ident>` would be `FOO`, `<ty>` would be `u32`,
//   `<tt>` would be `("a" "b")`, and `<expr>` would be `{ 22 }`.
// - `def`: nonterminals defined like `FOO: &'static str =
//   "bar";`. Here, `<ident>` would be Foo`, `<ty>` would be `&'static
//   str`, and `<tt>` would be `"bar".
// - `reg`: nonterminals defined like `FOO: &'input str = regexp r"something";`
//   Here, `<ident>` would be Foo`, `<ty>` would be `&'input
//   str`, and `<expr>` would be `r"something"`.
//
// These strings are deliberately constructed to be easy to match and
// work with in subsequent macro definitions.
#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_parse_grammar_definition {
    // Base case.
    {
        $m:ident {
            (arg $($args:tt)*)
                (any $(($any_nt:ident, $any_ty:ty))*)
                (def $(($def_nt:ident, $def_ty:ty, $def_tt:tt))*)
                (map $(($map_nt:ident, $map_ty:ty, $map_tt:tt))*)
                (reg $(($reg_nt:ident, $reg_ty:ty, $reg_tt:tt))*)
        }
    } => {
        rusty_peg_parser_parsed! {
            (arg $($args)*)
            (any $(($any_nt, $any_ty))*)
            (def $(($def_nt, $def_ty, $def_tt))*)
            (map $(($map_nt, $map_ty, $map_tt))*)
            (reg $(($reg_nt, $reg_ty, $reg_tt))*)
        }
    };

    // Match a "map" definition:
    {
        $m:ident {
            (arg $($args:tt)*)
                (any $(($any_nt:ident, $any_ty:ty))*)
                (def $(($def_nt:ident, $def_ty:ty, $def_tt:tt))*)
                (map $(($map_nt:ident, $map_ty:ty, $map_tt:tt))*)
                (reg $(($reg_nt:ident, $reg_ty:ty, $reg_tt:tt))*)
        }
        $nonterminal:ident: $ty:ty = $defn:tt => $body:expr ;
        $($remainder:tt)*
    } => {
        rusty_peg_parse_grammar_definition! {
            $m {
                (arg $($args)*)
                    (any $(($any_nt, $any_ty))* ($nonterminal, $ty))
                    (def $(($def_nt, $def_ty, $def_tt))*)
                    (map $(($map_nt, $map_ty, $map_tt))* ($nonterminal, $ty, ($defn => $body)))
                    (reg $(($reg_nt, $reg_ty, $reg_tt))*)
            }
            $($remainder)*
        }
    };

    // Match a "equate" definition:
    {
        $m:ident {
            (arg $($args:tt)*)
                (any $(($any_nt:ident, $any_ty:ty))*)
                (def $(($def_nt:ident, $def_ty:ty, $def_tt:tt))*)
                (map $(($map_nt:ident, $map_ty:ty, $map_tt:tt))*)
                (reg $(($reg_nt:ident, $reg_ty:ty, $reg_tt:tt))*)
        }
        $nonterminal:ident: $ty:ty = $defn:tt ;
        $($remainder:tt)*
    } => {
        rusty_peg_parse_grammar_definition! {
            $m {
                (arg $($args)*)
                    (any $(($any_nt, $any_ty))* ($nonterminal, $ty))
                    (def $(($def_nt, $def_ty, $def_tt))* ($nonterminal, $ty, ($defn)))
                    (map $(($map_nt, $map_ty, $map_tt))*)
                    (reg $(($reg_nt, $reg_ty, $reg_tt))*)
            }
            $($remainder)*
        }
    };
}

// Invoked by rusty_peg_parse_grammar_definition, actually generates
// the parser structs and so forth.
#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_parser_parsed {
    {
        (arg ($name:ident) ($base:ty))
            (any $(($any_nt:ident, $any_ty:ty))*)
            (def $(($def_nt:ident, $def_ty:ty, ($def_tt:tt)))*)
            (map $(($map_nt:ident, $map_ty:ty, ($map_tt:tt => $map_expr:expr)))*)
            (reg $(($reg_nt:ident, $reg_ty:ty, $reg_tt:tt))*)
    } => {
        #[allow(non_snake_case)]
        pub struct $name<'input> {
            marker: $crate::util::PhantomData<&'input()>,
            base: $base,
            $($any_nt: $crate::Cache<'input,$any_ty>),*
        }

        impl<'input> $name<'input> {
            fn new(base: $base) -> $name<'input> {
                $name {
                    marker: $crate::util::PhantomData,
                    base: base,
                    $($any_nt: $crate::util::HashMap::new()),*
                }
            }
        }

        $(rusty_peg_declare_map_nonterminal!{$name, $map_nt, $map_ty, $map_tt, $map_expr})*
        $(rusty_peg_declare_identity_nonterminal!{$name, $def_nt, $def_ty, $def_tt})*
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_declare_map_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty, $defn:tt, $body:expr) => {
        #[allow(non_camel_case_types)]
        #[derive(Debug)]
        pub struct $nonterminal;

        impl<'input> $crate::Symbol<'input,$grammar<'input>> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                format!("{:?}", self)
            }

            fn parse(&self,
                     grammar: &mut $grammar<'input>,
                     start: $crate::Input<'input>)
                     -> $crate::ParseResult<'input,$ty>
            {
                $crate::util::memoize(
                    grammar,
                    |g| &mut g.$nonterminal,
                    start.offset,
                    |g| {
                        let parser = rusty_peg_named_item!($defn);
                        let (end, rusty_peg_named_item_pat!($defn)) =
                            try!($crate::Symbol::parse(&parser, g, start));
                        Ok((end,$body))
                    })
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_declare_identity_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty, $defn:tt) => {
        #[allow(non_camel_case_types)]
        #[derive(Debug)]
        pub struct $nonterminal;

        impl<'input> $crate::Symbol<'input,$grammar<'input>> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                format!("{:?}", self)
            }

            fn parse(&self,
                     grammar: &mut $grammar<'input>,
                     start: $crate::Input<'input>)
                     -> $crate::ParseResult<'input,$ty>
            {
                $crate::util::memoize(
                    grammar,
                    |g| &mut g.$nonterminal,
                    start.offset,
                    |g| {
                        let parser = rusty_peg_item!($defn);
                        $crate::Symbol::parse(&parser, g, start)
                    })
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_named_item {
    ( ( $($a:tt)* ) ) => {
        rusty_peg_named_items!($($a)*)
    };
    ( $a:tt ) => {
        rusty_peg_item!($a)
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_named_items {
    ( < $name:ident : $a:tt > , $($bs:tt)* ) => {
        {
            let bs = rusty_peg_named_items!($($bs)*);
            rusty_peg_items!($a, bs)
        }
    };
    ( < $name:ident : $a:tt > ) => {
        rusty_peg_item!($a)
    };
    ( $a:tt, $($bs:tt)* ) => {
        {
            let bs = rusty_peg_named_items!($($bs)*);
            rusty_peg_items!($a, bs)
        }
    };
    ( $a:tt ) => {
        rusty_peg_item!($a)
    };
    ( ) => {
        Empty
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_named_item_pat {
    ( ( $($a:tt)* ) ) => {
        rusty_peg_named_items_pat!($($a)*)
    };
    ( $a:tt ) => {
        _
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_named_items_pat {
    ( < $name:ident : $a:tt > , $($bs:tt)* ) => {
        ($name, rusty_peg_named_items_pat!($($bs)*))
    };
    ( < $name:ident : $a:tt > ) => {
        $name
    };
    ( $a:tt, $($bs:tt)* ) => {
        (_, rusty_peg_named_items_pat!($($bs)*))
    };
    ( $a:tt ) => {
        _
    };
    ( ) => {
        ()
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_items {
    ( $a:tt, $($bs:tt)* ) => {
        $crate::util::Join { first: rusty_peg_item!($a), second: rusty_peg_items!($($bs)*), }
    };
    ( $a:tt / $($bs:tt)* ) => {
        $crate::util::Or { a: rusty_peg_item!($a), b: rusty_peg_items!($($bs)*) }
    };
    ( $a:tt ) => {
        rusty_peg_item!($a)
    };
    ( ) => {
        Empty
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_item {
    { ( ) } => {
        Empty
    };

    { ( $tt:tt ) } => {
        rusty_peg_item!($tt)
    };

    { ( $($tt:tt)* ) } => {
        rusty_peg_items!($($tt)*)
    };

    { [ $($tt:tt)* ] } => {
        $crate::util::Optional { parser: rusty_peg_items!($($tt)*) }
    };

    { { + $($tt:tt)* } } => {
        $crate::util::Repeat { parser: rusty_peg_items!($($tt)*), min: 1,
                               separator: $crate::util::Whitespace }
    };

    { { * $($tt:tt)* } } => {
        $crate::util::Repeat { parser: rusty_peg_items!($($tt)*), min: 0,
                               separator: $crate::util::Whitespace }
    };

    { { $($tt:tt)* } } => {
        $crate::util::Repeat { parser: rusty_peg_items!($($tt)*), min: 0,
                               separator: $crate::util::Whitespace }
    };

    { $name:expr } => {
        $name
    };
}

