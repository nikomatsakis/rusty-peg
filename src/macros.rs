// NOTE: The only macro intentionally exported is `rusty_peg`. All
// other macros should be marked `#[doc(hidden)]` and considered
// internal implementation details.

#[macro_export]
macro_rules! rusty_peg {
    { parser $name:ident<'input>: $base:ty { $($grammar_defn:tt)+ } } => {
        rusty_peg_parser! {
            parser $name: $base { $($grammar_defn)* }
        }
    };

    { parser $name:ident<'input> { $($grammar_defn:tt)+ } } => {
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
                    (fld)
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
//   (fld (<ident>, <ty>, (<tt>))...)
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
// - `reg`: nonterminals defined like `FOO: &'input str = regex(r"something");`
//   or `FOO: &'input str = regex(r"something") - ["a", "b"];`
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
                (fld $(($fld_nt:ident, $fld_ty:ty, $fld_tt:tt))*)
        }
    } => {
        rusty_peg_parser_parsed! {
            (arg $($args)*)
            (any $(($any_nt, $any_ty))*)
            (def $(($def_nt, $def_ty, $def_tt))*)
            (map $(($map_nt, $map_ty, $map_tt))*)
            (reg $(($reg_nt, $reg_ty, $reg_tt))*)
            (fld $(($fld_nt, $fld_ty, $fld_tt))*)
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
                (fld $(($fld_nt:ident, $fld_ty:ty, $fld_tt:tt))*)
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
                    (fld $(($fld_nt, $fld_ty, $fld_tt))*)
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
                (fld $(($fld_nt:ident, $fld_ty:ty, $fld_tt:tt))*)
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
                    (fld $(($fld_nt, $fld_ty, $fld_tt))*)
            }
            $($remainder)*
        }
    };

    // Match a "regexp" definition with no exceptions:
    {
        $m:ident {
            (arg $($args:tt)*)
                (any $(($any_nt:ident, $any_ty:ty))*)
                (def $(($def_nt:ident, $def_ty:ty, $def_tt:tt))*)
                (map $(($map_nt:ident, $map_ty:ty, $map_tt:tt))*)
                (reg $(($reg_nt:ident, $reg_ty:ty, $reg_tt:tt))*)
                (fld $(($fld_nt:ident, $fld_ty:ty, $fld_tt:tt))*)
        }
        $nonterminal:ident: $ty:ty = regex($defn:expr);
        $($remainder:tt)*
    } => {
        rusty_peg_parse_grammar_definition! {
            $m {
                (arg $($args)*)
                    (any $(($any_nt, $any_ty))* ($nonterminal, $ty))
                    (def $(($def_nt, $def_ty, $def_tt))*)
                    (map $(($map_nt, $map_ty, $map_tt))*)
                    (reg $(($reg_nt, $reg_ty, $reg_tt))* ($nonterminal, $ty, ($defn, [])))
                    (fld $(($fld_nt, $fld_ty, $fld_tt))*)
            }
            $($remainder)*
        }
    };

    // Match a "regexp" definition with exceptions:
    {
        $m:ident {
            (arg $($args:tt)*)
                (any $(($any_nt:ident, $any_ty:ty))*)
                (def $(($def_nt:ident, $def_ty:ty, $def_tt:tt))*)
                (map $(($map_nt:ident, $map_ty:ty, $map_tt:tt))*)
                (reg $(($reg_nt:ident, $reg_ty:ty, $reg_tt:tt))*)
                (fld $(($fld_nt:ident, $fld_ty:ty, $fld_tt:tt))*)
        }
        $nonterminal:ident: $ty:ty = regex($defn:expr) - [ $($exceptions:expr),* ];
        $($remainder:tt)*
    } => {
        rusty_peg_parse_grammar_definition! {
            $m {
                (arg $($args)*)
                    (any $(($any_nt, $any_ty))* ($nonterminal, $ty))
                    (def $(($def_nt, $def_ty, $def_tt))*)
                    (map $(($map_nt, $map_ty, $map_tt))*)
                    (reg $(($reg_nt, $reg_ty, $reg_tt))*
                         ($nonterminal, $ty, ($defn, [$($exceptions),*])))
                    (fld $(($fld_nt, $fld_ty, $fld_tt))*)
            }
            $($remainder)*
        }
    };

    // Match a "fold" definition:
    {
        $m:ident {
            (arg $($args:tt)*)
                (any $(($any_nt:ident, $any_ty:ty))*)
                (def $(($def_nt:ident, $def_ty:ty, $def_tt:tt))*)
                (map $(($map_nt:ident, $map_ty:ty, $map_tt:tt))*)
                (reg $(($reg_nt:ident, $reg_ty:ty, $reg_tt:tt))*)
                (fld $(($fld_nt:ident, $fld_ty:ty, $fld_tt:tt))*)
        }
        $nonterminal:ident: $ty:ty = fold(<$lhs_nm:ident:$lhs_defn:tt>,
                                          $($rhs_defn:tt => $rhs_expr:expr),+);
        $($remainder:tt)*
    } => {
        rusty_peg_parse_grammar_definition! {
            $m {
                (arg $($args)*)
                    (any $(($any_nt, $any_ty))* ($nonterminal, $ty))
                    (def $(($def_nt, $def_ty, $def_tt))*)
                    (map $(($map_nt, $map_ty, $map_tt))*)
                    (reg $(($reg_nt, $reg_ty, $reg_tt))*)
                    (fld $(($fld_nt, $fld_ty, $fld_tt))*
                         ($nonterminal, $ty,
                          (fold($lhs_nm,$lhs_defn,$(($rhs_defn,$rhs_expr)),+))))
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
            (any $(($any_nt:ident, $any_ty:ty))+)
            (def $(($def_nt:ident, $def_ty:ty, ($def_tt:tt)))*)
            (map $(($map_nt:ident, $map_ty:ty, ($map_tt:tt => $map_expr:expr)))*)
            (reg $(($reg_nt:ident, $reg_ty:ty, ($reg_re:expr, [$($reg_exn:expr),*])))*)
            (fld $(($fld_nt:ident, $fld_ty:ty, $fld_tt:tt))*)
    } => {
        pub struct $name<'input> {
            marker: $crate::util::PhantomData<&'input()>,
            pub base: $base,
            caches: Caches<'input>,
            regexs: Regexs
        }

        #[allow(non_snake_case)]
        struct Caches<'input> {
            __dummy__: (),
            $($any_nt: $crate::Cache<'input,$any_ty>),*
        }

        #[allow(non_snake_case)]
        struct Regexs {
            __dummy__: (),
            $($reg_nt: $crate::util::Rc<$crate::util::RegexNt>),*
        }

        impl<'input> $name<'input> {
            fn new(base: $base) -> $name<'input> {
                $name {
                    marker: $crate::util::PhantomData,
                    base: base,
                    caches: Caches { __dummy__: (), $($any_nt: $crate::util::HashMap::new()),* },
                    regexs: Regexs {
                        __dummy__: (),
                        $($reg_nt: $crate::util::Rc::new($crate::util::RegexNt::new(
                            $reg_re,
                            vec![$($reg_exn),*].into_iter()
                                               .map(|t: &'static str| t.to_string())
                                               .collect()))),*
                    },
                }
            }
        }

        $(rusty_peg_declare_map_nonterminal!{$name, $map_nt, $map_ty, $map_tt, $map_expr})*
        $(rusty_peg_declare_identity_nonterminal!{$name, $def_nt, $def_ty, $def_tt})*
        $(rusty_peg_declare_regexp_nonterminal!{$name, $reg_nt, $reg_ty})*
        $(rusty_peg_declare_fold_nonterminal!{$name, $fld_nt, $fld_ty, $fld_tt})*
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_declare_map_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty, $defn:tt, $body:expr) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug)]
        pub struct $nonterminal;

        impl<'input> $crate::Symbol<'input,$grammar<'input>> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                stringify!($nonterminal).to_string()
            }

            fn parse(&self,
                     grammar: &mut $grammar<'input>,
                     start: $crate::Input<'input>)
                     -> $crate::ParseResult<'input,$ty>
            {
                $crate::util::memoize(
                    grammar,
                    |g| &mut g.caches.$nonterminal,
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
        #[derive(Copy, Clone, Debug)]
        pub struct $nonterminal;

        impl<'input> $crate::Symbol<'input,$grammar<'input>> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                stringify!($nonterminal).to_string()
            }

            fn parse(&self,
                     grammar: &mut $grammar<'input>,
                     start: $crate::Input<'input>)
                     -> $crate::ParseResult<'input,$ty>
            {
                $crate::util::memoize(
                    grammar,
                    |g| &mut g.caches.$nonterminal,
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
macro_rules! rusty_peg_declare_regexp_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug)]
        pub struct $nonterminal;

        impl<'input> $crate::Symbol<'input,$grammar<'input>> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                stringify!($nonterminal).to_string()
            }

            fn parse(&self,
                     grammar: &mut $grammar<'input>,
                     start: $crate::Input<'input>)
                     -> $crate::ParseResult<'input,$ty>
            {
                let regex = grammar.regexs.$nonterminal.clone();
                $crate::util::memoize(
                    grammar,
                    |grammar| &mut grammar.caches.$nonterminal,
                    start.offset,
                    |grammar| {
                        $crate::Symbol::parse(
                            &*regex,
                            grammar,
                            start)
                    })
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! rusty_peg_declare_fold_nonterminal {
    ($grammar:ident, $nonterminal:ident, $ty:ty,
     (fold($lhs_nm:ident,$lhs_defn:tt,$(($rhs_defn:tt,$rhs_expr:expr)),+))) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug)]
        pub struct $nonterminal;

        impl<'input> $crate::Symbol<'input,$grammar<'input>> for $nonterminal {
            type Output = $ty;

            fn pretty_print(&self) -> String {
                stringify!($nonterminal).to_string()
            }

            fn parse(&self,
                     grammar: &mut $grammar<'input>,
                     start: $crate::Input<'input>)
                     -> $crate::ParseResult<'input,$ty>
            {
                $crate::util::memoize(
                    grammar,
                    |grammar| &mut grammar.caches.$nonterminal,
                    start.offset,
                    |grammar| {
                        let lhs_parser = rusty_peg_item!($lhs_defn);
                        let (mut mid, mut $lhs_nm) =
                            try!($crate::Symbol::parse(&lhs_parser,
                                                       grammar,
                                                       start));
                        loop {
                            mid = $crate::util::skip_whitespace(mid);

                            $(
                                let rhs_parser = rusty_peg_named_item!($rhs_defn);
                                match $crate::Symbol::parse(&rhs_parser, grammar, mid) {
                                    Ok((end, rusty_peg_named_item_pat!($rhs_defn))) => {
                                        $lhs_nm = $rhs_expr;
                                        mid = end;
                                        continue;
                                    }
                                    Err(_) => { }
                                }
                                )+

                                break;
                        }

                        Ok((mid, $lhs_nm))
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

