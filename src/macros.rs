#[macro_export]
macro_rules! rusty_peg {
    { parser $name:ident: $base:ty { $($grammar_defn:tt)* } } => {
        rusty_peg_with_nonterminals! {
            rusty_peg_declare_parser(($name) ($base)); $($grammar_defn)*
        }

        rusty_peg_with_nonterminals! {
            rusty_peg_init_parser(($name) ($base)); $($grammar_defn)*
        }

        rusty_peg_declare_nonterminals! { $name, $($grammar_defn)* }
    }
}

#[macro_export]
macro_rules! rusty_peg_with_nonterminals {
    ( $m:ident($($args:tt)*) ;
      $nonterminal:ident: $ty:ty = $defn:tt => $body:expr ;
      $($remainder:tt)* ) => {
        rusty_peg_with_nonterminals! { $m($($args)* ($nonterminal, $ty));
                                       $($remainder)* }
    };
    ( $m:ident($($args:tt)*) ;
      $nonterminal:ident: $ty:ty = $defn:tt ;
      $($remainder:tt)* ) => {
        rusty_peg_with_nonterminals! { $m($($args)* ($nonterminal, $ty));
                                       $($remainder)* }
    };
    ( $m:ident($($args:tt)*) ; ) => {
        $m! { $($args)* }
    };
}

#[macro_export]
macro_rules! rusty_peg_declare_parser {
    ( ($name:ident) ($base:ty) $(($nonterminal:ident, $ty:ty))* ) => {
        #[allow(non_snake_case)]
        pub struct $name<'input> {
            marker: $crate::std::marker::PhantomData<&'input()>,
            base: $base,
            $($nonterminal: $crate::Cache<'input,$ty>),*
        }
    }
}

#[macro_export]
macro_rules! rusty_peg_init_parser {
    ( ($name:ident) ($base:ty) $(($nonterminal:ident, $ty:ty))* ) => {
        impl<'input> $name<'input> {
            fn new(base: $base) -> $name<'input> {
                $name {
                    marker: $crate::std::marker::PhantomData,
                    base: base,
                    $($nonterminal: $crate::std::collections::HashMap::new()),*
                }
            }
        }
    }
}

#[macro_export]
macro_rules! rusty_peg_declare_nonterminals {
    ( $grammar:ident, $nonterminal:ident: $ty:ty = $defn:tt => $body:expr ;
      $($remainder:tt)* ) => {
        rusty_peg_declare_map_nonterminal! { $grammar, $nonterminal, $ty, $defn, $body }
        rusty_peg_declare_nonterminals! { $grammar, $($remainder)* }
    };
    ( $grammar:ident, $nonterminal:ident: $ty:ty = $defn:tt ;
      $($remainder:tt)* ) => {
        rusty_peg_declare_identity_nonterminal! { $grammar, $nonterminal, $ty, $defn }
        rusty_peg_declare_nonterminals! { $grammar, $($remainder)* }
    };
    ( $grammar:ident, ) => {
    };
}

#[macro_export]
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
macro_rules! rusty_peg_named_item {
    ( ( $($a:tt)* ) ) => {
        rusty_peg_named_items!($($a)*)
    };
    ( $a:tt ) => {
        rusty_peg_item!($a)
    }
}

#[macro_export]
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
macro_rules! rusty_peg_named_item_pat {
    ( ( $($a:tt)* ) ) => {
        rusty_peg_named_items_pat!($($a)*)
    };
    ( $a:tt ) => {
        _
    }
}

#[macro_export]
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
macro_rules! rusty_peg_items {
    ( $a:tt, $($bs:tt)* ) => {
        $crate::util::Join { first: rusty_peg_item!($a), second: rusty_peg_items!($($bs)*), }
    };
    ( $a:tt | $($bs:tt)* ) => {
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

