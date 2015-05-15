macro_rules! grammar {
    { for $grammar:ty { $($grammar_defn:tt)* } } => {
        impl $crate::Grammar for $grammar { }
        declare_nonterminals! { $grammar, $($grammar_defn)* }
    }
}

macro_rules! declare_nonterminals {
    ( $grammar:ty, $nonterminal:ident: $ty:ty = $defn:tt => $body:expr ;
      $($remainder:tt)* ) => {
        declare_map_nonterminal! { $grammar, $nonterminal, $ty, $defn, $body }
        declare_nonterminals! { $grammar, $($remainder)* }
    };
    ( $grammar:ty, $nonterminal:ident: $ty:ty = $defn:tt ;
      $($remainder:tt)* ) => {
        declare_identity_nonterminal! { $grammar, $nonterminal, $ty, $defn }
        declare_nonterminals! { $grammar, $($remainder)* }
    };
    ( $grammar:ty, ) => {
    };
}

macro_rules! declare_map_nonterminal {
    ($grammar:ty, $nonterminal:ident, $ty:ty, $defn:tt, $body:expr) => {
        #[allow(non_camel_case_types)]
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
    ($grammar:ty, $nonterminal:ident, $ty:ty, $defn:tt) => {
        #[allow(non_camel_case_types)]
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
    pub struct Foo;

    grammar! {
        for Foo {
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
        assert_eq!(1, should_parse_prefix(&Foo, &Hi, "Hi"));
    }

    #[test]
    #[should_panic]
    fn parse_hi_from_ho() {
        assert_eq!(2, should_parse_prefix(&Foo, &Hi, "Ho"));
    }

    #[test]
    fn parse_hiorho_from_hi() {
        assert_eq!(1, should_parse_prefix(&Foo, &HiOrHo, "Hi"));
    }

    #[test]
    fn parse_hiorho_from_ho() {
        assert_eq!(2, should_parse_prefix(&Foo, &HiOrHo, "Ho"));
    }

    #[test]
    fn parse_hiho_from_ho() {
        assert_eq!((), should_parse_prefix(&Foo, &HiHo, "Hi Ho"));
    }

    #[test]
    fn parse_sum_from_ho() {
        assert_eq!(1221, should_parse_prefix(&Foo, &Sum, "Hi + Ho + Ho + Hi"));
    }

    #[test]
    fn parse_repeat() {
        assert_eq!(vec![1, 2, 2, 1, 2], should_parse_prefix(&Foo, &Rep, "Hi Ho Ho Hi Ho"));
    }
}

mod classy {
    use regex::Regex;
    use std::collections::HashSet;
    use Parser;

    #[derive(Debug)]
    pub struct ClassDefn {
        name: String,
        members: Vec<MemberDefn>
    }

    #[derive(Debug)]
    pub enum MemberDefn {
        Field(Box<FieldDefn>),
        Method(Box<MethodDefn>),
    }

    #[derive(Debug)]
    pub struct FieldDefn {
        name: String,
        ty: TypeRef
    }

    #[derive(Debug)]
    pub struct MethodDefn {
        name: String,
        arg_tys: Vec<TypeRef>,
        ret_ty: TypeRef
    }

    #[derive(Debug)]
    pub struct TypeRef {
        id: String
    }

    #[derive(Debug)]
    pub struct Classy {
        identifier: Regex,
        keywords: HashSet<String>,
    }

    impl Classy {
        fn new() -> Classy {
            Classy {
                identifier: Regex::new("^[a-zA-Z_][a-zA-Z_0-9]*").unwrap(),
                keywords: vec!["class"].into_iter().map(|x| x.to_string()).collect(),
            }
        }
    }

    #[allow(non_camel_case_types)]
    #[derive(Debug)]
    struct ID;

    impl Parser<Classy> for ID {
        type Output = String;

        fn pretty_print(&self) -> String {
            format!("{:?}", self)
        }

        fn parse<'a>(&self,
                     grammar: &'a Classy,
                     start: ::Input<'a>)
                     -> ::ParseResult<'a,String>
        {
            match grammar.identifier.find(&start.text[start.offset..]) {
                Some((_, offset)) => {
                    let end = start.offset_by(offset);
                    let matched = &start.text[start.offset..end.offset];
                    if !grammar.keywords.contains(matched) {
                        return Ok((end, matched.to_string()));
                    }
                }
                None => { }
            }

            Err(::Error { expected: "identifier", offset: start.offset })
        }
    }

    grammar! {
        for Classy {
            CLASS: ClassDefn =
                ("class", <name:ID>, "{", <members:{MEMBER}>, "}") => {
                    ClassDefn { name: name, members: members }
                };

            MEMBER: MemberDefn =
                (FIELD_DEFN | METHOD_DEFN);

            FIELD_DEFN: MemberDefn =
                (<name:ID>, ":", <ty:TYPE_REF>, ";") => {
                    MemberDefn::Field(Box::new(
                        FieldDefn { name: name, ty: ty }))
                };

            TYPE_REF: TypeRef =
                (<id:ID>) => {
                    TypeRef { id: id }
                };

            METHOD_DEFN: MemberDefn =
                (<name:ID>, "(", <args:{TYPE_REF}>, ")", "->", <ret:TYPE_REF>, ";") => {
                    MemberDefn::Method(Box::new(
                        MethodDefn { name: name, arg_tys: args, ret_ty: ret }))
                };
        }
    }

    #[test]
    fn parse_a_class() {
        let classy = Classy::new();
        CLASS.parse_prefix(
            &classy,
            "class x { f: u32; g: i32; h(i32) -> u32; }").unwrap();
    }
}

