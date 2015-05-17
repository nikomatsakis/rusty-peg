mod silly_grammar {
    use Symbol;

    pub struct Foo;

    rusty_peg_grammar! {
        nonterminals for Foo {
            Hi: u32 = ("Hi") => 1;
            Ho: u32 = "Ho" => 2;

            HiOrHo: u32 = (Hi|Ho);

            Sum: u32 = (Sum1 | HiOrHo);
            Sum1: u32 = (<x:HiOrHo>, "+", <y:Sum>) => {x + y*10};

            HiHo: () = (Hi, Ho) => ();

            Rep: Vec<u32> = {HiOrHo};
        }
    }

    fn should_parse_prefix<'input,G,P:?Sized>(
        grammar: &mut G,
        parser: &P,
        text: &'input str)
        -> P::Output
        where P: Symbol<'input,G>
    {
        parser.parse_prefix(grammar, text).unwrap().1
    }

    #[test]
    fn parse_hi_from_hi() {
        assert_eq!(1, should_parse_prefix(&mut Foo, &Hi, "Hi"));
    }

    #[test]
    #[should_panic]
    fn parse_hi_from_ho() {
        assert_eq!(2, should_parse_prefix(&mut Foo, &Hi, "Ho"));
    }

    #[test]
    fn parse_hiorho_from_hi() {
        assert_eq!(1, should_parse_prefix(&mut Foo, &HiOrHo, "Hi"));
    }

    #[test]
    fn parse_hiorho_from_ho() {
        assert_eq!(2, should_parse_prefix(&mut Foo, &HiOrHo, "Ho"));
    }

    #[test]
    fn parse_hiho_from_ho() {
        assert_eq!((), should_parse_prefix(&mut Foo, &HiHo, "Hi Ho"));
    }

    #[test]
    fn parse_sum_from_ho() {
        assert_eq!(1221, should_parse_prefix(&mut Foo, &Sum, "Hi + Ho + Ho + Hi"));
    }

    #[test]
    fn parse_repeat() {
        assert_eq!(vec![1, 2, 2, 1, 2], should_parse_prefix(&mut Foo, &Rep, "Hi Ho Ho Hi Ho"));
    }
}

mod classy {
    use regex::Regex;
    use std::collections::HashSet;
    use Symbol;

    #[derive(Debug)]
    pub struct ClassDefn<'input> {
        name: &'input str,
        members: Vec<MemberDefn<'input>>
    }

    #[derive(Debug)]
    pub enum MemberDefn<'input> {
        Field(Box<FieldDefn<'input>>),
        Method(Box<MethodDefn<'input>>),
    }

    #[derive(Debug)]
    pub struct FieldDefn<'input> {
        name: &'input str,
        ty: TypeRef<'input>
    }

    #[derive(Debug)]
    pub struct MethodDefn<'input> {
        name: &'input str,
        arg_tys: Vec<TypeRef<'input>>,
        ret_ty: TypeRef<'input>
    }

    #[derive(Debug)]
    pub struct TypeRef<'input> {
        id: &'input str
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

    impl<'input> Symbol<'input,Classy> for ID {
        type Output = &'input str;

        fn pretty_print(&self) -> String {
            format!("{:?}", self)
        }

        fn parse(&self,
                 grammar: &mut Classy,
                 start: ::Input<'input>)
                 -> ::ParseResult<'input,&'input str>
        {
            match grammar.identifier.find(&start.text[start.offset..]) {
                Some((_, offset)) => {
                    let end = start.offset_by(offset);
                    let matched = &start.text[start.offset..end.offset];
                    if !grammar.keywords.contains(matched) {
                        return Ok((end, matched));
                    }
                }
                None => { }
            }

            Err(::Error { expected: "identifier", offset: start.offset })
        }
    }

    rusty_peg_grammar! {
        nonterminals for Classy {
            CLASS: ClassDefn<'input> =
                ("class", <name:ID>, "{", <members:{MEMBER}>, "}") => {
                    ClassDefn { name: name, members: members }
                };

            MEMBER: MemberDefn<'input> =
                (FIELD_DEFN | METHOD_DEFN);

            FIELD_DEFN: MemberDefn<'input> =
                (<name:ID>, ":", <ty:TYPE_REF>, ";") => {
                    MemberDefn::Field(Box::new(
                        FieldDefn { name: name, ty: ty }))
                };

            TYPE_REF: TypeRef<'input> =
                (<id:ID>) => {
                    TypeRef { id: id }
                };

            METHOD_DEFN: MemberDefn<'input> =
                (<name:ID>, "(", <args:{TYPE_REF}>, ")", "->", <ret:TYPE_REF>, ";") => {
                    MemberDefn::Method(Box::new(
                        MethodDefn { name: name, arg_tys: args, ret_ty: ret }))
                };
        }
    }

    #[test]
    fn parse_a_class() {
        let mut classy = Classy::new();
        let (_, result) =
            CLASS.parse_prefix(
                &mut classy,
                "class x { f: u32; g: i32; h(i32) -> u32; }").unwrap();

        assert_eq!(
            normalize_space(&format!("{:#?}", result)),
            normalize_space("ClassDefn {
                name: \"x\",
                members: [
                    Field(
                        FieldDefn {
                            name: \"f\",
                            ty: TypeRef {
                                id: \"u32\"
                            }
                        }
                        ),
                    Field(
                        FieldDefn {
                            name: \"g\",
                            ty: TypeRef {
                                id: \"i32\"
                            }
                        }
                        ),
                    Method(
                        MethodDefn {
                            name: \"h\",
                            arg_tys: [
                                TypeRef {
                                    id: \"i32\"
                                }
                                ],
                            ret_ty: TypeRef {
                                id: \"u32\"
                            }
                        }
                        )
                    ]
            }"));
    }

    fn normalize_space(text: &str) {
        Regex::new(r"\s+").unwrap().replace_all(text, " ");
    }
}

