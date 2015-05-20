use regex::Regex;
use std::rc::Rc;
use Symbol;

#[derive(Debug)]
pub struct ClassDefn<'input> {
    name: &'input str,
    members: Vec<Rc<MemberDefn<'input>>>
}

#[derive(Debug)]
pub enum MemberDefn<'input> {
    Field(Box<FieldDefn<'input>>),
    Method(Box<MethodDefn<'input>>),
}

#[derive(Debug)]
pub struct FieldDefn<'input> {
    name: &'input str,
    ty: TypeRef<'input>,
}

#[derive(Debug)]
pub struct MethodDefn<'input> {
    name: &'input str,
    arg_tys: Vec<TypeRef<'input>>,
    ret_ty: TypeRef<'input>,
}

#[derive(Clone,Debug)]
pub struct TypeRef<'input> {
    id: &'input str
}

rusty_peg! {
    parser Classy<'input> {
        ID: &'input str =
            regex(r"^[a-zA-Z_][a-zA-Z_0-9]*") - ["class"];

        CLASS: Rc<ClassDefn<'input>> =
            ("class", <name:ID>, "{", <members:{MEMBER}>, "}") => {
                Rc::new(ClassDefn { name: name, members: members })
            };

        MEMBER: Rc<MemberDefn<'input>> =
            (FIELD_DEFN / METHOD_DEFN);

        FIELD_DEFN: Rc<MemberDefn<'input>> =
            (<name:ID>, ":", <ty:TYPE_REF>, ";") => {
                Rc::new(MemberDefn::Field(Box::new(
                    FieldDefn { name: name, ty: ty })))
            };

        TYPE_REF: TypeRef<'input> =
            (<id:ID>) => {
                TypeRef { id: id }
            };

        METHOD_DEFN: Rc<MemberDefn<'input>> =
            (<name:ID>, "(", <args:{TYPE_REF}>, ")", "->", <ret:TYPE_REF>, ";") => {
                Rc::new(MemberDefn::Method(Box::new(
                    MethodDefn { name: name, arg_tys: args, ret_ty: ret })))
            };
    }
}

#[test]
fn parse_a_class() {
    let mut classy = Classy::new(());
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

