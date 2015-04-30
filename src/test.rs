use super::{Grammar, Input, Parser, Peg, PegResult};
use super::util::*;

pub struct TestGrammar;

#[test]
fn basic_test() {
    let parser = Literal::new(format!("A"));
    
}
