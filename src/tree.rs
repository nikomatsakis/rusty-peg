use std::mem;
use std::rc::Rc;

#[derive(Debug)]
pub struct ParseTree<K> {
    pub kind: K,
    pub span: Span,
    pub child: Option<Rc<ParseTree<K>>>,
    pub sibling: Option<Rc<ParseTree<K>>>,
}

#[derive(Debug)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

pub const DUMMY_SPAN: Span = Span { lo: 0, hi: 0 };

///////////////////////////////////////////////////////////////////////////

impl<K> ParseTree<K> {
    #[cfg(test)]
    fn build(value: K, span: Span, children: Vec<ParseTree<K>>) -> ParseTree<K> {
        ParseTree::new(value, span, ParseTree::sibling_chain(children))
    }

    pub fn new(kind: K, span: Span, child: Option<ParseTree<K>>) -> ParseTree<K> {
        let child = child.map(Rc::new);
        ParseTree { kind: kind, span: span, child: child, sibling: None }
    }

    pub fn sibling_chain(mut children: Vec<ParseTree<K>>) -> Option<ParseTree<K>> {
        let mut p = match children.pop() {
            Some(p) => p,
            None => { return None; }
        };
        while let Some(mut q) = children.pop() {
            q.sibling = Some(Rc::new(p));
            p = q;
        }
        Some(p)
    }

    pub fn add_child_front(&mut self, mut child: ParseTree<K>) {
        assert!(child.sibling.is_none());
        let old_child = mem::replace(&mut self.child, None);
        child.sibling = old_child;
        self.child = Some(Rc::new(child));
    }

    pub fn children(&self) -> ChildIterator<K> {
        ChildIterator { next: to_ref(&self.child) }
    }
}

fn to_ref<K>(tree: &Option<Rc<ParseTree<K>>>) -> Option<&ParseTree<K>> {
    tree.as_ref().map(|t| &**t)
}

///////////////////////////////////////////////////////////////////////////

pub struct ChildIterator<'tree, K: 'tree> {
    next: Option<&'tree ParseTree<K>>
}

impl<'tree, K> Iterator for ChildIterator<'tree, K> {
    type Item = &'tree ParseTree<K>;

    fn next(&mut self) -> Option<&'tree ParseTree<K>> {
        match self.next {
            Some(ptr) => {
                self.next = to_ref(&ptr.sibling);
                Some(ptr)
            }
            None => None,
        }
    }
}

///////////////////////////////////////////////////////////////////////////

impl Span {
    pub fn new(lo: usize, hi: usize) -> Span {
        Span { lo: lo, hi: hi }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_tree {
        ({ $value:expr; $($children:tt)* }) => {
            ParseTree::build($value,
                             DUMMY_SPAN,
                             vec![$(test_tree!($children)),*])
        }
    }

    #[test]
    fn iterate() {
        let tree = test_tree!({22; {44; {66;}} {88;}});
        let kids: Vec<_> = tree.children().map(|x| x.kind).collect();
        assert_eq!(kids, vec![44, 88]);
    }
}
