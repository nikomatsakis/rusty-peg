use std::mem;

#[derive(Debug)]
pub struct ParseTree<K> {
    pub kind: K,
    pub span: Span,
    pub child: Option<Box<ParseTree<K>>>,
    pub sibling: Option<Box<ParseTree<K>>>,
}

#[derive(Debug)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

pub const DUMMY_SPAN: Span = Span { lo: 0, hi: 0 };

///////////////////////////////////////////////////////////////////////////

impl<K> ParseTree<K> {
    fn build(value: K, span: Span, mut children: Vec<ParseTree<K>>) -> ParseTree<K> {
        let mut tree = ParseTree::new(value, span);
        children.reverse();
        for child in children {
            tree.add_child_front(child);
        }
        tree
    }

    pub fn new(kind: K, span: Span) -> ParseTree<K> {
        ParseTree { kind: kind, span: span, child: None, sibling: None }
    }

    pub fn add_child_front(&mut self, mut child: ParseTree<K>) {
        assert!(child.sibling.is_none());
        let old_child = mem::replace(&mut self.child, None);
        child.sibling = old_child;
        self.child = Some(Box::new(child));
    }

    pub fn add_child_back(&mut self, new_child: ParseTree<K>) {
        assert!(new_child.sibling.is_none());
        match self.child {
            None => { self.child = Some(Box::new(new_child)); }
            Some(ref mut child) => { child.add_sibling_back(new_child); }
        }
    }

    pub fn add_sibling_back(&mut self, new_sibling: ParseTree<K>) {
        match self.sibling {
            None => { self.sibling = Some(Box::new(new_sibling)); }
            Some(ref mut sibling) => { sibling.add_sibling_back(new_sibling); }
        }
    }

    pub fn children(&self) -> ChildIterator<K> {
        ChildIterator { next: to_ref(&self.child) }
    }
}

fn to_ref<K>(tree: &Option<Box<ParseTree<K>>>) -> Option<&ParseTree<K>> {
    tree.as_ref().map(|t| &**t)
}

fn to_mut_ref<K>(tree: &mut Option<Box<ParseTree<K>>>) -> Option<&mut ParseTree<K>> {
    tree.as_mut().map(|t| &mut **t)
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
        let kids = tree.children().map(|x| x.kind).collect();
        assert_eq!(kids, vec![44, 88]);
    }
}
