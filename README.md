Rusty PEG is a set of macros and library code designed to make it very
easy to write parsers in Rust. The name derives from the fact that the
libary is intended to generate parsers that use the
[Parsing Expression Grammar][PEG] scheme.

### A brief tutorial on Rusty PEG

Defining a grammar in Rusty PEG is done through the `rusty_peg!`
macro. This section explains how the macro works by working through a
[calculator example](src/test/calculator.rs): the calculator takes as
input a string like `3+5*11/3+1` and computes the result (`22`),
respecting the order of operations and so forth.

Every use of the `rusty_peg!` macro will wind up defining a particular
type called a *parser* (along with various other types). This parser
can be instantiated and used to parse a particular input. In this
case, we want to call our parser type `Calculator`, as shown here:

```rust
rusty_peg! {
    // Name of the type for the parser you are defining.
    // The macro will generate a struct named, in this case,
    // `Calculator`. This struct will have one lifetime parameter,
    // named `'input`, which represents the lifetime of the
    // input text. This lifetime may appear in the types of nonterminals
    // (which might match a slice of the input, for example) and other
    // such cases.
    parser Calculator<'input> {
        ... // symbol definitions come here; read on
    }
}    
```

Inside the parser definitions come a series of *symbol definitions*. A
*symbol* is just a named part of the grammar that matches against part
of the input and produces some result from that structure (you may be
familiar with the terms "terminal" and "nonterminal" -- a symbol is
effectively the union of the two, since PEG grammars don't draw a
distinction between the tokenizer and the parser.). This result could
be a data structure like an AST in a compiler or something else.  In
the case of our calculator, the output is the result of the
calcualtion, so the result type will be `u32`.

```rust
rusty_peg! {
    parser Calculator<'input> {
    
        // Every symbol definition starts out out with a name (here, `EXPR`)
        // and a type (here, `u32`), an `=` sign, and then a definition.
        // There are various kinds of definitions you can do. This first
        // definition is the simplest; it just defines a kind of shorthand,
        // saying that an "EXPR" is the same as an "ADD_SUB_EXPR" (another
        // symbol, defined below).
        EXPR: u32 = ADD_SUB_EXPR;
        
        // This is an example of a "map" nonterminal. "Map" first match a
        // series of other strings and symbols, and then allow you to
        // write some arbitraryRust code that will execute and produce the
        // result. In this case, we are defining `PAREN_EXPR`, which will
        // match an open parentheses, an arbirtary expression, and then
        // a close parentheses. The notation `<e:EXPR>` defines a variable
        // (`e`) which can be referenced in the expression code.
        // In this case, the expression is just `e`, meaning that the value
        // of a paren expression is the same as the expression without the parens.
        PAREN_EXPR: u32 = ("(", <e:EXPR>, ")") => e;

        // This is an example of a "fold" nonterminal. Fold nonterminals are a
        // special form that first match one instance of some base form (in this case,
        // `<lhs:MUL_DIV_EXPR>`) and then match zero or more instances of various
        // extensions. This is a common pattern that arises in expressions
        // in particular. Each time an extension is parsed, a little bit of custom
        // code runs to produce a new result. In this case, `ADD_SUB_EXPR` is
        // basically parsing a series like:
        //
        //     MUL_DIV_EXPR + MUL_DIV_EXPR - MUL_DIV_EXPR
        //
        // The tiered structure you see here is intended to enforce the
        // order of operations: all multiplications and divisions will be performed
        // before we parse the `+` and `-` operators.
        ADD_SUB_EXPR: u32 =
            fold(<lhs:MUL_DIV_EXPR>,
                 ("+", <rhs:MUL_DIV_EXPR>) => { lhs + rhs },
                 ("-", <rhs:MUL_DIV_EXPR>) => { lhs - rhs });

        // Another fold definition, this time for `*` and `/`.
        MUL_DIV_EXPR: u32 =
            fold(<lhs:ATOM_EXPR>,
                 ("*", <rhs:ATOM_EXPR>) => { lhs * rhs },
                 ("/", <rhs:ATOM_EXPR>) => { lhs / rhs });

        // `ATOM_EXPR` is the base expression form. It uses the `/` operator, which
        // is called "ordered choice". Basically the parser will attempt the various
        // options listed, in order, and take the first option which matches.
        //
        // Note that `/` plays the same role as the `|` operator in traditional CFGs,
        // but it has quite different semantics.
        //
        // IT IS IMPORTANT TO ORDER YOUR CHOICES CORRECTLY!
        ATOM_EXPR: u32 =
            (NUMBER / PAREN_EXPR);

        // The next two symbols define a number. This is done by combining
        // a "regex" symbol (the final kind of symbol) with a map. Currently this
        // cannot be done in one step, though it seems like it'd be a nice addition. :)
        //
        // Regex symbols match a given regular expression against the input. They
        // always produce the type `&'input str` -- which is a slice from the input
        // string. They are based on the regex crate.
        NUMBER: u32 =
            (<s:NUMBER_STRING>) => u32::from_str(s).unwrap();

        NUMBER_STRING: &'input str =
            regex(r"[0-9]+");
    }
}
```

### Running the parser

Currently the parser is executed by creating an instance of the parser
type and invoking the `parse_complete` method on one of the symbol types,
as shown in this test:

```rust
#[test]
fn parse_expr() {
    let mut classy = Calculator::new(());
    let result =
        EXPR.parse_complete(&mut classy, "3 + 5*11/3 + 1")
            .unwrap();
    assert_eq!(result, 22);
}
```

### Caching

Part of how PEGs work is that they cache intermediate results. This
implies that all of your symbol types must be cloneable, and cloning
ought to be cheap, because every symbol will be stored in a cache and
may be cloned out of that cache many times. Use `Rc` if necessary.

### Symbol kinds in more detail

There actually isn't much more to say than what you see above, but
there are a few bits and pieces. First, let's define a grammar for
symbol definitions. We'll use a modified form of BNF.

``` 
SYMBOL = IDENTIFIER ":" TYPE "=" DEFINITION ";"
DEFINITION = ITEM ["=>" EXPR]
           | "regex" "(" EXPR ")" ["-" "[" EXPR {"," EXPR} "]"]
           | "fold" "(" "<" IDENTIFIER ":" ITEM ">",
                        ITEM "=>" EXPR,
                        {"," ITEM "=>" EXPR} ")"
```

#### Items and items lists

The above definitions frequently reference "ITEM", which are the
building blocks of our grammar definitions. The various kinds of items
are as follows:

- identifier like `EXPR` or `NUMBER`: name of a symbol type, either
  defined within the grammar of by the user (se custom symbol types
  below).
- string like `"foo"`: matches that precise string.
- `(ITEMS)`: matches the items list `ITEMS` (see below for item lists)
- `[ITEMS]`: matches the items list `ITEMS`, but optionally. Has the type `Option<T>`
  where `T` is the type of the items list.
- `{ITEMS}`: matches the items list `ITEMS` zero or more times, and has the type
  `Vec<T>`, where `T` is the type of the items list.
- `{+ ITEMS}`: matches the items list `ITEMS` one or more times, and has the type
  `Vec<T>`, where `T` is the type of the items list.

An *items list* `ITEMS` has the form of a single item `ITEM` or else a
comma-separated list like `ITEM0, ITEM1, ITEM2`. The latter has the
type `(T0,T1,T2)` where `Tn` is the type of `ITEMn`.
  
- `[ITEM]` (or `[ITEM, ITEM]`) is an optional match of an item. It has the type
  `Option<T>` where `T` is the type of `ITEM`.
- `{ITEM,}` defines zero-or-more instances 

#### Defining custom symbol kinds

It is possible to define custom symbol kinds by implementing the
`Symbol` trait manually. Docs "coming soon" (or not so soon, as case
may be), or at least a test demonstrating how it is done.

#### Regular expression symbols

Regular expression symbols can also include an optional "exclusion
list", as seen in [the `Classy` example](src/test/classy.rs). This is 
mostly used to exclude keywords from the identifier list:

```
        ID: &'input str =
            regex(r"^[a-zA-Z_][a-zA-Z_0-9]*") - ["class"];
```

### Future directions / Want to help?

This library is uber-unstable. I'd love to get feedback. And if you
have an idea how for how Rusty PEG could be improved, I'd love to hear
it (or see a PR, for that matter).  Here are some thoughts I had on
changes I might make in the future:

- Remove the need for the comma operator; this is blocked on a bug in rustc which has been
  fixed but not yet made it out to a stable release
- Separate construction of the *parser* from construction of the cache.
- Perhaps have the macro generate helper functions to do the parser so
  the user has to write less annoying stuff to invoke the parser.
- Pay more attention to error messages and allow user to customize error recovery
  more.
- Perhaps provide a way to stop implicitly skipping whitespace everywhere.
- Add the `&` and `!` operators.
- Generalize the macro a bit, for example allowing `=>` map expressions uniformly
  (e.g., on regular expressions, and on the left-hand-side of a fold).
- Easier way to define lists of keywords.
- Some way to check libraries for ambiguity (in other words, to check that `/` and `|` operator
  are equivalent).

I'm also just interested in exploring bottom-up parsers and split
tokenizers etc, but that'd be quite a different thing and would
probably just be a different project.

[PEG]: http://en.wikipedia.org/wiki/Parsing_expression_grammar
