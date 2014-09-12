# Rowling

#### A simple expression language with static typing and row types.

Rowling has static types and type inferrence via a modified version of Hindley-Milner. It supports algebraic types a la Haskell, as well as row types (record types). Rowling's row types are particularly well-suited to JSON structures; for example, the JSON blob `{foo: 1, bar: "hello", baz: [2, 3]}` could be considered to have the row type `(foo: Int, bar: String, baz: [Int])`. In fact, rowling can operate on JSON structures "out of the box". Rowling has powerful pattern-matching.
