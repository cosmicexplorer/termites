termites
========

A parser generator for scala.

# Goals

1. Allow creation of expressive parsers using domain-specific syntax.
2. Parse in a lightweight way to real objects using implicit conversions or other tactics.
    - make this suitable for csv parsing as well as compilers
    - "validated" string types (strings that are the result of a parse) can organize text so that the type system forbids creating invalid strings
    - consider easy methods to turn output into traversable tree
3. Integrate tokenization into parsing.
    - give parsers control over their own token stream
    - use meaningful defaults and allow overrides
4. Combine parsers.
    - parser-controlled tokenization makes this difficult
5. Re-entrant, streaming, interruptible parsing with multiple modes of operation.

## Reach Goals

1. Generate code for other JVM languages.
    - not hard
2. Generate efficient automata, not interpreted trees.
    - can clash with parser combination and other goals
3. Speed competitive with popular production-grade parser generators.
    - not really worth it
4. Read flex/bison files.
    - not really worth it
5. Generate code for non-JVM languages.
    - would make it difficult for this to remain a pure library
6. Parallelism of any sort.
    - potential conflicts with automata generation unless we're smart

# The Name is Dumb

It's called termites because termites attack wood and books and paper and whatnot. Using DSLs and parser combinators can be viewed as breaking down a language into smaller parts (smaller parsers).

# License

[GPL](GPL.md)
