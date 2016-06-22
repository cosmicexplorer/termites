termites
========

A parser generator for scala.

# Goals

1. Allow creation of expressive parsers using domain-specific syntax.
2. Parse in a lightweight way to real objects using implicit conversions or other tactics.
3. Integrate tokenization into parsing.
4. Combine parsers.
5. Generate efficient automata, not interpreted trees.
6. Re-entrant, streaming, interruptible parsing with multiple modes of operation for the generated parser.

## Reach Goals

1. Speed competitive with popular production-grade parser generators.
2. Generate code for other JVM languages.
2. Generate code for non-JVM languages.
3. Parallelism of any sort.

# The Name is Dumb

It's called termites because termites attack wood and books and paper and whatnot. Using DSLs and parser combinators can be viewed as breaking down a language into smaller parts (smaller parsers).

# License

[GPL](GPL.md)
