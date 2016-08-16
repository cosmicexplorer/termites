package termites

/* Phase 2: convert (e-n-)PDAs to optimal deterministic PDAs without epsilon
transitions.

This process is pretty standard. We need to ensure we make the data structures
created in Phase 1 useful for this task. The output should be a PDA which
performs an O(1) operation per input token.
*/
object Parser {
  
}

/* Phase 3: add API to parse strings into objects.

In jison, you can return an arbitrary javascript object upon successful parsing
of each construction. This looks like (using the new EBNF-like syntax):

my_other_construction:
  'hey'
  ;
my_construction:
  my_other_construction 'wow'
    { $$ = new MyObject($1, $2); }
  ;
start_symbol:
  my_construction
    { return $1; }
  ;

The resulting parser would accept only the string "heywow" (modulo some lexer
stuff), and return a new MyObject constructed with arguments "hey" and
"wow". Each construction by default "returns" a string, but as shown above, they
can also be used to construct arbitrary objects. This makes construction of even
complex ASTs relatively easy, because each construction can map 1-1 to an
arbitrary javascript object, with an easy API.

Why not just use jison?
1. doesn't need to generate code (while it is still efficient)
2. there are no magic variables such as $$, $1, etc.
3. each parser has an associated object that it parses to, so each parser is
"real"; you don't need to have a separate bison file for each language you parse
from. This makes reuse of grammars easier. Because each parser "owns" its own
language, and parsers can be combined, parse trees and error messages can be
made clearer.
4. tokenization and parsing are integrated.
5. doesn't use LL/LR algorithm, so no unnecessary conflicts except where the
grammar is actually ambiguous; solves the C lexer hack
*/

/* Phase 4: add nice things (backreferences, streaming API, parse things other
than strings)

- backreferences :: implement a simple syntax for backreferences
- streaming :: Sometimes you want to parse a string all at once. Other times,
  you may be dealing with a stream of values which either comes in slowly, or
  that you may not want to maintain in memory all at once. For this reason, we
  want an API for reentrant/interruptible parsing, which essentially consumes
  all of the text in a given chunk and advances the PDA, which may either
  reject the stream or be in an intermediate state. at the end of the stream, if
  the PDA is in a final state, we return the result. in addition, we want to
  create an easy API for a stream to be parsed into a sequence of values, such
  that no objects from previous elements in the sequence are kept in memory, and
  we can produce a stream of objects as each are parsed (THIS IS REALLY COOL).
- things other than strings :: transitions currently exist as "given this state
  and top of the stack, if the current token is equal to <x>, then transition to
  next state and modify the stack". parser combination is a powerful concept; we
  can very easily change "current token" to "current arbitrary object", as long
  as we have some (quick-to-check) notion of equality. we could go overboard and
  use this concept to implement parser combination in the first place, but
  that'd be going a bit overboard
*/
