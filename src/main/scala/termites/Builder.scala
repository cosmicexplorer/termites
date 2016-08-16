package termites

/* Phase 1: make a DSL for generating PDAs

we're gonna generate some utility classes that'll be used to create
epsilon-nondeterministic-PDAs. we'll use an adjacency list representation to
easily combine PDAs and then convert that to a faster lookup mechanism when
converting to a minimal deterministic PDA.

there are multiple ways of doing this. i like the idea of creating an algebra of
PDAs which map back to something like the algebra of regular expressions (which
is pretty interesting in its own right, although i'm not sure it's a "real"
algebra or whatever the term is). the parser dsl contains "expressions" and
"operators". expressions are valid (epsilon-nondeterministic-)PDAs, and
operators can be unary or binary. all operators are similar to operators found
in regular expressions, although we'll also allow some sugar (e.g. ? and
+). backreferences are hard to cleanly integrate into this, so we'll leave that
off the table for now (although they can be useful since they are more general
than just CFGs).

what does that mean? we'll see below. we'll develop this enough to be able to
make a function which accepts a string and creates an (e-n-)PDA which accepts
only that string. we'll then extend this to regular expressions (implement
unary/binary operators, and pull PDAs (technically still NFAs) from js native
regexps (we won't allow lookahead/behind for now; these JUST validate whether a
string is in the language!)). finally, we'll create an expressive DSL for
combining parsers with the power of CFGs -- that is, with the ability of
recursive constructions (which requires a stack).
*/

object Builder {
  // all of the below is for ENPDAs (nondeterministic PDAs with epsilon
  // transitions); we'll have different classes for DPDAs

  // decouples identity from location in memory
  object StackSymbol {
    private var idCounter: Long = 0
    // TODO: more scalable uniqueness check! make it thread-safe!
    def getId(): Long = {
      idCounter += 1
      idCounter
    }
  }
  class StackSymbol {
    private val id: Long = StackSymbol.getId()
    // TODO: just override .equal()?
    def isSame(other: StackSymbol): Boolean = id == other.id
  }

  // TODO: add functionality here when converting ENPDA -> DPDA
  sealed trait StackOp
  case class Push(syms: StackSymbol*) extends StackOp
  object Pop extends StackOp

  // TODO: implementation of this is easy, but could be faster / tail recursive
  // to refactor so that can reuse states
  class StateTree(transitions: Transition*) {
    def addTransition(trans: Transition): StateTree =
      new StateTree((trans +: transitions): _*)
    // binary operations
    // NOTE: this is mutually recursive through Transition! can cause stack
    // overflow! also, cycles DEFINITELY cause stack overflow!
    def then(next: StateTree): StateTree =
      new StateTree(transitions.map(_.addStateAfter(next)): _*)
    // TODO: need "or with priority"
    def or(other: StateTree): StateTree =
      new StateTree(Epsilon(this), Epsilon(other))
    // unary operations
    // *
    def star: StateTree = then(new StateTree(Epsilon(this)))
    // +
    def plus: StateTree = then(star)
    // ?
    def maybe: StateTree = or(new FinalState)
  }
  class FinalState(
    transitions: Transition*
  ) extends StateTree(transitions: _*) {
    override def then(next: StateTree): StateTree =
      super.then(next).addTransition(Epsilon(next))
  }

  // TODO: update for things-not-strings!
  case class Token(ch: Char)

  // tok = None means epsilon transition, stackTop = None means this transition
  // applies for any stack symbol
  sealed trait Transition {
    // in all cases when taking the transition leads to a final state, instead
    // go to "next"
    def addStateAfter(next: StateTree): Transition
  }
  case class Epsilon(toState: StateTree) extends Transition {
    override def addStateAfter(next: StateTree): Transition =
      Epsilon(toState.then(next))
  }
  case class FullTransition(
    tok: Token,
    stackTop: Symbol,
    toState: StateTree,
    op: StackOp
  ) extends Transition {
    override def addStateAfter(next: StateTree): Transition =
      FullTransition(tok, stackTop, toState.then(next), op)
  }
}

/* Phase 2: convert (e-n-)PDAs to optimal deterministic PDAs without epsilon
transitions.

This process is pretty standard. We need to ensure we make the data structures
created in Phase 1 useful for this task. The output should be a PDA which
performs an O(1) operation per input token.
*/

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
