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

  // all symbols will just be compared by whether they're the same object
  class StackSymbol

  // TODO: add functionality here when converting ENPDA -> DPDA
  sealed trait StackOp
  case class Push(syms: StackSymbol*) extends StackOp
  object Pop extends StackOp
  object NoOp extends StackOp

  // all that matters for states is whether they're the same
  class State

  // TODO: update for things-not-strings!
  case class Token(ch: Char)

  sealed trait StackTransition
  object Stackless extends StackTransition
  case class StackAware(
    stackTop: StackSymbol, op: StackOp
  ) extends StackTransition

  // tok = None means epsilon transition, stackTop = None means this transition
  // applies for any stack symbol
  sealed abstract class Transition(val from: State, val to: State) {
    def mapStates(map: (State => State)): Transition
  }
  case class Epsilon(f: State, t: State) extends Transition(f, t) {
    override def mapStates(map: (State => State)): Transition =
      Epsilon(map(f), map(t))
  }
  case class FullTransition(
    f: State,
    t: State,
    tok: Token,
    trans: StackTransition
  ) extends Transition(f, t) {
    override def mapStates(map: (State => State)): Transition =
      FullTransition(map(f), map(t), tok, trans)
  }

  // there's only one final state; just use e-transitions to final state
  // edge list representation
  // no validation that any transitions contain start or fin
  case class ENPDA(start: State, fin: State, transitions: Seq[Transition]) {
    /* binary operators */
    def then(next: ENPDA): ENPDA =
      ENPDA(start, next.fin,
        Epsilon(fin, next.start) +: (transitions ++ next.transitions))

    def or(other: ENPDA): ENPDA = {
      val newStart = new State
      val startTrans = Seq(
        Epsilon(newStart, start), Epsilon(newStart, other.start))
      val newEnd = new State
      val endTrans = Seq(
        Epsilon(fin, newEnd), Epsilon(other.fin, newEnd))
      ENPDA(newStart, newEnd,
        startTrans ++ endTrans ++ transitions ++ other.transitions)
    }

    /* unary operators */
    // *
    def star: ENPDA = {
      val newSt = new State
      val newTrans = Seq(Epsilon(newSt, start), Epsilon(fin, newSt))
      ENPDA(newSt, newSt, newTrans ++ transitions)
    }
    // +
    def plus: ENPDA = ENPDA(start, fin, Epsilon(fin, start) +: transitions)
    // ?
    def maybe: ENPDA = ENPDA(start, fin, Epsilon(start, fin) +: transitions)
    // {n}
    def multiple(times: Int): ENPDA = multiple(times, getVisited)
    private def multiple(times: Int, visited: Set[State]): ENPDA = {
      require(times >= 0, "cannot perform a negative multiple of a PDA")
      if (0 == times) {
        ENPDA.MultIdentity
      } else {
        (2 to times).foldLeft(this) { (cur, _) =>
          cur.then(copyWithVisited(visited))
        }
      }
    }
    // {m, n}
    def range(from: Int, to: Int): ENPDA = {
      require(to >= from,
        "greater bound of multiple must be greater than lesser bound")
      val visited = getVisited
      val lowerBound = multiple(from, visited)
      (from + 1 to to).foldLeft(lowerBound) { (cur, _) =>
        cur.then(copyWithVisited(visited).maybe)
      }
    }

    /* utilities */
    private def getVisited: Set[State] =
      transitions.foldLeft(Set.empty[State]) { (set, trans) =>
        set + trans.from + trans.to
      }
    private def copyWithVisited(visited: Set[State]): ENPDA = {
      val stateMap = visited.toSeq.map(_ -> new State).toMap
      val newTrans = transitions.map(_.mapStates(stateMap))
      ENPDA(stateMap(start), stateMap(fin), newTrans)
    }
  }
  object ENPDA {
    val MultIdentity: ENPDA = {
      val newSt = new State
      ENPDA(newSt, newSt, Seq())
    }
  }

  def fromLiteral(from: String): ENPDA =
    from.foldLeft(ENPDA.MultIdentity) { (pda, char) =>
      val st = new State
      val end = new State
      val trans = new FullTransition(st, end, Token(char), Stackless)
      pda.then(ENPDA(st, end, Seq(trans)))
    }
  // TODO: parsing is fun! (vomit)
  def fromRegex(regex: String): ENPDA = ???
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
