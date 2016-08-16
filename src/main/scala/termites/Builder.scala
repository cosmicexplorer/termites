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
  case class PopMany(n: Int) extends StackOp {
    require(n > 0, "must pop more than one element from stack")
  }
  object NoOp extends StackOp

  // all that matters for states is whether they're the same
  class State

  // TODO: update for things-not-strings!
  case class Token(ch: Char)

  case class StackTransition(stackTop: StackSymbol, op: StackOp)

  // tok = None means epsilon transition, stackTop = None means this transition
  // applies for any stack symbol
  sealed abstract class Transition(val from: State, val to: State)
  class Epsilon(f: State, t: State) extends Transition(f, t)
  class StacklessTransition(
    f: State,
    t: State,
    val tok: Token
  ) extends Transition(f, t)
  class PureStackTransition(
    f: State,
    t: State,
    val op: StackOp
  ) extends Transition(f, t)
  class PureStackWithTopTransition(
    f: State,
    t: State,
    val trans: StackTransition
  ) extends Transition(f, t)
  class FullTransition(
    f: State,
    t: State,
    val tok: Token,
    val trans: StackTransition
  ) extends Transition(f, t)

  // edge list representation
  // no validation that any transitions contain start or fin
  case class ENPDA(
    start: State,
    fin: Set[State],
    transitions: Seq[Transition]
  ) {
    /* binary operators */
    def then(next: ENPDA): ENPDA = {
      val newTrans = fin.toSeq.map(st => new Epsilon(st, next.start))
      ENPDA(
        start, next.fin,
        newTrans ++ transitions ++ next.transitions)
    }

    def or(other: ENPDA): ENPDA = {
      val newStart = new State
      val startTrans = Seq(
        new Epsilon(newStart, start), new Epsilon(newStart, other.start))
      ENPDA(
        newStart, fin ++ other.fin,
        startTrans ++ transitions ++ other.transitions)
    }

    /* unary operators */
    // *
    def star: ENPDA = plus.maybe
    // +
    def plus: ENPDA = {
      val newTrans = fin.toSeq.map(st => new Epsilon(st, start))
      ENPDA(start, fin, newTrans ++ transitions)
    }
    // ?
    def maybe: ENPDA = {
      val newTrans = fin.toSeq.map(st => new Epsilon(start, st))
      ENPDA(start, fin, newTrans ++ transitions)
    }
    // {n}
    def multiple(times: Int): ENPDA = {
      require(times >= 0, "cannot perform a negative multiple of a PDA")
      if (0 == times) return Empty
      val newSym = new StackSymbol
      val newEnd = new State
      val newTrans = fin.toSeq.flatMap { st => Seq(
        new PureStackTransition(st, start, Push(newSym)),
        new PureStackWithTopTransition(
          st, newEnd, new StackTransition(newSym, PopMany(times)))
      )}
      ENPDA(start, Set(newEnd), newTrans ++ transitions)
    }
    // {m, n}
    def range(from: Int, to: Int): ENPDA = {
      require(to >= from,
        "greater bound of multiple must be greater than lesser bound")
      multiple(from).then(maybe.multiple(from - to))
    }
    def complement: ENPDA = ENPDA(start, getStates -- fin, transitions)

    /* utilities */
    private def getStates: Set[State] =
      fin + start ++ transitions.foldLeft(Set.empty[State]) { (set, trans) =>
        set + trans.from + trans.to
      }
  }
  val Empty: ENPDA = {
    val newSt = new State
    ENPDA(newSt, Set(newSt), Seq())
  }

  def fromLiteral(from: String): ENPDA = from.foldLeft(Empty) { (pda, char) =>
    val st = new State
    val end = new State
    val trans = new StacklessTransition(st, end, Token(char))
    pda.then(ENPDA(st, Set(end), Seq(trans)))
  }

  // TODO: do regex when we can make fast PDAs!
}
