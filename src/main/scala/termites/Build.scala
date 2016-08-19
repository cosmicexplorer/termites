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

object Build {
  // all of the below is for ENPDAs (nondeterministic PDAs with epsilon
  // transitions); we'll have different classes for DPDAs

  // all symbols will just be compared by whether they're the same object
  class StackSymbol

  // TODO: add functionality here when converting ENPDA -> DPDA
  sealed trait StackOp {
    def stackSyms: Set[StackSymbol] = Set()
    def invoke(stack: List[StackSymbol]): List[StackSymbol]
  }
  // syms are added as if the leftmost is pushed first, then rightwards
  class Push(syms: StackSymbol*) extends StackOp {
    val symsStackDelta = syms.reverse.toList
    override def stackSyms: Set[StackSymbol] = syms.toSet
    override def invoke(stack: List[StackSymbol]): List[StackSymbol] =
      symsStackDelta ::: stack
  }
  object Pop extends StackOp {
    override def invoke(stack: List[StackSymbol]): List[StackSymbol] =
      stack.tail // TODO: don't let this create an empty stack!
  }
  // TODO: this needs to ensure it's popping the right symbols, not just any
  class PopMany(n: Int) extends StackOp {
    require(n >= 1, "must pop more at least one element from stack")
    // TODO: don't let this create an empty stack! also, drop will just return
    // an empty list if more than <size> elements are dropped!
    override def invoke(stack: List[StackSymbol]): List[StackSymbol] =
      stack.drop(n)
  }
  object NoOp extends StackOp {
    override def invoke(stack: List[StackSymbol]): List[StackSymbol] = stack
  }

  // all that matters for states is whether they're the same
  class State

  case class Token[T](ch: T)

  case class StackTransition(stackTop: StackSymbol, op: StackOp)

  // tok = None means epsilon transition, stackTop = None means this transition
  // applies for any stack symbol
  sealed abstract class Transition[T](val from: State, val to: State) {
    def tokens: Set[Token[T]] = Set()
    def stackSyms: Set[StackSymbol] = Set()
  }
  class Epsilon[T](f: State, t: State) extends Transition[T](f, t)
  class StacklessTransition[T](
    f: State,
    t: State,
    val tok: Token[T]
  ) extends Transition[T](f, t) {
    override def tokens: Set[Token[T]] = Set(tok)
  }
  class PureStackTransition[T](
    f: State,
    t: State,
    val op: StackOp
  ) extends Transition[T](f, t) {
    override def stackSyms: Set[StackSymbol] = op.stackSyms
  }
  class PureStackWithTopTransition[T](
    f: State,
    t: State,
    val trans: StackTransition
  ) extends Transition[T](f, t) {
    override def stackSyms: Set[StackSymbol] =
      trans.op.stackSyms + trans.stackTop
  }
  class FullTransition[T](
    f: State,
    t: State,
    val tok: Token[T],
    val trans: StackTransition
  ) extends Transition(f, t) {
    override def tokens: Set[Token[T]] = Set(tok)
    override def stackSyms: Set[StackSymbol] =
      trans.op.stackSyms + trans.stackTop
  }

  // edge list representation
  // no validation that any transitions contain start or fin
  class ENPDA[T](
    val start: State,
    val fin: Set[State],
    val transitions: Seq[Transition[T]]
  ) {
    /* binary operators */
    def then(next: ENPDA[T]): ENPDA[T] = {
      val newTrans = fin.toSeq.map(st => new Epsilon[T](st, next.start))
      new ENPDA[T](
        start, next.fin,
        newTrans ++ transitions ++ next.transitions)
    }

    def or(other: ENPDA[T]): ENPDA[T] = {
      val newStart = new State
      val startTrans = Seq(
        new Epsilon[T](newStart, start), new Epsilon[T](newStart, other.start))
      new ENPDA[T](
        newStart, fin ++ other.fin,
        startTrans ++ transitions ++ other.transitions)
    }

    /* unary operators */
    // *
    def star: ENPDA[T] = plus.maybe
    // +
    def plus: ENPDA[T] = {
      val newTrans = fin.toSeq.map(st => new Epsilon[T](st, start))
      new ENPDA[T](start, fin, newTrans ++ transitions)
    }
    // ?
    def maybe: ENPDA[T] = {
      val newTrans = fin.toSeq.map(st => new Epsilon[T](start, st))
      new ENPDA[T](start, fin, newTrans ++ transitions)
    }
    // {n}
    // TODO: using a stack symbol to do multiples is a bit unorthodox. it's
    // easier to implement, but are there perf issues? (i think not)
    def multiple(times: Int): ENPDA[T] = {
      require(times >= 0, "cannot perform a negative multiple of a PDA")
      if (0 == times) return ENPDA.Empty[T]
      val newSym = new StackSymbol
      val newEnd = new State
      val newTrans = fin.toSeq.flatMap { st => Seq(
        new PureStackTransition[T](st, start, new Push(newSym)),
        new PureStackWithTopTransition[T](
          st, newEnd, new StackTransition(newSym, new PopMany(times)))
      )}
      new ENPDA[T](start, Set(newEnd), newTrans ++ transitions)
    }
    // {m, n}
    def range(from: Int, to: Int): ENPDA[T] = {
      require(to >= from,
        "greater bound of multiple must be greater than lesser bound")
      multiple(from).then(maybe.multiple(from - to))
    }
    // TODO: this is wrong; we need to figure out how DPDAs handle lack of an
    // action at the given stack / state for this to work
    def complement: ENPDA[T] =
      new ENPDA[T](start, enumerate.states -- fin, transitions)

    /* utilities */
    def enumerate: Enumerated[T] = {
      val folded = transitions.foldLeft(Enumerated.empty[T]) { (cur, trans) =>
        Enumerated(
          cur.states + trans.from + trans.to,
          cur.tokens ++ trans.tokens,
          cur.syms ++ trans.stackSyms)
      }
      Enumerated(folded.states + start ++ fin, folded.tokens, folded.syms)
    }
  }

  object ENPDA {
    def Empty[T]: ENPDA[T] = {
      val newSt = new State
      new ENPDA[T](newSt, Set(newSt), Seq())
    }
  }

  case class Enumerated[T](
    states: Set[State], tokens: Set[Token[T]], syms: Set[StackSymbol])
  object Enumerated {
    def empty[T]: Enumerated[T] = Enumerated(Set.empty, Set.empty, Set.empty)
  }

  def fromLiteral[T](from: Traversable[T]): ENPDA[T] =
    from.foldLeft(Empty[T]) { (pda, char) =>
      val st = new State
      val end = new State
      val trans = new StacklessTransition[T](st, end, Token(char))
      pda.then(new ENPDA[T](st, Set(end), Seq(trans)))
    }

  // TODO: do regex when we can make fast PDAs!
}
