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
  class ParserCreationError(msg: String)
      extends Exception(msg)

  // all symbols will just be compared by whether they're the same object
  class StackSymbol

  // all that matters for states is whether they're the same
  class State

  case class Token[T](ch: T)

  sealed trait StackOperation {
    // whether a specific symbol is required at top of stack to transition
    val stackTop: Option[StackSymbol]
    def invoke(stack: List[StackSymbol]): List[StackSymbol]
    // used in DPDA creation; None implies the transition should be removed
    def addSymbolBefore(sym: StackSymbol): Option[StackOperation]
    def stackSyms: Set[StackSymbol]
  }
  // syms are added as if the leftmost is pushed first, then rightwards
  class Push(top: Option[StackSymbol], syms: StackSymbol*)
      extends StackOperation {
    val symsStackDelta = syms.reverse.toList
    override val stackTop: Option[StackSymbol] = top
    override def invoke(stack: List[StackSymbol]): List[StackSymbol] =
      symsStackDelta ::: stack
    override def addSymbolBefore(sym: StackSymbol): Option[StackOperation] =
      Some(new Push(top, (sym +: syms): _*))
    override def stackSyms: Set[StackSymbol] = syms.toSet ++ top.toSet
  }
  class Pop(top: StackSymbol) extends StackOperation {
    override val stackTop: Option[StackSymbol] = Some(top)
    override def invoke(stack: List[StackSymbol]): List[StackSymbol] = {
      // TODO: remove contracts in prod builds somehow?
      require(!stack.isEmpty, "can't pop empty stack")
      stack.tail
    } ensuring(!_.isEmpty, "popping stack should never be empty")
    override def addSymbolBefore(sym: StackSymbol): Option[StackOperation] =
      if (top == sym) Some(NoOp) else None
    override def stackSyms: Set[StackSymbol] = Set(top)
  }
  object NoOp extends StackOperation {
    override val stackTop: Option[StackSymbol] = None
    override def invoke(stack: List[StackSymbol]): List[StackSymbol] = stack
    override def addSymbolBefore(sym: StackSymbol): Option[StackOperation] =
      Some(new Push(None, sym))
    override def stackSyms: Set[StackSymbol] = Set.empty
  }

  case class Transition[T](
    from: State,
    to: State,
    token: Option[Token[T]] = None,
    trans: Option[Transition[T]] = None
  ) {
    def stackSyms: Set[StackSymbol] =
      trans.map(_.stackSyms).getOrElse(Set.empty)
    def mapStates(map: (State => State)): Transition[T] =
      Transition[T](map(from), map(to), token, trans)
  }
  object Transition {
    def Epsilon[T](from: State, to: State): Transition[T] = Transition(from, to)
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
      val newTrans = fin.toSeq.map(st => Transition.Epsilon[T](st, next.start))
      new ENPDA[T](
        start,
        next.fin,
        newTrans ++ transitions ++ next.transitions)
    }

    def or(other: ENPDA[T]): ENPDA[T] = {
      val newStart = new State
      val startTrans = Seq(
        Transition.Epsilon[T](newStart, start),
        Transition.Epsilon[T](newStart, other.start))
      new ENPDA[T](
        newStart,
        fin ++ other.fin,
        startTrans ++ transitions ++ other.transitions)
    }

    /* unary operators */
    // *
    def star: ENPDA[T] = plus.maybe
    // +
    def plus: ENPDA[T] = {
      val newTrans = fin.toSeq.map(st => Transition.Epsilon[T](st, start))
      new ENPDA[T](start, fin, newTrans ++ transitions)
    }
    // ?
    def maybe: ENPDA[T] = {
      val newTrans = fin.toSeq.map(st => Transition.Epsilon[T](start, st))
      new ENPDA[T](start, fin, newTrans ++ transitions)
    }
    // {n}
    def multiple(times: Int): ENPDA[T] = {
      require(times >= 0, "cannot perform a negative multiple of a PDA")
      val visited = enumerate.states
      (2 to times).foldLeft(this) { (cur, _) =>
        cur.then(copyWithVisited(visited))
      }
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
          cur.tokens ++ trans.token,
          cur.syms ++ trans.stackSyms)
      }
      Enumerated(folded.states + start ++ fin, folded.tokens, folded.syms)
    }
    def copyWithVisited(visited: Set[State]): ENPDA[T] = {
      val stateMap = visited.toSeq.map(_ -> new State).toMap
      val newStart = stateMap(start)
      val newFin = fin.map(stateMap)
      val newTrans = transitions.map(_.mapStates(stateMap))
      new ENPDA[T](newStart, newFin, newTrans)
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
    from.foldLeft(ENPDA.Empty[T]) { (pda, char) =>
      val st = new State
      val end = new State
      val trans = new Transition[T](st, end, Some(Token(char)))
      pda.then(new ENPDA[T](st, Set(end), Seq(trans)))
    }

  // TODO: do regex when we can make fast PDAs!
}
