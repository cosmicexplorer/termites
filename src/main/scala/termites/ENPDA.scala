package termites

class StackSym

case class StackOp(
  toPop: Option[StackSym],
  toPush: Seq[StackSym]
) {
  // TODO: add functions to push stack transitions onto this!
}

class State

case class Transition[T](
  from: State,
  to: State,
  ch: Option[T] = None,
  op: Option[StackOp] = None
) {
  // TODO: add functions to push stack transitions onto this!
}

class NonterminalId

class ENPDA[T](
  val start: State,
  val end: State,
  val all: Set[Transition[T]],
  val ids: Set[NonterminalId]
) {
  // TODO: make this faster by not recomputing each time
  val byFrom: Map[State, Set[Transition[T]]] = all.groupBy(_.from)
  val byTo: Map[State, Set[Transition[T]]] = all.groupBy(_.to)

  def ->(rhs: ENPDA[T]): ENPDA[T] = {
    // ->(this)-(rhs)->
    val extended = Transition[T](end, rhs.start)
    val newTrans = all ++ rhs.all + extended
    new ENPDA[T](start, rhs.end, newTrans, ids ++ rhs.ids)
  }

  def |(rhs: ENPDA[T]): ENPDA[T] = {
    /*         (this)
              /      \
     ->(start)        (end)->
              \      /
               (rhs )
     */
    val newStart = new State
    val toThis = Transition[T](newStart, start)
    val toThat = Transition[T](newStart, rhs.start)

    val newEnd = new State
    val fromThis = Transition[T](end, newEnd)
    val fromThat = Transition[T](rhs.end, newEnd)

    val newTrans = all ++ rhs.all + toThis + toThat + fromThis + fromThat
    new ENPDA[T](newStart, newEnd, newTrans, ids ++ rhs.ids)
  }

  def !(): ENPDA[T] = ???
}
