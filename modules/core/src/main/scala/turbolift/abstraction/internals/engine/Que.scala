package turbolift.abstraction.internals.engine
import turbolift.abstraction.!!

sealed trait Que[M[_], U]

object Que {
  def empty[M[_], U] = empty_.asInstanceOf[Que[M, U]]
  private val empty_ = QueCases.Empty[Lambda[X => X], Any]
}

object QueCases {
  case class Empty[M[_], U]() extends Que[M, U]
  case class SeqStep[M[_], U, A](fun: Any => A !! U, next: Que[M, U]) extends Que[M, U]
  case class ParStepLeft[M[_], U, A](todoRight: A !! U, next: Que[M, U]) extends Que[M, U]
  case class ParStepRight[M[_], U, A](doneLeft: M[A], next: Que[M, U]) extends Que[M, U]
}
