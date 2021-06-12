package turbolift.abstraction.internals.engine
import turbolift.abstraction.!!

sealed trait Step[M[_], U]

object Step {
  def empty[M[_], U] = empty_.asInstanceOf[Step[M, U]]
  private val empty_ = StepCases.Empty[Lambda[X => X], Any]()
}

object StepCases {
  case class Empty[M[_], U]() extends Step[M, U]
  case class SeqStep[M[_], U, A](fun: Any => A !! U, next: Step[M, U]) extends Step[M, U]
  case class ParStepLeft[M[_], U, A](todoRight: A !! U, next: Step[M, U]) extends Step[M, U]
  case class ParStepRight[M[_], U, A](doneLeft: M[A], next: Step[M, U]) extends Step[M, U]
}
