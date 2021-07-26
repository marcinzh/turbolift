package turbolift.abstraction.internals.engine
import turbolift.abstraction.!!


sealed trait Step[-A, +B, M[_], U]

object Step:
  def empty[A, B, M[_], U] = empty_.asInstanceOf[Step[A, B, M, U]]
  private val empty_ = StepCases.Empty[Any, [X] =>> X, Any]()


object StepCases:
  final case class Empty[A, M[_], U]() extends Step[A, A, M, U]
  final case class SeqStep[A, B, C, M[_], U](fun: A => B !! U, next: Step[B, C, M, U]) extends Step[A, C, M, U]
  final case class ParStepLeft[A, B, C, M[_], U](todoRight: B !! U, next: Step[(A, B), C, M, U]) extends Step[A, C, M, U]
  final case class ParStepRight[A, B, C, M[_], U](doneLeft: M[A], next: Step[(A, B), C, M, U]) extends Step[B, C, M, U]
