package turbolift.internals.engine
import turbolift.!!


private[engine] sealed trait Step[-A, +B, M[_], U]

private[engine] object Step:
  def empty[A, B, M[_], U] = empty_.asInstanceOf[Step[A, B, M, U]]
  private val empty_ = StepCases.Done[Any, [X] =>> X, Any]()


private[engine] object StepCases:
  final case class Done[A, M[_], U]() extends Step[A, A, M, U]
  final case class More[A, B, C, M[_], U](fun: A => B !! U, next: Step[B, C, M, U]) extends Step[A, C, M, U]
  final case class ZipLeft[A, B, C, M[_], U](todoRight: B !! U, next: Step[(A, B), C, M, U]) extends Step[A, C, M, U]
  final case class ZipRight[A, B, C, M[_], U](doneLeft: M[A], next: Step[(A, B), C, M, U]) extends Step[B, C, M, U]
