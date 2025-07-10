package turbolift.data
import scala.collection.immutable.Queue
import turbolift.!!
import turbolift.Extensions._


/** Queue of continuations.
 *
 * Internal state of breadth-first handler of [[turbolift.effects.Choice]] effect.
 */
final case class QueVector[A, U](todo: Queue[QueVector.Cont[A, U]], done: Vector[A]):
  def drain: Vector[A] !! U =
    todo.dequeueOption match
      case None => done.pure_!!
      case Some(head, tail) => head(copy(todo = tail))

  def addTodo(xs: IterableOnce[QueVector.Cont[A, U]]): QueVector[A, U] = copy(todo = todo ++ xs)
  def addDone(a: A): QueVector[A, U] = copy(done = done :+ a)


object QueVector:
  def empty[A, U] = QueVector[A, U](Queue(), Vector())
  type Cont[A, U] = QueVector[A, U] => Vector[A] !! U
