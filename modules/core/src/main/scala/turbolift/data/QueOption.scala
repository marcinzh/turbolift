package turbolift.data
import scala.collection.immutable.Queue
import turbolift.!!
import turbolift.Extensions._


/** Queue of continuations.
 *
 * Internal state of breadth-first handler of [[turbolift.effects.Choice]] effect.
 */
final case class QueOption[A, U](todo: Queue[QueOption.Cont[A, U]]) extends AnyVal:
  def drain: Option[A] !! U =
    todo.dequeueOption match
      case None => !!.none
      case Some(head, tail) => head(copy(todo = tail))

  def addTodo(xs: IterableOnce[QueOption.Cont[A, U]]): QueOption[A, U] = copy(todo = todo ++ xs)


object QueOption:
  def empty[A, U] = QueOption[A, U](Queue())
  type Cont[A, U] = QueOption[A, U] => Option[A] !! U
