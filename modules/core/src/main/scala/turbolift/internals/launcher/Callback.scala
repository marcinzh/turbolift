package turbolift.internals.launcher
import java.util.concurrent.ForkJoinPool
import scala.util.{Try, Success, Failure}
import turbolift.internals.engine.Panic



private[internals] sealed trait Callback[A] extends Function1[Try[A], Unit]:
  def untyped = this.asInstanceOf[Callback.Untyped]
  def success(a: A) = this.apply(Success(a))
  def failure(e: Throwable) = this.apply(Failure(e))


private[internals] object Callback:
  private[turbolift] type Untyped = Callback[Any]

  def apply[A](f: Try[A] => Unit): Callback[A] = new:
    override def apply(aa: Try[A]): Unit = f(aa)


  protected abstract class HasResult[A] extends Callback[A]:
    @volatile protected var result: Try[A] | Null = null

    def get: Try[A] =
      if result != null
      then result.nn
      else Failure(new Panic("Premature attempt to access fiber's result."))

    def panicked: Boolean =
      result match
        case Failure(_ : Panic) => true
        case _ => false


  final class Sync[A] extends HasResult[A]:
    override def apply(aa: Try[A]): Unit = { result = aa }


  final class Notify[A] extends HasResult[A] with ForkJoinPool.ManagedBlocker:
    override def apply(aa: Try[A]): Unit =
      synchronized {
        result = aa
        notify()
      }

    def await(): Unit = synchronized { if result == null then wait() }

    override def isReleasable: Boolean = result != null
    override def block: Boolean = { await(); true }

    def blocking(): Unit = ForkJoinPool.managedBlock(this)
