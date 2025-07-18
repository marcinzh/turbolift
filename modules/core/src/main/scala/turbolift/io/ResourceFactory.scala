package turbolift.io
import java.io.{Closeable => JCloseable}
import turbolift.!!
import turbolift.effects.{IO, Resource}


/** To be used with [[turbolift.effects.ResourceEffect ResourceEffect]]. */
trait ResourceFactory[A, U]:
  def acquire: A !! U
  def release(value: A): Unit !! U


object ResourceFactory:
  def apply[A, U](acquire: A !! U, release: A => Unit !! U): ResourceFactory[A, U] =
    val acq = acquire
    val rel = release
    new ResourceFactory[A, U]:
      override def acquire = acq
      override def release(a: A) = rel(a)

  extension [A](thiz: ResourceFactory[A, IO])
    def use: A !! Resource = Resource.use(thiz)

  trait Closeable[A <: JCloseable] extends ResourceFactory[A, IO]:
    final override def release(a: A): Unit !! IO = IO(a.close)
