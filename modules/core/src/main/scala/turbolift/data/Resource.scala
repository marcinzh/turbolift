package turbolift.data
import java.io.{Closeable => JCloseable}
import turbolift.!!
import turbolift.effects.{IO, Finalizer}


/** To be used with [[turbolift.effects.FinalizerEffect FinalizerEffect]]. */
final case class Resource[A, U](acquire: A !! U, release: A => Unit !! U):
  def use: A !! Finalizer[U] = Finalizer.use(this)


object Resource:
  def apply[U](acquire: Unit !! U, release: Unit !! U): Resource[Unit, U] = apply(acquire, _ => release)

  def apply[A <: JCloseable, U <: IO](acquire: A !! U): Resource[A, U] = apply(acquire, a => IO(a.close))
