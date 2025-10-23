package turbolift.data
import java.io.{Closeable => JCloseable}
import turbolift.!!
import turbolift.effects.{IO, Finalizer}
import turbolift.io.AtomicVar


/** To be used with [[turbolift.effects.FinalizerEffect FinalizerEffect]]. */
final case class Resource[A, -U](acquire: A !! U, release: A => Unit !! U):
  def use: A !! Finalizer[U] = Finalizer.use(this)

  def use[B, V <: U](body: A => B !! V): B !! V = IO.bracket(acquire, release)(body)

  def useLazily[B, V <: U & IO, W <: U & IO](body: (A !! V) => B !! W): B !! W =
    AtomicVar.createLockful[Option[A]](None).flatMap: avar =>
      val onAcquire =
        avar.getsEff:
          case Some(a) => !!.pure(a)
          case None =>
            IO.uncancellable:
              avar.updateEff:
                case aa @ Some(a) => !!.pure((a, aa))
                case None => acquire.map(a => (a, Some(a)))

      val onRelease = avar.getsEff:
        case Some(a) => release(a)
        case None => !!.unit

      body(onAcquire).guarantee(onRelease)


object Resource:
  def apply[U](acquire: Unit !! U, release: Unit !! U): Resource[Unit, U] = apply(acquire, _ => release)

  def apply[A <: JCloseable, U <: IO](acquire: A !! U): Resource[A, U] = apply(acquire, a => IO(a.close))
