package turbolift.data
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{Cause, Snap}


/** Internal state of [[turbolift.effects.Finalizer]] effect's predefined handler. */
opaque type Trail[-U <: IO] = Snap[Unit] !! U

object Trail:
  val empty: Trail[IO] = !!.pure(Snap.unit)
  def apply[U <: IO](comp: Unit !! U): Trail[U] = IO.snap(comp)

  extension [U <: IO](thiz: Trail[U])
    def run: Unit !! U = thiz.flatMap(IO.unsnap)
    def &(that: Trail[U]): Trail[U] = thiz.zipWithPar(that)(lift(_ & _))
    def ++(that: Trail[U]): Trail[U] = thiz.zipWith(that)(lift(_ ++ _))

  private def lift(op: (Cause, Cause) => Cause)(a: Snap[Unit], b: Snap[Unit]): Snap[Unit] =
    ((a, b): @unchecked) match
      case (Snap.Failure(c), Snap.Failure(d)) => Snap.Failure(op(c, d))
      case (Snap.Failure(_), _) => a
      case (_, Snap.Failure(_)) => b
      case (Snap.Cancelled, _) => a
      case (_, Snap.Cancelled) => b
      case (Snap.Aborted(_, _), _) => a
      case (_, Snap.Aborted(_, _)) => b
      case _ => Snap.unit
