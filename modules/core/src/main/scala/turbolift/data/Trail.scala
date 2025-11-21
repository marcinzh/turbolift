package turbolift.data
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.{Cause, Snap}


/** Internal state of [[turbolift.effects.Finalizer]] effect's default handler. */
opaque type Trail[-U] = Snap[Unit] !! U

object Trail:
  val empty: Trail[Any] = !!.pure(Snap.unit)
  def apply[U](comp: Unit !! U): Trail[U] = IO.snap(comp)

  extension [U](thiz: Trail[U])
    def run: Unit !! U = thiz.flatMap(IO.unsnap)
    def ||(that: Trail[U]): Trail[U] = thiz.zipWithPar(that)(lift(_ || _))
    def &&(that: Trail[U]): Trail[U] = thiz.zipWith(that)(lift(_ && _))

  private def lift(op: (Cause, Cause) => Cause)(a: Snap[Unit], b: Snap[Unit]): Snap[Unit] =
    (a, b) match
      case (Snap.Failure(c1), Snap.Failure(c2)) => Snap.Failure(op(c1, c2))
      case (Snap.Failure(_), _) => a
      case (_, Snap.Failure(_)) => b
      case _ => Snap.unit
