package turbolift.internals.auxx
import scala.annotation.implicitNotFound
import turbolift.!!
import turbolift.io.IO

//@#@ not used anymore

@implicitNotFound(msg =
  "Effect leak (implicit not found: CanUnsafeRun)"+
  "\n  Effects remaining unhandled:"+
  "\n    ${U}"
)
//// asserts U <= IO
private[turbolift] sealed trait CanUnsafeRun[U]:
  def apply[A](comp: A !! U): A !! Any

private[turbolift] object CanUnsafeRun:
  private[turbolift] val singleton = new CanUnsafeRun[Any]:
    override def apply[A](comp: A !! Any): A !! Any = comp

  implicit def CanUnsafeRun_evidence[U](implicit ev: IO <:< U): CanUnsafeRun[U] =
    CanUnsafeRun.singleton.asInstanceOf[CanUnsafeRun[U]]
