package turbolift.internals.auxx
import scala.annotation.implicitNotFound
import turbolift.!!

//@#@ not used anymore

@implicitNotFound(msg =
  "Effect leak in partial handler (implicit not found: CanPartiallyHandle)"+
  "\n  Effects requested by the computation:"+
  "\n    ${V}"+
  "\n  Effects remaining unhandled:"+
  "\n    ${U}"
)
//// asserts: U ⊇ V \ W
private[turbolift] sealed trait CanPartiallyHandle[U, V, W]:
  def apply[A](comp: A !! V): A !! (W & U)

private[turbolift] object CanPartiallyHandle:
  private[turbolift] val singleton = new CanPartiallyHandle[Any, Any, Any]:
    override def apply[A](comp: A !! Any): A !! Any = comp

  implicit def CanPartiallyHandle_evidence[U, V, W](implicit ev: (W & U) <:< V): CanPartiallyHandle[U, V, W] =
    CanPartiallyHandle.singleton.asInstanceOf[CanPartiallyHandle[U, V, W]]
