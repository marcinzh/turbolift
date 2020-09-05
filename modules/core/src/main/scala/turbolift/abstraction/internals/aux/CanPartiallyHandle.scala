package turbolift.abstraction.internals.aux
import scala.annotation.implicitNotFound
import turbolift.abstraction.!!


@implicitNotFound(msg =
  "Effect leak in partial handler (implicit not found: CanPartiallyHandle)"+
  "\n  Effects requested by the computation:"+
  "\n    ${V}"+
  "\n  Effects remaining unhandled:"+
  "\n    ${U}"
)
//// asserts: U ⊇ V \ W
sealed trait CanPartiallyHandle[U, V, W] {
  def apply[A](comp: A !! V): A !! W with U
}

object CanPartiallyHandle {
  private[abstraction] val singleton = new CanPartiallyHandle[Any, Any, Any] {
    def apply[A](comp: A !! Any): A !! Any = comp
  }

  implicit def CanPartiallyHandle_evidence[U, V, W](implicit ev: (W with U) <:< V): CanPartiallyHandle[U, V, W] =
    CanPartiallyHandle.singleton.asInstanceOf[CanPartiallyHandle[U, V, W]]
}
