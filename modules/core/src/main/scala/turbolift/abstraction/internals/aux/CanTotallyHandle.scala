package turbolift.abstraction.internals.aux
import scala.annotation.implicitNotFound
import turbolift.abstraction.!!


@implicitNotFound(msg =
  "Effect leak in total handler (implicit not found: CanTotallyHandle)"+
  "\n  Effects requested by the computation:"+
  "\n    ${U}"+
  "\n  Effects handled by the handler:"+
  "\n    ${V}"
)
//// asserts U <= V
sealed trait CanTotallyHandle[U, V]:
  def apply[A](comp: A !! U): A !! V

object CanTotallyHandle:
  private[abstraction] val singleton = new CanTotallyHandle[Any, Any]:
    override def apply[A](comp: A !! Any): A !! Any = comp

  implicit def CanTotallyHandle_evidence[U, V](implicit ev: V <:< U): CanTotallyHandle[U, V] =
    CanTotallyHandle.singleton.asInstanceOf[CanTotallyHandle[U, V]]
