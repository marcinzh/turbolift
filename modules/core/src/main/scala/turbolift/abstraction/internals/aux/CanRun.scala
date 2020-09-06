package turbolift.abstraction.internals.aux
import scala.annotation.implicitNotFound
import turbolift.abstraction.!!


@implicitNotFound(msg =
  "Effect leak (implicit not found: CanRun)"+
  "\n  Effects remaining unhandled:"+
  "\n    ${U}"
)
//// asserts U is empty set
sealed trait CanRun[U] {
  def apply[A](comp: A !! U): A !! Any
}

object CanRun {
  private[abstraction] val singleton = new CanRun[Any] {
    override def apply[A](comp: A !! Any): A !! Any = comp
  }

  implicit def CanRun_evidence[U](implicit ev: U =:= Any): CanRun[U] =
    CanRun.singleton.asInstanceOf[CanRun[U]]
}
