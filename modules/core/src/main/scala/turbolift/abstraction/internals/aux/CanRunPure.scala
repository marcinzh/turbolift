package turbolift.abstraction.internals.aux
import scala.annotation.implicitNotFound
import turbolift.abstraction.!!


@implicitNotFound(msg =
  "Can't run the computation, because it has some unhandled effects:"+
  "\n    ${U}"
)
//// asserts U is empty set
sealed trait CanRunPure[U] {
  def apply[A](comp: A !! U): A !! Any
}

object CanRunPure {
  private[abstraction] val singleton = new CanRunPure[Any] {
    def apply[A](comp: A !! Any): A !! Any = comp
  }

  implicit def CanRunPure_evidence[U](implicit ev: U =:= Any): CanRunPure[U] =
    CanRunPure.singleton.asInstanceOf[CanRunPure[U]]
}
