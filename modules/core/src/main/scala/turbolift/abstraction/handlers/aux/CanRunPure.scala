package turbolift.abstraction.handlers.aux
import scala.annotation.implicitNotFound
import turbolift.abstraction.!!


@implicitNotFound(msg =
  "Can't run the computation, because it has some unhandled effects:"+
  "\n    ${U}"
)
//// asserts U is empty set
sealed trait CanRunPure[U] {
  def apply[A](eff: A !! U): A !! Any
}

object CanRunPure {
  private[abstraction] val singleton = new CanRunPure[Any] {
    def apply[A](eff: A !! Any): A !! Any = eff
  }
}

trait CanRunPureExports {
  implicit def CanRunPure_evidence[U](implicit ev: U =:= Any): CanRunPure[U] =
    CanRunPure.singleton.asInstanceOf[CanRunPure[U]]
}
