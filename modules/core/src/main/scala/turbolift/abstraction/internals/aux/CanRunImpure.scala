package turbolift.abstraction.internals.aux
import scala.annotation.implicitNotFound
import turbolift.abstraction.!!


@implicitNotFound(msg =
  "Can't run the computation, because given handler doesn't handle all requested effects. To handle only subset of the effects, try using `handleWith` method"+
  "\nIn this particular case, the set of effects requested by the computation is:"+
  "\n    ${U}"+
  "\nAnd the set of effects to handled by the handler is:"+
  "\n    ${V}"
)
//// asserts U <= V
sealed trait CanRunImpure[U, V] {
  def apply[A](eff: A !! U): A !! V
}

object CanRunImpure {
  private[abstraction] val singleton = new CanRunImpure[Any, Any] {
    def apply[A](eff: A !! Any): A !! Any = eff
  }
}

trait CanRunImpureImplicits {
  implicit def CanRunImpure_evidence[U, V](implicit ev: V <:< U): CanRunImpure[U, V] =
    CanRunImpure.singleton.asInstanceOf[CanRunImpure[U, V]]
}
