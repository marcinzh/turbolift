package turbolift.abstraction.internals.aux
import scala.annotation.implicitNotFound
import turbolift.abstraction.!!


@implicitNotFound(msg =
  "Can't safely apply the partial handler, because some of computation's effects would leak. "+
  "It is required that caller manually computes set difference between computation's effects and handler's effects. "+
  "The resulting set is then passed as explicit type parameter to `handle[_]` or `handleWith[_]` methods. "+
  "Compiler can only detect, if the resulting set is correct."+
  "\nIn this particular case, the set of effects requested by the computation is:"+
  "\n    ${V}"+
  "\nAnd the set of effects to remain after application of the handler is:"+
  "\n    ${U}"
)
//// asserts U = V \ W
sealed trait CanHandle[U, V, W] {
  def apply[A](comp: A !! V): A !! W with U
}

object CanHandle {
  private[abstraction] val singleton = new CanHandle[Any, Any, Any] {
    def apply[A](comp: A !! Any): A !! Any = comp
  }

  implicit def CanHandle_evidence[U, V, W](implicit ev: (W with U) <:< V): CanHandle[U, V, W] =
    CanHandle.singleton.asInstanceOf[CanHandle[U, V, W]]
}
