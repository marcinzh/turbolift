package turbolift.std_effects
import cats.Monoid
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.{Accum}
import turbolift.std_handlers.DefaultWriterHandler


trait WriterSig[U, W] {
  def tell(w: W): Unit !! U
  def listen[A](scope: A !! U): (W, A) !! U
  def censor[A](scope: A !! U)(mod: W => W): A !! U
  def clear[A](scope: A !! U): A !! U
}


trait Writer[W] extends Effect[WriterSig[?, W]] {
  final def tell(w: W): Unit !! this.type = embedFO(_.tell(w))
  final def tell[X](x: X)(implicit ev: Accum[X, W]): Unit !! this.type = tell(ev.one(x))
  final def listen[A, U](scope: A !! U): (W, A) !! U with this.type = embedHO[U](_.listen(scope))
  final def censor[A, U](scope: A !! U)(f: W => W): A !! U with this.type = embedHO[U](_.censor(scope)(f))
  final def clear[A, U](scope: A !! U): A !! U with this.type = embedHO[U](_.clear(scope))

  def handler(implicit W: Monoid[W]) = DefaultWriterHandler[W, this.type](this).apply(W.empty)
}
