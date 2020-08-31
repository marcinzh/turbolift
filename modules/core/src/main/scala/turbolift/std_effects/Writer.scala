package turbolift.std_effects
import cats.Monoid
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.{Accum}
import turbolift.std_handlers.DefaultWriterHandler


trait WriterSig[U, W] extends Signature[U] {
  def tell(w: W): Unit !! U
  def listen[A](scope: A !! U): (W, A) !! U
  def censor[A](scope: A !! U)(mod: W => W): A !! U
  def clear[A](scope: A !! U): A !! U
}


trait Writer[W] extends Effect[WriterSig[?, W]] {
  def tell(w: W): Unit !! this.type = encodeFO(_.tell(w))
  def tell[X](x: X)(implicit ev: Accum[X, W]): Unit !! this.type = tell(ev.one(x))
  def listen[A, U](scope: A !! U): (W, A) !! U with this.type = encodeHO[U](_.listen(scope))
  def censor[A, U](scope: A !! U)(f: W => W): A !! U with this.type = encodeHO[U](_.censor(scope)(f))
  def clear[A, U](scope: A !! U): A !! U with this.type = encodeHO[U](_.clear(scope))

  def handler(implicit W: Monoid[W]) = DefaultWriterHandler[W, this.type](this).apply(W.empty)
}
