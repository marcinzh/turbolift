package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.internals.effect.Signature
import turbolift.std_handlers.DefaultStateHandler


trait StateSig[U, S] extends Signature[U] {
  def get: S !! U
  def put(s: S): Unit !! U
  def mod(f: S => S): Unit !! U = get.flatMap(s => put(f(s)))
}

trait State[S] extends Effect[StateSig[?, S]] {
  final val get: S !! this.type = encodeFO(_.get)
  final def put(s: S): Unit !! this.type = encodeFO(_.put(s))
  final def mod(f: S => S): Unit !! this.type = encodeFO(_.mod(f))

  val handler = DefaultStateHandler[S, this.type](this)
}
