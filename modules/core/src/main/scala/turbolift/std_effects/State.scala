package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.std_handlers.DefaultStateHandler


trait StateSig[U, S] {
  def get: S !! U
  def put(s: S): Unit !! U
  def mod(f: S => S): Unit !! U = get.flatMap(s => put(f(s)))
}

trait State[S] extends Effect[StateSig[?, S]] {
  final val get: S !! this.type = embedFO(_.get)
  final def put(s: S): Unit !! this.type = embedFO(_.put(s))
  final def mod(f: S => S): Unit !! this.type = embedFO(_.mod(f))

  def handler(initial: S): ThisHandler[(S, ?)] = DefaultStateHandler(this, initial)
}
