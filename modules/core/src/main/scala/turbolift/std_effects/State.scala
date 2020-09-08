package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.std_handlers.DefaultStateHandler


trait StateSig[U, S] {
  def get: S !! U
  def gets[A](f: S => A): A !! U
  def put(s: S): Unit !! U
  def swap(s: S): S !! U
  def modify(f: S => S): Unit !! U
  def update[A](f: S => (S, A)): A !! U
}

trait State[S] extends Effect[StateSig[?, S]] {
  final val get: S !! this.type = embedFO(_.get)
  final def gets[A](f: S => A): A !! this.type = embedFO(_.gets(f))
  final def put(s: S): Unit !! this.type = embedFO(_.put(s))
  final def swap(s: S): S !! this.type = embedFO(_.swap(s))
  final def modify(f: S => S): Unit !! this.type = embedFO(_.modify(f))
  final def update[A](f: S => (S, A)): A !! this.type = embedFO(_.update(f))

  def handler(initial: S): ThisIHandler[(S, ?)] = DefaultStateHandler(this, initial)
}
