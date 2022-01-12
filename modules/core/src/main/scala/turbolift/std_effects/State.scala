package turbolift.std_effects
import turbolift.abstraction.{!!, Effect, Signature}


trait StateSig[S] extends Signature:
  def get: S !@! ThisEffect
  def gets[A](f: S => A): A !@! ThisEffect
  def put(s: S): Unit !@! ThisEffect
  def swap(s: S): S !@! ThisEffect
  def modify(f: S => S): Unit !@! ThisEffect
  def update[A](f: S => (S, A)): A !@! ThisEffect


trait State[S] extends Effect[StateSig[S]] with StateSig[S]:
  final override val get: S !! this.type = impure(_.get)
  final override def gets[A](f: S => A): A !! this.type = impure(_.gets(f))
  final override def put(s: S): Unit !! this.type = impure(_.put(s))
  final override def swap(s: S): S !! this.type = impure(_.swap(s))
  final override def modify(f: S => S): Unit !! this.type = impure(_.modify(f))
  final override def update[A](f: S => (S, A)): A !! this.type = impure(_.update(f))

  def handler(initial: S): ThisIHandler[(S, _)] = StateHandler(this, initial)
