package turbolift.std_effects
import turbolift.{!!, Effect, Signature}
import turbolift.std_effects.default_handlers.StateHandler


trait StateSig[S] extends Signature:
  def get: S !@! ThisEffect
  def gets[A](f: S => A): A !@! ThisEffect
  def put(s: S): Unit !@! ThisEffect
  def swap(s: S): S !@! ThisEffect
  def modify(f: S => S): Unit !@! ThisEffect
  def modifyGet(f: S => S): S !@! ThisEffect
  def getModify(f: S => S): S !@! ThisEffect
  def getModifyGet(f: S => S): (S, S) !@! ThisEffect
  def update[A](f: S => (S, A)): A !@! ThisEffect
  def updateGet[A](f: S => (S, A)): (S, A) !@! ThisEffect
  def getUpdate[A](f: S => (S, A)): (S, A) !@! ThisEffect
  def getUpdateGet[A](f: S => (S, A)): (S, S, A) !@! ThisEffect


trait State[S] extends Effect[StateSig[S]] with StateSig[S]:
  final override val get: S !! this.type = operate(_.get)
  final override def gets[A](f: S => A): A !! this.type = operate(_.gets(f))
  final override def put(s: S): Unit !! this.type = operate(_.put(s))
  final override def swap(s: S): S !! this.type = operate(_.swap(s))
  final override def modify(f: S => S): Unit !! this.type = operate(_.modify(f))
  final override def modifyGet(f: S => S): S !! this.type = operate(_.modifyGet(f))
  final override def getModify(f: S => S): S !! this.type = operate(_.getModify(f))
  final override def getModifyGet(f: S => S): (S, S) !! this.type = operate(_.getModifyGet(f))
  final override def update[A](f: S => (S, A)): A !! this.type = operate(_.update(f))
  final override def updateGet[A](f: S => (S, A)): (S, A) !! this.type = operate(_.updateGet(f))
  final override def getUpdate[A](f: S => (S, A)): (S, A) !! this.type = operate(_.getUpdate(f))
  final override def getUpdateGet[A](f: S => (S, A)): (S, S, A) !! this.type = operate(_.getUpdateGet(f))

  def handler(initial: S): ThisHandler.Free[(S, _)] = StateHandler(this, initial)
