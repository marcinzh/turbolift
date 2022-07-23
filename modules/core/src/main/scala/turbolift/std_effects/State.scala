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
  def update[A](f: S => (A, S)): A !@! ThisEffect
  def updateGet[A](f: S => (A, S)): (A, S) !@! ThisEffect
  def getUpdate[A](f: S => (A, S)): (A, S) !@! ThisEffect
  def getUpdateGet[A](f: S => (A, S)): (A, S, S) !@! ThisEffect


trait State[S] extends Effect[StateSig[S]] with StateSig[S]:
  final override val get: S !! this.type = perform(_.get)
  final override def gets[A](f: S => A): A !! this.type = perform(_.gets(f))
  final override def put(s: S): Unit !! this.type = perform(_.put(s))
  final override def swap(s: S): S !! this.type = perform(_.swap(s))
  final override def modify(f: S => S): Unit !! this.type = perform(_.modify(f))
  final override def modifyGet(f: S => S): S !! this.type = perform(_.modifyGet(f))
  final override def getModify(f: S => S): S !! this.type = perform(_.getModify(f))
  final override def getModifyGet(f: S => S): (S, S) !! this.type = perform(_.getModifyGet(f))
  final override def update[A](f: S => (A, S)): A !! this.type = perform(_.update(f))
  final override def updateGet[A](f: S => (A, S)): (A, S) !! this.type = perform(_.updateGet(f))
  final override def getUpdate[A](f: S => (A, S)): (A, S) !! this.type = perform(_.getUpdate(f))
  final override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! this.type = perform(_.getUpdateGet(f))

  /** Default handler for this effect. */
  def handler(initial: S): ThisHandler.Free[(_, S)] = StateHandler(this, initial)
