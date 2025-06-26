package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.Extensions._
import turbolift.handlers.{stateHandler_local, stateHandler_shared}


trait StateSignature[S] extends Signature:
  def get: S !! ThisEffect
  def gets[A](f: S => A): A !! ThisEffect
  def put(s: S): Unit !! ThisEffect
  def swap(s: S): S !! ThisEffect
  def modify(f: S => S): Unit !! ThisEffect
  def modifyGet(f: S => S): S !! ThisEffect
  def getModify(f: S => S): S !! ThisEffect
  def getModifyGet(f: S => S): (S, S) !! ThisEffect
  def update[A](f: S => (A, S)): A !! ThisEffect
  def updateGet[A](f: S => (A, S)): (A, S) !! ThisEffect
  def getUpdate[A](f: S => (A, S)): (A, S) !! ThisEffect
  def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! ThisEffect

  def getsEff[A, U <: ThisEffect](f: S => A !! U): A !! U
  def putEff[U <: ThisEffect](s: S !! U): Unit !! U
  def swapEff[U <: ThisEffect](s: S !! U): S !! U
  def modifyEff[U <: ThisEffect](f: S => S !! U): Unit !! U
  def modifyGetEff[U <: ThisEffect](f: S => S !! U): S !! U
  def getModifyEff[U <: ThisEffect](f: S => S !! U): S !! U
  def getModifyGetEff[U <: ThisEffect](f: S => S !! U): (S, S) !! U
  def updateEff[A, U <: ThisEffect](f: S => (A, S) !! U): A !! U
  def updateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U
  def getUpdateEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U
  def getUpdateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S, S) !! U

  def localPut[A, U <: ThisEffect](s: S)(body: A !! U): A !! U
  def localPutEff[A, U <: ThisEffect](s: S !! U)(body: A !! U): A !! U
  def localModify[A, U <: ThisEffect](f: S => S)(body: A !! U): A !! U
  def localModifyEff[A, U <: ThisEffect](f: S => S !! U)(body: A !! U): A !! U


trait StateEffect[S] extends Effect[StateSignature[S]] with StateSignature[S]:
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

  final override def getsEff[A, U <: this.type](f: S => A !! U): A !! U = perform(_.getsEff(f))
  final override def putEff[U <: this.type](s: S !! U): Unit !! U = perform(_.putEff(s))
  final override def swapEff[U <: this.type](s: S !! U): S !! U = perform(_.swapEff(s))
  final override def modifyEff[U <: this.type](f: S => S !! U): Unit !! U = perform(_.modifyEff(f))
  final override def modifyGetEff[U <: this.type](f: S => S !! U): S !! U = perform(_.modifyGetEff(f))
  final override def getModifyEff[U <: this.type](f: S => S !! U): S !! U = perform(_.getModifyEff(f))
  final override def getModifyGetEff[U <: this.type](f: S => S !! U): (S, S) !! U = perform(_.getModifyGetEff(f))
  final override def updateEff[A, U <: this.type](f: S => (A, S) !! U): A !! U = perform(_.updateEff(f))
  final override def updateGetEff[A, U <: this.type](f: S => (A, S) !! U): (A, S) !! U = perform(_.updateGetEff(f))
  final override def getUpdateEff[A, U <: this.type](f: S => (A, S) !! U): (A, S) !! U = perform(_.getUpdateEff(f))
  final override def getUpdateGetEff[A, U <: this.type](f: S => (A, S) !! U): (A, S, S) !! U = perform(_.getUpdateGetEff(f))

  final override def localPut[A, U <: this.type](s: S)(body: A !! U): A !! U = perform(_.localPut(s)(body))
  final override def localPutEff[A, U <: this.type](s: S !! U)(body: A !! U): A !! U = perform(_.localPutEff(s)(body))
  final override def localModify[A, U <: this.type](f: S => S)(body: A !! U): A !! U = perform(_.localModify(f)(body))
  final override def localModifyEff[A, U <: this.type](f: S => S !! U)(body: A !! U): A !! U = perform(_.localModifyEff(f)(body))

  /** Predefined handlers for this effect. */
  object handlers:
    def local(initial: S): ThisHandler[Identity, (_, S), Any] = StateEffect.this.stateHandler_local(initial)
    def shared(initial: S): ThisHandler[Identity, (_, S), IO] = StateEffect.this.stateHandler_shared(initial)


trait State[S] extends StateEffect[S]:
  export handlers.{local => handler}
