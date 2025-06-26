package turbolift.handlers
import turbolift.!!
import turbolift.effects.{StateEffect, StateSignature}
import turbolift.Extensions._


extension [S](fx: StateEffect[S])
  def stateHandler_local(initial: S): fx.ThisHandler[Identity, (_, S), Any] =
    new fx.impl.Stateful[Identity, (_, S), Any] with fx.impl.Sequential.Restartable with StateSignature[S]:
      override type Local = S

      override def onInitial: S !! Any = !!.pure(initial)

      override def onReturn(a: Unknown, s: S): (Unknown, S) !! Any = !!.pure((a, s))

      override def onRestart(a_s: (Unknown, S)): Unknown !! fx.type = fx.put(a_s._2) &&! !!.pure(a_s._1)

      override def onUnknown(aa: (Unknown, S)): Option[Unknown] = Some(aa._1)

      override val get: S !! ThisEffect = Local.get

      override def gets[A](f: S => A): A !! ThisEffect = Local.get.map(f)

      override def put(s2: S): Unit !! ThisEffect = Local.put(s2)

      override def swap(s2: S): S !! ThisEffect = Local.swap(s2)

      override def modify(f: S => S): Unit !! ThisEffect = Local.modify(f)

      override def modifyGet(f: S => S): S !! ThisEffect = Local.modifyGet(f)

      override def getModify(f: S => S): S !! ThisEffect = Local.getModify(f)

      override def getModifyGet(f: S => S): (S, S) !! ThisEffect = Local.getModifyGet(f)

      override def update[A](f: S => (A, S)): A !! ThisEffect = Local.update(f)

      override def updateGet[A](f: S => (A, S)): (A, S) !! ThisEffect = Local.updateGet(f)

      override def getUpdate[A](f: S => (A, S)): (A, S) !! ThisEffect = Local.getUpdate(f)

      override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! ThisEffect = Local.getUpdateGet(f)

      override def getsEff[A, U <: ThisEffect](f: S => A !! U): A !! U = Local.getsEff(f)

      override def putEff[U <: ThisEffect](s: S !! U): Unit !! U = Local.putEff(s)

      override def swapEff[U <: ThisEffect](s: S !! U): S !! U = Local.swapEff(s)

      override def modifyEff[U <: ThisEffect](f: S => S !! U): Unit !! U = Local.modifyEff(f)

      override def modifyGetEff[U <: ThisEffect](f: S => S !! U): S !! U = Local.modifyGetEff(f)

      override def getModifyEff[U <: ThisEffect](f: S => S !! U): S !! U = Local.getModifyEff(f)

      override def getModifyGetEff[U <: ThisEffect](f: S => S !! U): (S, S) !! U = Local.getModifyGetEff(f)

      override def updateEff[A, U <: ThisEffect](f: S => (A, S) !! U): A !! U = Local.updateEff(f)

      override def updateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = Local.updateGetEff(f)

      override def getUpdateEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = Local.getUpdateEff(f)

      override def getUpdateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S, S) !! U = Local.getUpdateGetEff(f)

      override def localPut[A, U <: ThisEffect](s: S)(body: A !! U): A !! U = Control.delimitPut(body, s).map(_._1)

      override def localPutEff[A, U <: ThisEffect](s: S !! U)(body: A !! U): A !! U = s.flatMap(Control.delimitPut(body, _)).map(_._1)

      override def localModify[A, U <: ThisEffect](f: S => S)(body: A !! U): A !! U = Control.delimitModify(body, f).map(_._1)

      override def localModifyEff[A, U <: ThisEffect](f: S => S !! U)(body: A !! U): A !! U = Local.getsEff(f).flatMap(Control.delimitPut(body, _)).map(_._1)

    .toHandler
