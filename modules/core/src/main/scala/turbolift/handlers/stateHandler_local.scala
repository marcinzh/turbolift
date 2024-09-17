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

    .toHandler
