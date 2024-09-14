package turbolift.handlers
import turbolift.!!
import turbolift.effects.{StateEffect, StateSignature, IO}
import turbolift.io.AtomicVar
import turbolift.Extensions._


extension [S](fx: StateEffect[S])
  def stateHandler_shared(initial: S): fx.ThisHandler[Identity, (_, S), IO] =
    AtomicVar(initial) >>=! { avar =>
      new fx.impl.Proxy[IO] with StateSignature[S]:
        override def get: S !! ThisEffect = avar.get

        override def gets[A](f: S => A): A !! ThisEffect = avar.gets(f)

        override def put(s: S): Unit !! ThisEffect = avar.put(s)

        override def swap(s: S): S !! ThisEffect = avar.swap(s)

        override def modify(f: S => S): Unit !! ThisEffect = avar.modify(f)

        override def modifyGet(f: S => S): S !! ThisEffect = avar.modifyGet(f)

        override def getModify(f: S => S): S !! ThisEffect = avar.getModify(f)

        override def getModifyGet(f: S => S): (S, S) !! ThisEffect = avar.getModifyGet(f)

        override def update[A](f: S => (A, S)): A !! ThisEffect = avar.update(f)

        override def updateGet[A](f: S => (A, S)): (A, S) !! ThisEffect = avar.updateGet(f)

        override def getUpdate[A](f: S => (A, S)): (A, S) !! ThisEffect = avar.getUpdate(f)

        override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! ThisEffect = avar.getUpdateGet(f)

      .toHandler
      .mapEffK([A] => (a: A) => avar.get.map((a, _)))
    }
