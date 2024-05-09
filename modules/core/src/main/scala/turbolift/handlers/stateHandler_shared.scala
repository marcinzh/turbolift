package turbolift.handlers
import turbolift.!!
import turbolift.effects.{StateEffect, StateSignature, IO}
import turbolift.io.Ref
import turbolift.Extensions._


extension [S](fx: StateEffect[S])
  def stateHandler_shared(initial: S): fx.ThisHandler[Identity, (_, S), IO] =
    Ref(initial) >>=! { ref =>
      new fx.impl.Proxy[IO] with StateSignature[S]:
        override def get: S !! ThisEffect = ref.get

        override def gets[A](f: S => A): A !! ThisEffect = ref.gets(f)

        override def put(s: S): Unit !! ThisEffect = ref.put(s)

        override def swap(s: S): S !! ThisEffect = ref.swap(s)

        override def modify(f: S => S): Unit !! ThisEffect = ref.modify(f)

        override def modifyGet(f: S => S): S !! ThisEffect = ref.modifyGet(f)

        override def getModify(f: S => S): S !! ThisEffect = ref.getModify(f)

        override def getModifyGet(f: S => S): (S, S) !! ThisEffect = ref.getModifyGet(f)

        override def update[A](f: S => (A, S)): A !! ThisEffect = ref.update(f)

        override def updateGet[A](f: S => (A, S)): (A, S) !! ThisEffect = ref.updateGet(f)

        override def getUpdate[A](f: S => (A, S)): (A, S) !! ThisEffect = ref.getUpdate(f)

        override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! ThisEffect = ref.getUpdateGet(f)

      .toHandler
      .mapEffK([A] => (a: A) => ref.get.map((a, _)))
    }
