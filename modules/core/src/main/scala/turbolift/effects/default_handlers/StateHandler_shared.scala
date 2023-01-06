package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.io.{IO, Ref}
import turbolift.effects.{State, StateSig}


extension [S](fx: State[S])
  private[effects] def stateHandler_shared(initial: S): fx.ThisHandler[(_, S), IO] =
    Ref(initial) >>=! { ref =>
      new fx.Proxy[IO] with StateSig[S]:
        override val get: S !! IO = ref.get

        override def gets[A](f: S => A): A !! IO = ref.gets(f)

        override def put(s: S): Unit !! IO = ref.put(s)

        override def swap(s: S): S !! IO = ref.swap(s)

        override def modify(f: S => S): Unit !! IO = ref.modify(f)

        override def modifyGet(f: S => S): S !! IO = ref.modifyGet(f)

        override def getModify(f: S => S): S !! IO = ref.getModify(f)

        override def getModifyGet(f: S => S): (S, S) !! IO = ref.getModifyGet(f)

        override def update[A](f: S => (A, S)): A !! IO = ref.update(f)

        override def updateGet[A](f: S => (A, S)): (A, S) !! IO = ref.updateGet(f)

        override def getUpdate[A](f: S => (A, S)): (A, S) !! IO = ref.getUpdate(f)

        override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! IO = ref.getUpdateGet(f)

      .toHandler
      .flatMap([A] => (a: A) => ref.get.map((a, _)))
    }
