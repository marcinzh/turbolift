package turbolift.handlers
import turbolift.!!
import turbolift.effects.{StateEffect, StateSignature, IO}
import turbolift.io.AtomicVar
import turbolift.Extensions._


extension [S](fx: StateEffect[S])
  def stateHandler_shared(initial: S): fx.ThisHandler[Identity, (_, S), IO] =
    AtomicVar(initial).flatMapHandler: avar =>
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

        override def getsEff[A, U <: ThisEffect](f: S => A !! U): A !! U = avar.getsEff(f)

        override def putEff[U <: ThisEffect](s: S !! U): Unit !! U = avar.putEff(s)

        override def swapEff[U <: ThisEffect](s: S !! U): S !! U = avar.swapEff(s)

        override def modifyEff[U <: ThisEffect](f: S => S !! U): Unit !! U = avar.modifyEff(f)

        override def modifyGetEff[U <: ThisEffect](f: S => S !! U): S !! U = avar.modifyGetEff(f)

        override def getModifyEff[U <: ThisEffect](f: S => S !! U): S !! U = avar.getModifyEff(f)

        override def getModifyGetEff[U <: ThisEffect](f: S => S !! U): (S, S) !! U = avar.getModifyGetEff(f)

        override def updateEff[A, U <: ThisEffect](f: S => (A, S) !! U): A !! U = avar.updateEff(f)

        override def updateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = avar.updateGetEff(f)

        override def getUpdateEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = avar.getUpdateEff(f)

        override def getUpdateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S, S) !! U = avar.getUpdateGetEff(f)

        override def localPut[A, U <: ThisEffect](s: S)(body: A !! U): A !! U =
          for
            s0 <- avar.swap(s)
            a <- body
            _ <- avar.put(s0)
          yield a

        override def localPutEff[A, U <: ThisEffect](s: S !! U)(body: A !! U): A !! U =
          for
            s0 <- avar.swapEff(s)
            a <- body
            _ <- avar.put(s0)
          yield a

        override def localModify[A, U <: ThisEffect](f: S => S)(body: A !! U): A !! U =
          for
            s0 <- avar.getModify(f)
            a <- body
            _ <- avar.put(s0)
          yield a

        override def localModifyEff[A, U <: ThisEffect](f: S => S !! U)(body: A !! U): A !! U =
          for
            s0 <- avar.getModifyEff(f)
            a <- body
            _ <- avar.put(s0)
          yield a

      .toHandler
      .mapEffK([A] => (a: A) => avar.get.map((a, _)))
