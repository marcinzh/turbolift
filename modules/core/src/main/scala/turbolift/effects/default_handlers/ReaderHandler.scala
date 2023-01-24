package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.effects.{Reader, ReaderSig}


extension [R](fx: Reader[R])
  private[effects] def readerHandler(initial: R): fx.ThisHandler.FreeId =
    new fx.Stateful[R, [X] =>> X] with fx.Parallel.Trivial with ReaderSig[R]:
      override def onPure[A](a: A, r: R): A = a

      override val ask: R !@! ThisEffect = (k, r) => k(r)

      override def asks[A](f: R => A): A !@! ThisEffect = (k, r) => k(f(r))

      override def localPut[A, U <: ThisEffect](r1: R)(body: A !! U): A !@! U = 
        (k, r) => k.local(body, r1).flatMap {
          case (a, k, _) => k(a)
        }

      override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !@! U =
        (k, r) => k.local(body, f(r)).flatMap {
          case (a, k, _) => k(a)
        }
      
    .toHandler(initial)
