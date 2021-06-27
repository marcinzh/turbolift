package turbolift.std_effects
import cats.Id
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ReaderHandler:
  def apply[R, Fx <: Reader[R]](fx: Fx, initial: R): fx.ThisIHandler[Id] =
    new fx.Stateful[R, Id]:
      override def onReturn[A](r: R, a: A): A = a

      override def onTransform[M[_]: MonadPar] = new Transformed[M]:
        override def flatMap[A, B](tma: R => M[A])(f: A => R => M[B]): R => M[B] =
          r => tma(r).flatMap(a => f(a)(r))

        override def zipPar[A, B](tma: R => M[A], tmb: R => M[B]): R => M[(A, B)] =
          r => tma(r) *! tmb(r)
      
      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new ReaderSig[U, R]:
        override val ask: R !! U =
          kk.withLift(lift => r => kk.pureInner(lift.pureStash(r)))

        override def asks[A](f: R => A): A !! U =
          kk.withLift(lift => r => kk.pureInner(lift.pureStash(f(r))))

        override def localPut[A](r: R)(body: A !! U): A !! U =
          kk.withLift(lift => _ => lift.run(body)(r))

        override def localModify[A](f: R => R)(body: A !! U): A !! U =
          kk.withLift(lift => r => lift.run(body)(f(r)))
      
    .toHandler(initial)
