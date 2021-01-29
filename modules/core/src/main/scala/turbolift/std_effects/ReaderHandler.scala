package turbolift.std_effects
import cats.Id
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ReaderHandler {
  def apply[R, Fx <: Reader[R]](fx: Fx, initial: R): fx.ThisIHandler[Id] =
    new fx.Unary[R, Id] {
      override def purer[A](r: R, a: A): A = a

      override def transform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: R => M[A])(f: A => R => M[B]): R => M[B] =
          r => tma(r).flatMap(a => f(a)(r))

        override def zipPar[A, B](tma: R => M[A], tmb: R => M[B]): R => M[(A, B)] =
          r => tma(r) *! tmb(r)
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new ReaderSig[U, R] {
        override val ask: R !! U =
          ctx.withLift(lift => r => ctx.pureInner(lift.pureStash(r)))

        override def asks[A](f: R => A): A !! U =
          ctx.withLift(lift => r => ctx.pureInner(lift.pureStash(f(r))))

        override def local[A](r: R)(scope: A !! U): A !! U =
          ctx.withLift(lift => _ => lift.run(scope)(r))

        override def localModify[A](mod: R => R)(scope: A !! U): A !! U =
          ctx.withLift(lift => r => lift.run(scope)(mod(r)))
      }
    }.toHandler(initial)
}
