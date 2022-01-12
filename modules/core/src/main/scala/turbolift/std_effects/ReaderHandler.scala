package turbolift.std_effects
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ReaderHandler:
  def apply[R, Fx <: Reader[R]](fx: Fx, initial: R): fx.ThisIIdHandler =
    new fx.Stateful[R, [X] =>> X] with ReaderSig[R]:
      override def onReturn[A](a: A): R => A = _ => a

      override def onFlatMap[A, B, M[_]: MonadPar](tma: R => M[A])(f: A => R => M[B]): R => M[B] =
        r => tma(r).flatMap(a => f(a)(r))

      override def onProduct[A, B, M[_]: MonadPar](tma: R => M[A], tmb: R => M[B]): R => M[(A, B)] =
        r => tma(r) *! tmb(r)
      
      override val ask: R !@! ThisEffect =
        kk ?=> r => kk.outer(kk.inner(r))

      override def asks[A](f: R => A): A !@! ThisEffect =
        kk ?=> r => kk.outer(kk.inner(f(r)))

      override def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !@! U =
        kk ?=> _ => kk.locally(body)(r)

      override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !@! U =
        kk ?=> r => kk.locally(body)(f(r))
      
    .toHandler(initial)
