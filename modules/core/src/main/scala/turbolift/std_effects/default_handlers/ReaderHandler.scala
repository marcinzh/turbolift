package turbolift.std_effects.default_handlers
import turbolift.!!
import turbolift.typeclass.MonadZip
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{Reader, ReaderSig}


private[std_effects] object ReaderHandler:
  def apply[R, Fx <: Reader[R]](fx: Fx, initial: R): fx.ThisHandler.FreeId =
    new fx.Stateful[R, [X] =>> X] with ReaderSig[R]:
      override def onPure[A](a: A): R => A = _ => a

      override def onFlatMap[A, B, M[_]: MonadZip](tma: R => M[A])(f: A => R => M[B]): R => M[B] =
        r => tma(r).flatMap(a => f(a)(r))

      override def onZip[A, B, M[_]: MonadZip](tma: R => M[A], tmb: R => M[B]): R => M[(A, B)] =
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
