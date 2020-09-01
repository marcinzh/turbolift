package turbolift.std_handlers
import cats.Id
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{ReaderSig, Reader}


object DefaultReaderHandler {
  def apply[R, Fx <: Reader[R]](fx: Fx) = new fx.Unary[R, Id] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](r: R, a: A): A = a

      def flatMap[A, B](tma: R => M[A])(f: A => R => M[B]): R => M[B] =
        r => tma(r).flatMap(a => f(a)(r))

      def zipPar[A, B](tma: R => M[A], tmb: R => M[B]): R => M[(A, B)] =
        r => tma(r) *! tmb(r)
    }

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with ReaderSig[U, R] {
      val ask: R !! U =
        withLift { l => r =>
          pureInner(l.pureStash(r))
        }

      def local[A](mod: R => R)(scope: A !! U): A !! U =
        withLift { l => r =>
          l.run(scope)(mod(r))
        }
    }
  }.self
}
