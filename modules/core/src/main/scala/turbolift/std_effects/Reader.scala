package turbolift.std_effects
import cats.Id
// import cats.implicits._
// import cats.instances.functor._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait ReaderSig[P[_], R] extends Signature[P] {
  def ask: P[R]
  def local[A](mod: R => R)(scope: P[A]): P[A]
}


trait Reader[R] extends Effect[ReaderSig[?[_], R]] {
  val ask: R !! this.type = encodeFO(_.ask)
  def asks[A](f: R => A): A !! this.type = ask.map(f)
  def local[A, U](mod: R => R)(scope: A !! U): A !! U with this.type = encodeHO[U](run => _.local(mod)(run(scope)))

  val handler = DefaultReaderHandler[R, this.type](this)
}


object DefaultReaderHandler {
  def apply[R, Fx <: Reader[R]](fx: Fx) = new fx.Unary[R, Id] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      // def pure[A](a: A): R => M[A] = _ => M.pure(a)

      def purer[A](a: A): R => A = _ => a

      def lift[A](ma: M[A]): R => M[A] = _ => ma

      def flatMap[A, B](tma: R => M[A])(f: A => R => M[B]): R => M[B] =
        r => tma(r).flatMap(a => f(a)(r))

      def zipPar[A, B](tma: R => M[A], tmb: R => M[B]): R => M[(A, B)] =
        r => tma(r) *! tmb(r)
    }

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with ReaderSig[P, R] {
      val ask: P[R] = liftOuter(r => pureInner(r))

      def local[A](mod: R => R)(scope: P[A]): P[A] =
        withUnlift { run =>
          r => run(scope)(mod(r))
        }
    }
  }.self
}
