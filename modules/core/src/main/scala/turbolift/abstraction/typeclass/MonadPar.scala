package turbolift.abstraction.typeclass
// import simulacrum.{typeclass, op}
import cats.{Monad, Defer, Id, StackSafeMonad}


trait MonadPar[F[_]] extends Monad[F] with ZipPar[F] with Defer[F] {
}

object MonadPar {
  def apply[F[_]](implicit ev: MonadPar[F]) = ev

  val identity: MonadPar[Id] = new MonadPar[Id] {
    def pure[A](a: A): A = a
    def flatMap[A, B](a: A)(f: A => B): B = f(a)
    def zipPar[A, B](a: A, b: B): (A, B) = (a, b)
    def defer[A](a: => A): A = a
    def tailRecM[A, B](a: A)(f: A => Either[A, B]): B = Monad[Id].tailRecM(a)(f)
  }

  trait StackSafe[F[_]] extends MonadPar[F] with StackSafeMonad[F]
}
