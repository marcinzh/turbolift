package turbolift.abstraction.typeclass
// import simulacrum.typeclass
// import simulacrum.{typeclass, op}
// import cats.Functor


trait ZipPar[F[_]] {
  def zipPar[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object ZipPar {
  def apply[F[_]](implicit ev: ZipPar[F]) = ev
}

trait ZipParImplicits {
  implicit class ZipParSyntax[F[_], A](thiz: F[A])(implicit F: ZipPar[F]) {
    def *![B](that: F[B]): F[(A, B)] = F.zipPar(thiz, that)
    // def *<![B](that: F[B]): F[A] = F.zipPar1st(thiz, that)
    // def *>![B](that: F[B]): F[B] = F.zipPar2nd(thiz, that)
  }
}
