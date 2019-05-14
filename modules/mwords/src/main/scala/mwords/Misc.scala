package mwords


trait ~>[-F[_], +G[_]] { outer =>
  def apply[A](fa: F[A]): G[A]

  def andThen[H[_]](gh: G ~> H): F ~> H = new (F ~> H) {
    def apply[A](fa: F[A]): H[A] = gh(outer(fa))
  }
}


trait MiscExports {
  type Identity[+A] = A
}
