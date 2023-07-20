package turbolift.internals.auxx
import scala.annotation.implicitNotFound


@implicitNotFound(msg =
  "Invalid chaining of 2 handlers."+
  "\n  Output type of the first handler: ${F}, and"+
  "\n  Input type of the second handler: ${G}, don't match."
)

sealed trait CanPipe[F[+_], G[+_]]:
  def fit[A](fga: F[G[A]]): G[F[A]]

object CanPipe:
  // anything can be followed by identity
  given [F[+_]]: CanPipe[F, [X] =>> X] = new:
    def fit[A](fa: F[A]): F[A] = fa

  // const can be followed by another const
  given [C, D](using ctod: C <:< D): CanPipe[[_] =>> C, [_] =>> D] = new:
    def fit[A](c: C): D = ctod(c)
