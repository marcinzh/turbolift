package turbolift.internals.extensions
import turbolift.Handler
import turbolift.typeclass.ExtendTuple
import scala.util.{Try, Success, Failure}


trait HandlerExtensions:
  extension [S, L, N](thiz: Handler[(_, S), L, N])
    def eval: Handler.Id[L, N] = dropState

    def exec: Handler[[X] =>> S, L, N] = justState

    def justState: Handler[[X] =>> S, L, N] = thiz.map([A] => (pair: (A, S)) => pair._2)

    def dropState: Handler.Id[L, N] = thiz.map([A] => (pair: (A, S)) => pair._1)

    def mapState[S2](f: S => S2): Handler[(_, S2), L, N] =
      thiz.map([A] => (pair: (A, S)) =>
        val (a, s) = pair
        (a, f(s))
      )

    def ***![S2, S3, L2, N2](that: Handler[(_, S2), L2, N2])(using ET: ExtendTuple[S, S2, S3]): Handler[(_, S3), L & L2, N & N2] =
      thiz.composeWith(that).map([A] => (pairs: ((A, S), S2)) =>
        val ((a, s), s2) = pairs
        (a, ET.extendTuple(s, s2))
      )


  extension [L, N](thiz: Handler[Option, L, N])
    def toVector: Handler[Vector, L, N] =
      thiz.map([A] => (result: Option[A]) => result.toVector)
    
    def toEither[E](e: => E): Handler[Either[E, _], L, N] =
      thiz.map([A] => (result: Option[A]) => result.toRight(e))

    def toTry(e: => Throwable): Handler[Try, L, N] =
      thiz.map([A] => (result: Option[A]) => result.fold[Try[A]](Failure(e))(Success(_)))

    def getOrElse(default: => Nothing): Handler.Id[L, N] =
      thiz.map([A] => (result: Option[A]) => result.getOrElse(default))

    def getOrDie(message: => String): Handler.Id[L, N] =
      getOrElse(sys.error(message))

    def unsafeGet: Handler.Id[L, N] =
      thiz.map([A] => (result: Option[A]) => result.get)

    @annotation.targetName("flattenOptions")
    def |||![L2, N2](that: Handler[Option, L2, N2]): Handler[Option, L & L2, N & N2] =
      thiz.composeWith(that).map([A] => (options: Option[Option[A]]) => options.flatten)


  extension [E, L, N](thiz: Handler[Either[E, _], L, N])
    def toOption: Handler[Option, L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.toOption)

    def toTry(implicit ev: E <:< Throwable): Handler[Try, L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.toTry)

    def getOrElse(default: E => Nothing): Handler.Id[L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.fold(default, x => x))

    def getOrDie(message: E => String): Handler.Id[L, N] =
      getOrElse(e => sys.error(message(e)))

    def mapLeft[E2](f: E => E2): Handler[Either[E2, _], L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.swap.map(f).swap)

    @annotation.targetName("flattenEithers")
    def |||![E2, L2, N2](that: Handler[Either[E2, _], L2, N2]): Handler[Either[E | E2, _], L & L2, N & N2] =
      thiz.composeWith(that).map([A] => (eithers: Either[E2, Either[E, A]]) => eithers.flatten: Either[E2 | E, A])
