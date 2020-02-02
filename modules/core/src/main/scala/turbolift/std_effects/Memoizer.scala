package turbolift.std_effects
import cats.Id
import cats.implicits._
import turbolift.abstraction.{!!, Handler}
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits._


trait MemoizerSig[P[_], K, V] extends Signature[P] {
  def memo(fun: K => P[V])(k: K): P[V]
  def snapshot: P[Map[K, V]]
}


trait Memoizer[K, V] extends Effect[MemoizerSig[?[_], K, V]] {
  def memo[U](fun: K => V !! U)(k: K): V !! U with this.type = encodeHO[U](run => _.memo(k1 => run(fun(k1)))(k))
  def snapshot: Map[K, V] !! this.type = encodeFO(_.snapshot)

  val handler = DefaultMemoizerHandler[K, V, this.type](this)
}


//@#@TODO reuse State effect somehow
object DefaultMemoizerHandler {
  def apply[K, V, Fx <: Memoizer[K, V]](fx: Fx): Handler.Apply[Id, fx.type] = new fx.Unary[Map[K, V], (Map[K, V], ?)] {
    private type S = Map[K, V]
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def pure[A](a: A): S => M[(S, A)] = s => M.pure((s, a))

      def lift[A](ma: M[A]): S => M[(S, A)] = s => ma.map((s, _))

      def flatMap[A, B](tma: S => M[(S, A)])(f: A => S => M[(S, B)]): S => M[(S, B)] =
        s0 => tma(s0).flatMap {
          case (s1, a) => f(a)(s1)
        }

      def zipPar[A, B](tma: S => M[(S, A)], tmb: S => M[(S, B)]): S => M[(S, (A, B))] =
        s0 => tma(s0).flatMap {
          case (s1, a) => tmb(s1).map {
            case (s2, b) => (s2, (a, b))
          }
        }
    }

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with MemoizerSig[P, K, V] {
      val snapshot: P[S] = liftOuter(m => pureInner((m, m)))

      def memo(fun: K => P[V])(k: K): P[V] =
        withUnlift { run => m0 =>
          m0.get(k) match {
            case Some(v) => run(pureOuter(v))(m0)
            case None =>
              run(outerMonad.defer(fun(k)).flatMap { v =>
                liftOuter(m => pureInner((m.updated(k, v), v)))
              })(m0)
          }
        }
    }
  }.apply(Map.empty[K, V]).dropState
}
