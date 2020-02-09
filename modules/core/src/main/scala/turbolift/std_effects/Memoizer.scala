package turbolift.std_effects
import cats.Id
// import cats.implicits._
import cats.instances.tuple._
import turbolift.abstraction.{!!, Handler}
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits._


trait MemoizerSig[U, K, V] extends Signature[U] {
  def memo(fun: K => V !! U)(k: K): V !! U
  def snapshot: Map[K, V] !! U
}


trait Memoizer[K, V] extends Effect[MemoizerSig[?, K, V]] {
  def memo[U](fun: K => V !! U)(k: K): V !! U with this.type = encodeHO[U](_.memo(fun)(k))
  def snapshot: Map[K, V] !! this.type = encodeFO(_.snapshot)

  val handler = DefaultMemoizerHandler[K, V, this.type](this)
}


//@#@TODO reuse State effect somehow
object DefaultMemoizerHandler {
  def apply[K, V, Fx <: Memoizer[K, V]](fx: Fx): Handler.Apply[Id, fx.type] = new fx.Unary[Map[K, V], (Map[K, V], ?)] {
    private type S = Map[K, V]
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](s: S, a: A): (S, A) = (s, a)

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

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with MemoizerSig[U, K, V] {
      val snapshot: S !! U =
        withLift { l => m =>
          pureInner((m, l.pureStash(m)))
        }

      def memo(fun: K => V !! U)(k: K): V !! U =
        withLift { l => m0 =>
          m0.get(k) match {
            case Some(v) => pureInner((m0, l.pureStash(v)))
            case None =>
              l.run(!!.defer(fun(k)).flatMap { v =>
                withLift { l => m =>
                  val m2 = m.updated(k, v)
                  pureInner((m2, l.pureStash(v)))
                }
              })(m0)
          }
        }
    }
  }.apply(Map.empty[K, V]).dropState
}
