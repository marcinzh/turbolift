package turbolift.std_handlers
import cats.Id
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{MemoizerSig, Memoizer}


//@#@TODO reuse State effect somehow
object DefaultMemoizerHandler {
  def apply[K, V, Fx <: Memoizer[K, V]](fx: Fx): fx.ThisHandler[Id] =
    new fx.Unary[Map[K, V], (Map[K, V], ?)] {
      private type S = Map[K, V]
      override def purer[A](s: S, a: A): (S, A) = (s, a)

      override def transform[M[_]: MonadPar] = new Transformed[M] {
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

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new MemoizerSig[U, K, V] {
        val snapshot: S !! U =
          ctx.withLift { lift => m =>
            ctx.pureInner((m, lift.pureStash(m)))
          }

        def memo(fun: K => V !! U)(k: K): V !! U =
          ctx.withLift { lift => m0 =>
            m0.get(k) match {
              case Some(v) => ctx.pureInner((m0, lift.pureStash(v)))
              case None =>
                lift.run(!!.defer(fun(k)).flatMap { v =>
                  ctx.withLift { lift => m =>
                    val m2 = m.updated(k, v)
                    ctx.pureInner((m2, lift.pureStash(v)))
                  }
                })(m0)
            }
          }
      }
    }.toHandler(Map.empty[K, V]).dropState
}
