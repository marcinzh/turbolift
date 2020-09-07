package turbolift.std_handlers
import cats.Id
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{MemoizerSig, Memoizer, State}


object DependentMemoizerHandler {
  def apply[K, V, Fx <: Memoizer[K, V]](fx: Fx): fx.ThisIHandler[Id] = {
    case object Storage extends State[Map[K, V]]

    new fx.Dependent[Storage.type] {
      override def interpret[U <: Storage.type] = new MemoizerSig[U, K, V] {
        override val snapshot: Map[K, V] !! U = Storage.get

        override def memo(fun: K => V !! U)(k: K): V !! U =
          Storage.get.flatMap { m1 =>
            m1.get(k) match {
              case Some(v) => !!.pure(v)
              case None => for {
                v <- !!.defer(fun(k))
                m2 = m1.updated(k, v)
                _ <- Storage.put(m2)
              } yield v
            }
          }
      }
    }
    .toHandler
    .provideWith(Storage.handler(Map.empty[K, V]))
    .dropState
  }
}
