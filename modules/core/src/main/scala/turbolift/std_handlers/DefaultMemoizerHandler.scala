package turbolift.std_handlers
import cats.Id
import turbolift.abstraction.!!
import turbolift.std_effects.{MemoizerSig, Memoizer, State}


object DefaultMemoizerHandler {
  def apply[K, V, Fx <: Memoizer[K, V]](fx: Fx): fx.ThisIHandler[Id] = {
    case object Storage extends State[Map[K, V]]

    new fx.Dependent[Storage.type] {
      override def interpret[U <: Storage.type] = new MemoizerSig[U, K, V] {
        override val snapshot: Map[K, V] !! U = Storage.get

        override def memo(fun: K => V !! U)(k: K): V !! U =
          Storage.get.flatMap { m =>
            m.get(k) match {
              case Some(v) => !!.pure(v)
              case None => for {
                v <- !!.defer(fun(k))
                _ <- Storage.modify(_.updated(k, v))
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
