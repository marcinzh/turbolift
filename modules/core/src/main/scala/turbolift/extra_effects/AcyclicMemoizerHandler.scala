package turbolift.extra_effects
import cats.Id
import turbolift.abstraction.!!
import turbolift.std_effects.State


object AcyclicMemoizerHandler {
  def apply[K, V, Fx <: AcyclicMemoizer[K, V]](fx: Fx): fx.ThisIHandler[Id] = {
    case object Storage extends State[Map[K, V]]

    new fx.Proxy[Storage.type] {
      override def onOperation[U <: Storage.type] = new AcyclicMemoizerSig[U, K, V] {
        override def get: Map[K, V] !! U =
          Storage.get

        override def memo(f: K => V !! U)(k: K): V !! U =
          Storage.get.flatMap { m =>
            m.get(k) match {
              case Some(v) => !!.pure(v)
              case None => for {
                v <- !!.defer(f(k))
                _ <- Storage.modify(_.updated(k, v))
              } yield v
            }
          }
      }
    }
    .toHandler
    .provideWith(Storage.handler(Map()))
    .dropState
  }
}
