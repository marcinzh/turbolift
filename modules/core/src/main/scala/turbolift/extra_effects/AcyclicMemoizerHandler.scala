package turbolift.extra_effects
import turbolift.abstraction.!!
import turbolift.std_effects.State


object AcyclicMemoizerHandler:
  def apply[K, V, Fx <: AcyclicMemoizer[K, V]](fx: Fx): fx.ThisIIdHandler =
    case object Storage extends State[Map[K, V]]

    new fx.Proxy[Storage.type] with AcyclicMemoizerSig[K, V]:
      override def get: Map[K, V] !@! ThisEffect =
        Storage.get

      override def memo[U <: ThisEffect](f: K => V !! U)(k: K): V !@! U =
        Storage.get.flatMap { m =>
          m.get(k) match
            case Some(v) => !!.pure(v)
            case None =>
              for
                v <- !!.defer(f(k))
                _ <- Storage.modify(_.updated(k, v))
              yield v
        }

    .toHandler
    .provideWith(Storage.handler(Map()))
    .dropState
