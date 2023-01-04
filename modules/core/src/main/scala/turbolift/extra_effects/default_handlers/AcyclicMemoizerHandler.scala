package turbolift.extra_effects.default_handlers
import turbolift.!!
import turbolift.std_effects.State
import turbolift.extra_effects.{AcyclicMemoizer, AcyclicMemoizerSig}


extension [K, V](fx: AcyclicMemoizer[K, V])
  private[extra_effects] def acyclicMemoizerHandler: fx.ThisHandler.FreeId =
    case object Storage extends State[Map[K, V]]

    new fx.Proxy[Storage.type] with AcyclicMemoizerSig[K, V]:
      override def domain: Set[K] !@! ThisEffect = Storage.gets(_.keySet)

      override def toMap: Map[K, V] !@! ThisEffect = Storage.get

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
