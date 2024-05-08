package turbolift.handlers
import turbolift.!!
import turbolift.effects.{AcyclicMemoizer, AcyclicMemoizerSignature}
import turbolift.effects.State
import turbolift.Extensions._


extension [K, V](fx: AcyclicMemoizer[K, V])
  def acyclicMemoizerHandler: fx.ThisHandler[Identity, Identity, Any] =
    case object Storage extends State[Map[K, V]]

    new fx.impl.Proxy[Storage.type] with AcyclicMemoizerSignature[K, V]:
      override def domain: Set[K] !@! ThisEffect = Storage.gets(_.keySet)

      override def toMap: Map[K, V] !@! ThisEffect = Storage.get

      override def memo[U <: ThisEffect](f: K => V !! U)(k: K): V !@! U =
        Storage.get.flatMap: m =>
          m.get(k) match
            case Some(v) => !!.pure(v)
            case None =>
              for
                v <- f(k)
                _ <- Storage.modify(_.updated(k, v))
              yield v

    .toHandler
    .provideWith(Storage.handler(Map()))
    .dropState
