package turbolift.handlers
import turbolift.!!
import turbolift.effects.{AcyclicMemoizerEffect, AcyclicMemoizerSignature, State}
import turbolift.Extensions._


extension [K, V](fx: AcyclicMemoizerEffect[K, V])
  def acyclicMemoizerHandler2[U](f: K => V !! (U & fx.type)): fx.ThisHandler[Identity, Identity, U] =
    case object Storage extends State[Map[K, V]]

    new fx.impl.Proxy[Storage.type & U] with AcyclicMemoizerSignature[K, V]:
      override def domain: Set[K] !! ThisEffect = Storage.gets(_.keySet)

      override def toMap: Map[K, V] !! ThisEffect = Storage.get

      override def memo(k: K): V !! ThisEffect =
        Storage.get.flatMap: m =>
          m.get(k) match
            case Some(v) => !!.pure(v)
            case None =>
              for
                v <- f(k)
                _ <- Storage.modify(_.updated(k, v))
              yield v

    .toHandler
    .partiallyProvideWith[U](Storage.handler(Map()))
    .dropState
