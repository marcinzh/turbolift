package turbolift.handlers
import turbolift.!!
import turbolift.effects.{AcyclicMemoizerEffect, AcyclicMemoizerSignature}
import turbolift.Extensions._


extension [K, V](fx: AcyclicMemoizerEffect[K, V])
  def acyclicMemoizer_local[U](f: K => V !! (U & fx.type)): fx.ThisHandler[Identity, Identity, U] =
    new fx.impl.Stateful[Identity, Identity, U] with fx.impl.Sequential with AcyclicMemoizerSignature[K, V]:
      override type Local = Map[K, V]

      override def onInitial = Map().pure_!!

      override def onReturn(x: Unknown, s: Local) = x.pure_!!

      override def domain: Set[K] !! ThisEffect = Local.gets(_.keySet)

      override def toMap: Map[K, V] !! ThisEffect = Local.get

      override def memo(k: K): V !! ThisEffect =
        Local.get.flatMap: m =>
          m.get(k) match
            case Some(v) => !!.pure(v)
            case None =>
              for
                v <- Control.reinterpret(f(k))
                _ <- Local.modify(_.updated(k, v))
              yield v

    .toHandler
