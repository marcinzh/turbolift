package turbolift.handlers
import turbolift.!!
import turbolift.effects.{CyclicMemoizerEffect, CyclicMemoizerSignature, State}
import turbolift.Extensions._


extension [K, V](fx: CyclicMemoizerEffect[K, V])
  def cyclicMemoizerHandler[U](f: K => V !! (U & fx.type)): fx.ThisHandler[Identity, Identity, U] =
    case object Storage extends State[Map[K, Thunk[V]]]

    new fx.impl.Proxy[Storage.type & U] with CyclicMemoizerSignature[K, V]:
      override def domain: Set[K] !! ThisEffect = Storage.gets(_.keySet)

      override def toMap: Map[K, V] !! ThisEffect = Storage.gets(_.view.mapValues(_.apply()).toMap) //@#@TODO mapValues not strict yet

      override def memo(k: K): (() => V) !! ThisEffect =
        Storage.get.flatMap: m =>
          m.get(k) match
            case Some(thunk) => !!.pure(thunk)
            case None =>
              val thunk = new Thunk[V]
              for
                _ <- Storage.put(m.updated(k, thunk))
                v <- Control.reinterpret(f(k))
                _ = { thunk := v }
              yield thunk

    .toHandler
    .partiallyProvideWith[U](Storage.handler(Map()))
    .dropState


private class Thunk[A] extends Function0[A]:
  private var result: A = null.asInstanceOf[A]
  def :=(value: A): Unit = result = value
  override def apply(): A = result
