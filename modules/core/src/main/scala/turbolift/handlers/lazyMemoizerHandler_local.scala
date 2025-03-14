package turbolift.handlers
import turbolift.!!
import turbolift.effects.{LazyMemoizerEffect, LazyMemoizerSignature, State}
import turbolift.Extensions._


extension [K, V](fx: LazyMemoizerEffect[K, V])
  def lazyMemoizerHandler_local[U](f: K => V !! (U & fx.type)): fx.ThisHandler[Identity, Identity, U] =
    new fx.impl.Stateful[Identity, Identity, U] with fx.impl.Sequential with LazyMemoizerSignature[K, V]:
      override type Local = Map[K, Thunk[V]]

      override def onInitial = Map().pure_!!

      override def onReturn(x: Unknown, s: Local) = x.pure_!!

      override def domain: Set[K] !! ThisEffect = Local.gets(_.keySet)

      override def toMap: Map[K, V] !! ThisEffect = Local.gets(_.view.mapValues(_.apply()).toMap) //@#@TODO mapValues not strict yet

      override def memo(k: K): (() => V) !! ThisEffect =
        Local.get.flatMap: m =>
          m.get(k) match
            case Some(thunk) => !!.pure(thunk)
            case None =>
              val thunk = new Thunk[V]
              for
                _ <- Local.put(m.updated(k, thunk))
                v <- Control.reinterpret(f(k))
                _ = { thunk := v }
              yield thunk

    .toHandler


private final class Thunk[A] extends Function0[A]:
  private var result: A = null.asInstanceOf[A]
  def :=(value: A): Unit = result = value
  override def apply(): A = result
