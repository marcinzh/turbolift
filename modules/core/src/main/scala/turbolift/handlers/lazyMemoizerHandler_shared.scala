package turbolift.handlers
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import turbolift.{!!, Handler}
import turbolift.io.OnceVar
import turbolift.effects.{LazyMemoizerEffect, LazyMemoizerSignature, IO}
import turbolift.Extensions._


extension [K, V](fx: LazyMemoizerEffect[K, V])
  def lazyMemoizerHandler_shared[U <: IO](f: K => V !! (U & fx.type)): fx.ThisHandler[Identity, Identity, U] =
    Handler.flatHandle:
      IO(new ConcurrentHashMap[K, OnceVar[V]]).map: storage =>
        new fx.impl.Proxy[U] with LazyMemoizerSignature[K, V]:
          override def domain: Set[K] !! ThisEffect =
            IO(storage.keySet().nn.asScala.toSet)

          override def toMap: Map[K, V] !! ThisEffect =
            for
              entries <- IO(storage.entrySet().nn.iterator().nn.asScala)
              m <- entries.foldLeftEff(Map[K, V]()): (m, entry) =>
                val k = entry.getKey.nn
                val ovar = entry.getValue.nn
                ovar.get.map(v => m + ((k, v)))
            yield m

          override def memo(k: K): (() => V) !! ThisEffect =
            IO:
              var wasFirst = false
              val ovar = storage.computeIfAbsent(k, _ => { wasFirst = true; OnceVar.unsafeCreate[V]() }).nn
              !!.when(wasFirst):
                Control.reinterpret(f(k)).flatMap(ovar.put)
              .as(ovar.unsafeAsThunk)
            .flatten

        .toHandler
