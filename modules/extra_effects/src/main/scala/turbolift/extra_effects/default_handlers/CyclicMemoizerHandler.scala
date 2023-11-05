package turbolift.extra_effects.default_handlers
import turbolift.!!
import turbolift.effects.State
import turbolift.extra_effects.{CyclicMemoizer, CyclicMemoizerSignature}


extension [K, V](fx: CyclicMemoizer[K, V])
  private[extra_effects] def cyclicMemoizerHandler: fx.ThisHandler.Free.Id =
    case object Storage extends State[Map[K, Thunk[V]]]

    new fx.impl.Proxy[Storage.type] with CyclicMemoizerSignature[K, V]:
      override def domain: Set[K] !@! ThisEffect = _ => Storage.gets(_.keySet)

      override def toMap: Map[K, V] !@! ThisEffect = _ => Storage.gets(_.view.mapValues(_.apply()).toMap) //@#@TODO mapValues not strict yet

      override def memo[U <: ThisEffect](f: K => V !! U)(k: K): (() => V) !@! U =
        kk => Storage.get.flatMap { m =>
          m.get(k) match
            case Some(thunk) => !!.pure(thunk)
            case None =>
              val thunk = new Thunk[V]
              for
                _ <- Storage.put(m.updated(k, thunk))
                v <- kk.escape(f(k))
                _ = { thunk := v }
              yield thunk
        }

    .toHandler
    .provideWith(Storage.handler(Map()))
    .dropState


private class Thunk[A] extends Function0[A]:
  private var result: A = null.asInstanceOf[A]
  def :=(value: A): Unit = result = value
  override def apply(): A = result
