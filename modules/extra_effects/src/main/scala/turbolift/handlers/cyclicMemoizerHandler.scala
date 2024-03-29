package turbolift.handlers
import turbolift.!!
import turbolift.effects.State
import turbolift.effects.{CyclicMemoizer, CyclicMemoizerSignature}


extension [K, V](fx: CyclicMemoizer[K, V])
  def cyclicMemoizerHandler: fx.ThisHandler.FromId.ToId.Free =
    case object Storage extends State[Map[K, Thunk[V]]]

    new fx.impl.Proxy[Storage.type] with CyclicMemoizerSignature[K, V]:
      override def domain: Set[K] !@! ThisEffect = Storage.gets(_.keySet)

      override def toMap: Map[K, V] !@! ThisEffect = Storage.gets(_.view.mapValues(_.apply()).toMap) //@#@TODO mapValues not strict yet

      override def memo[U <: ThisEffect](f: K => V !! U)(k: K): (() => V) !@! U =
        Storage.get.flatMap: m =>
          m.get(k) match
            case Some(thunk) => !!.pure(thunk)
            case None =>
              val thunk = new Thunk[V]
              for
                _ <- Storage.put(m.updated(k, thunk))
                v <- f(k)
                _ = { thunk := v }
              yield thunk

    .toHandler
    .provideWith(Storage.handler(Map()))
    .dropState


private class Thunk[A] extends Function0[A]:
  private var result: A = null.asInstanceOf[A]
  def :=(value: A): Unit = result = value
  override def apply(): A = result
