package turbolift.extra_effects
import cats.Id
import turbolift.abstraction.!!
import turbolift.std_effects.State


object CyclicMemoizerHandler {
  def apply[K, V, Fx <: CyclicMemoizer[K, V]](fx: Fx): fx.ThisIHandler[Id] = {
    case object Storage extends State[Map[K, Thunk[V]]]

    new fx.Dependent[Storage.type] {
      override def interpret[U <: Storage.type] = new CyclicMemoizerSig[U, K, V] {
        override def get: Map[K, V] !! U =
          Storage.gets(_.view.mapValues(_.apply()).toMap) //@#@TODO mapValues not strict yet

        override def memo(f: K => V !! U)(k: K): (() => V) !! U =
          Storage.get.flatMap { m =>
            m.get(k) match {
              case Some(thunk) => !!.pure(thunk)
              case None =>
                val thunk = new Thunk[V]
                for {
                  _ <- Storage.put(m.updated(k, thunk))
                  v <- f(k)
                  _ = { thunk := v }
                } yield thunk
            }
          }
      }
    }
    .toHandler
    .provideWith(Storage.handler(Map()))
    .dropState
  }

  private class Thunk[A] extends Function0[A] {
    private var result: A = null.asInstanceOf[A]
    def :=(value: A): Unit = result = value
    override def apply(): A = result
  }
}
