package turbolift.extra_effects
import turbolift.Effect
import turbolift.typeclass.AccumZero
import turbolift.std_effects.{Reader, WriterExt, State}
import turbolift.extra_effects.default_handlers.ReaderWriterStateHandler


object ReaderWriterState:
  object Syntax:
    extension [
      R, W, W1, S,
      FxR <: Reader[R],
      FxW <: WriterExt[W, W1],
      FxS <: State[S]
    ](thiz: Effect.Combine3[FxR, FxW, FxS])
      def handler(initialR: R, initialS: S)(implicit W: AccumZero[W, W1]): thiz.ThisHandler.Free[((W, S), _)] =
        ReaderWriterStateHandler.apply(thiz, initialR, initialS)
