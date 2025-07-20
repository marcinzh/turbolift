package turbolift.effects
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.typeclass.{PlusZero, AccumZero}
import turbolift.typeclass.Syntax._
import turbolift.io.AtomicVar


/** Signature of [[WriterEffectExt]].
 *
 * @tparam W Accumulator
 * @tparam W1 Singular value added to the accumulator
 */
trait WriterSignature[W, W1] extends Signature:
  def tell(w: W1): Unit !! ThisEffect
  def tells(w: W): Unit !! ThisEffect
  def mute[A, U <: ThisEffect](body: A !! U): A !! U
  def listen[A, U <: ThisEffect](body: A !! U): (A, W) !! U
  def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !! U
  def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !! U


/** Base trait for custom instances of Writer effect.
 *
 * {{{
 * case object MyWriter extends WriterEffect[Int]
 * // optional:
 * type MyWriter = MyWriter.type
 * }}}
 *
 * Notice that [[WriterEffectExt]] takes type 2 parameters.
 * This abstraction enables ergonomic syntax of `tell(w)`,
 * which relieves the user from the necessity of wrapping `w` in `Nel` (`raise(Nel(w))`),
 * as in the standard `Writer` monad.
 * For the simpler, single-parmeter version, see [[WriterEffect]].
 *
 * @see [[WriterEffect]]
 * @see [[WriterEffectK]]
 * @see [[WriterEffectG]]
 * @see [[WriterEffectGK]]
 * @see [[PolyWriterEffect]]
 * @see [[Writer]]
 *
 * @tparam W Accumulator
 * @tparam W1 Singular value added to the accumulator
 */
trait WriterEffectExt[W, W1] extends Effect[WriterSignature[W, W1]] with WriterSignature[W, W1]:
  enclosing =>
  final override def tell(w: W1): Unit !! this.type = perform(_.tell(w))
  final override def tells(w: W): Unit !! this.type = perform(_.tells(w))
  final override def mute[A, U <: this.type](body: A !! U): A !! U = perform(_.mute(body))
  final override def listen[A, U <: this.type](body: A !! U): (A, W) !! U = perform(_.listen(body))
  final override def censor[A, U <: this.type](f: W => W)(body: A !! U): A !! U = perform(_.censor(f)(body))
  final override def pass[A, U <: this.type](body: (A, W => W) !! U): A !! U = perform(_.pass(body))

  final def tell[K, V1](k: K, v: V1)(using ev: ((K, V1)) <:< W1): Unit !! this.type = tell(ev((k, v)))

  /** Predefined handlers for this effect. */
  object handlers:
    def local(using W: AccumZero[W, W1]): Handler[Identity, (_, W), enclosing.type, Any] =
      new impl.Stateful[Identity, (_, W), Any] with impl.Parallel.ForkJoin with WriterSignature[W, W1]:
        override type Local = W
        override def onInitial: W !! Any = !!.pure(W.zero)
        override def onReturn(a: Unknown, w: W): (Unknown, W) !! Any = !!.pure((a, w))
        override def onRestart(a_w: (Unknown, W)): Unknown !! enclosing.type = enclosing.tells(a_w._2) &&! !!.pure(a_w._1)
        override def onUnknown(aa: (Unknown, W)): Option[Unknown] = Some(aa._1)

        override def onZip[A, B, C](a_w: (A, W), b_w: (B, W), k: (A, B) => C): (C, W) = (k(a_w._1, b_w._1), a_w._2 |+| b_w._2)
        override def onFork(w: W): (W, W) = (w, W.zero)
        override def tell(w: W1): Unit !! ThisEffect = Local.modify(_ |+ w)
        override def tells(w: W): Unit !! ThisEffect = Local.modify(_ |+| w)
        override def mute[A, U <: ThisEffect](body: A !! U): A !! U = censor(_ => W.zero)(body)

        override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !! U =
          Control.delimitPut(body, W.zero).flatMap:
            case aw @ (a, w) => Local.modify(_ |+| w).as(aw)

        override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !! U =
          Control.delimitPut(body, W.zero).flatMap:
            case aw @ (a, w) => Local.modify(_ |+| f(w)).as(a)

        override def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !! U =
          Control.delimitPut(body, W.zero).flatMap:
            case ((a, f), w) => Local.modify(_ |+| f(w)).as(a)
      .toHandler


    def shared(using W: AccumZero[W, W1]): Handler[Identity, (_, W), enclosing.type, IO] =
      AtomicVar(W.zero).flatMapHandler: avar =>
        new impl.Proxy[IO] with WriterSignature[W, W1]:
          override def tell(w: W1): Unit !! ThisEffect = avar.modify(_ |+ w)
          override def tells(w: W): Unit !! ThisEffect = avar.modify(_ |+| w) 
          override def mute[A, U <: ThisEffect](body: A !! U): A !! U = censor(_ => W.zero)(body)

          override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !! U =
            for
              w0 <- avar.swap(W.zero)
              a <- body
              w1 <- avar.getModify(w0 |+| _)
            yield (a, w1)

          override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !! U =
            for
              w0 <- avar.swap(W.zero)
              a <- body
              _ <- avar.modify(w => w0 |+| f(w))
            yield a

          override def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !! U =
            for
              w0 <- avar.swap(W.zero)
              workaround <- body
              (a, f) = workaround
              _ <- avar.modify(w => w0 |+| f(w))
            yield a
        .toHandler
        .mapEffK([A] => (a: A) => avar.get.map((a, _)))


    /** Lile [[local]], but accumulate with given function instead of typeclass. */
    def localFold(using W =:= W1)(zero: W, plus: (W, W1) => W): Handler[Identity, (_, W), enclosing.type, Any] = local(using AccumZero.instanceEq(zero, plus))

    /** Lile [[shared]], but accumulate with given function instead of typeclass. */
    def sharedFold(using W =:= W1)(zero: W, plus: (W, W1) => W): Handler[Identity, (_, W), enclosing.type, IO] = shared(using AccumZero.instanceEq(zero, plus))


object WriterEffectExt:
  extension [W, W1](thiz: WriterEffectExt[W, W1])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler(using W: AccumZero[W, W1]): Handler[Identity, (_, W), thiz.type, Any] = thiz.handlers.local


/** Specialized [[WriterEffectExt]], where `W` and `W1` are the same (e.g. a monoid). */
trait WriterEffect[W] extends WriterEffectExt[W, W]

/** Specialized [[WriterEffectExt]], where values `W` are accumulated into `F[W]` (e.g. a collection). */
trait WriterEffectK[F[_], W] extends WriterEffectExt[F[W], W]

/** Specialized [[WriterEffectExt]], where pairs `(K, V)` are accumulated into `M[K, V]` (e.g. a map). */
trait WriterEffectG[M[_, _], K, V] extends WriterEffectExt[M[K, V], (K, V)]

/** Specialized [[WriterEffectExt]], where pairs `(K, V)` are accumulated into `M[K, F[V]]` (e.g. a map of collections). */
trait WriterEffectGK[M[_, _], K, F[_], V] extends WriterEffectExt[M[K, F[V]], (K, V)]


/** Polymorphic variant of [[WriterEffect]].
 *
 * In the monomorphic variant, the `W` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * // The `W` is explicitly set as `Int`:
 * case object MyWriter extends WriterEffect[Int]  
 *
 * // The `W` is inferred from the effect instance:
 * val computation = MyWriter.tell(42)
 * }}}
 *
 * In the polymorphic variant, the `W` type parameter is **contravariantly** inferred
 * at call sites of effect's operations and handlers.
 * In practice, the type can "grow as you go":
 *
 * {{{
 * case object MyWriter extends PolyWriterEffect
 *
 * val computation1 = MyWriter.tell(42)              // `W` inferred as `Int`
 * val computation2 = MyWriter.tell("OMG")           // `W` inferred as `String`
 * val computation3 = computation1 &&! computation2  // `W` inferred as `Int | String`
 *
 * // Inferred types of the above computations:
 * val _: Unit !! MyWriter.@@[Int]          = computation1
 * val _: Unit !! MyWriter.@@[String]       = computation2
 * val _: Unit !! MyWriter.@@[Int | String] = computation3
 * }}}
 */
abstract class PolyWriterEffect extends Effect.Polymorphic_-[WriterEffect, Any](new WriterEffect[Any] {}):
  final def tell[W](w: W): Unit !! @@[W] = polymorphize[W].perform(_.tell(w))
  final def mute[W] = MuteApply[W]
  final def listen[W] = ListenApply[W]
  final def censor[W] = CensorApply[W]
  final def pass[W] = PassApply[W]


  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class MuteApply[W]:
    def apply[A, U <: @@[W]](body: A !! U): A !! U = polymorphize[W].perform(_.mute(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class ListenApply[W]:
    def apply[A, U <: @@[W]](body: A !! U): (A, W) !! U = polymorphize[W].perform(_.listen(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CensorApply[W]:
    def apply[A, U <: @@[W]](f: W => W)(body: A !! U): A !! U = polymorphize[W].perform(_.censor(f)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class PassApply[W]:
    def apply[A, U <: @@[W]](body: (A, W => W) !! U): A !! U = polymorphize[W].perform(_.pass(body))


  /** Predefined handlers for this effect. */
  object handlers:
    def local[W](using W: PlusZero[W]): Handler[Identity, (_, W), @@[W], Any] = polymorphize[W].handler(_.handlers.local)
    def shared[W](using W: PlusZero[W]): Handler[Identity, (_, W), @@[W], IO] = polymorphize[W].handler(_.handlers.shared)

    /** Lile [[local]], but accumulate with given function instead of typeclass. */
    def localFold[W](zero: W, plus: (W, W) => W): Handler[Identity, (_, W), @@[W], Any] = local(using PlusZero.instance(zero, plus))

    /** Lile [[shared]], but accumulate with given function instead of typeclass. */
    def sharedFold[W](zero: W, plus: (W, W) => W): Handler[Identity, (_, W), @@[W], IO] = shared(using PlusZero.instance(zero, plus))


object PolyWriterEffect:
  extension (thiz: PolyWriterEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler[W](using W: PlusZero[W]): Handler[Identity, (_, W), thiz.@@[W], Any] = thiz.handlers.local


/** Predefined instance of [[PolyWriterEffect]].
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object Writer extends PolyWriterEffect
type Writer[W] = Writer.@@[W]
