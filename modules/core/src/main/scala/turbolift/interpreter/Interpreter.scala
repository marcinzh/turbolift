package turbolift.interpreter
import turbolift.{!!, Signature, Handler, HandlerCases => HC}
import turbolift.effects.{ChoiceSignature, IO}
import turbolift.internals.effect.AnyChoice


/** Super trait for any user-defined [[Interpreter Interpreter]].
  *
  * 1. Interpreters are not meant to be **created directly**.
  * Instead, one of abstract interpreters, provided by [[turbolift.Effect Effect]], should be inherited:
  *
  *    - [[Interpreter.Proxy Effect.impl.Proxy]]
  *    - [[Interpreter.Stateless Effect.impl.Stateless]]
  *    - [[Interpreter.Stateful Effect.impl.Stateful]]
  *
  * 2. Interpreters are not meant to be **used directly**.
  * Instead, a [[turbolift.Handler Handler]] should be created from the interpreter,
  * by calling `toHandler` method on it.
  */
sealed trait Interpreter extends Signature:
  /** Input of this interpreter. */
  type From[+A]

  /** Output of this interpreter. */
  type To[+A]

  /** Set of effects that this interpreter depends on. */
  type Dependency

  /** State of this interpreter. Named `Stan`, to avoid confusion with `State` effect. */
  type Stan

  /** Phantom type meaning the unknown part of the continuation's answer type.
    *
    * Full answer type is `To[Unknown] !! Ambient`.
    * The `To[+_]` part is known to this interpreter.
    * The `Unknown` and `Ambient` parts however, are not.
    * They specific to place(s) where the handler (obtained from this interpreter) would be applied.
    */
  type Unknown

  /** Phantom type meaning set of effects remaining after handling this effect. */
  type Ambient

  /** Alias for [[turbolift.Handler Handler]], specialized for this interperter. */
  final type ThisHandler = Handler[From, To, ThisEffect, Dependency]

  /** Alias for [[Control]], specialized for this interperter. */
  final type ThisControl[-A, U] = Control[A, Unknown, Stan, To, U & Dependency, Ambient]

  final type NullarySem[A, U] = A !! (U & Dependency)
  final type UnarySem[A, U] = ThisControl[A, U] => To[Unknown] !! Ambient
  final type BinarySem[A, U] = (ThisControl[A, U], Stan) => To[Unknown] !! Ambient

  def onInitial: Stan !! Dependency
  def onReturn(aa: From[Unknown], s: Stan): To[Unknown] !! Ambient
  def onRestart(aa: To[Unknown]): Unknown !! ThisEffect
  def onZip[A, B, C](aa: To[A], bb: To[B], k: (A, B) => C): To[C]
  def onFork(s: Stan): (Stan, Stan)
  def onJoin(s1: Stan, s2: Stan): Stan

  def tailResumptiveHint: Boolean = false
  def topmostOnlyHint: Boolean = false
  def multishotHint: Boolean = false
  def resurceUnsafeHint: Boolean = false

  /** Creates a [[turbolift.Handler Handler]] from this interpreter. */
  final def toHandler: ThisHandler = HC.Primitive[From, To, ThisEffect, Dependency](this)

  final val features: Features =
    val primary = Seq(
      Features.cond(Features.Choice, isInstanceOf[ChoiceSignature]),
      Features.cond(Features.Stateful, isInstanceOf[Interpreter.Stateful[?, ?, ?]]),
      Features.cond(Features.Io, this eq Interpreter.Io),
      Features.cond(Features.Restart, !isInstanceOf[Mixins.HasNotRestart]),
      Features.cond(Features.ForkJoin, !isInstanceOf[Mixins.HasNotForkJoin]),
      Features.cond(Features.Zip, !isInstanceOf[Mixins.HasNotZip]),
      Features.cond(Features.TailResump, tailResumptiveHint),
    ).reduce(_ | _)
    Seq(
      primary,
      Features.cond(Features.Stateful, primary.isIo),
      Features.cond(Features.Sequential, !(primary.hasZip | isInstanceOf[Mixins.Parallel.Trivial])),
    ).reduce(_ | _)


  private[turbolift] def enumSignatures: Array[Signature]
  private[turbolift] final val signatures: Array[Signature] =
    val sigs = enumSignatures
    if features.isChoice then
      sigs :+ AnyChoice
    else
      sigs

  private[turbolift] final def untyped: Interpreter.Untyped = asInstanceOf[Interpreter.Untyped]



object Interpreter:
  private[turbolift] type Apply[F[+_], G[+_], L, N] = Interpreter {
    type From[+X] = F[X]
    type To[+X] = G[X]
    type ThisEffect = L
    type Dependency = N
  }

  private[turbolift] type Untyped = Interpreter {
    type From[+X] = X
    type To[+X] = X
    type ThisEffect = Any
    type Dependency = Any
    type Stan = Any
    type Unknown = Any
  }


  private[turbolift] trait Unsealed extends Interpreter //// subclassed by Effect and Features

  /** Base class for any user-defined proxy [[Interpreter]].
    *
    * [[Proxy]] translates operations of this effect, into operations of some other effects (dependencies).
    * This is also known as "reinterpretation" in some effect systems.
    * 
    * @tparam Fx dependencies of this interpreter.
    */
  abstract class Proxy[Fx] extends Interpreter with Mixins.Parallel.Trivial: //// subclassed by Effect
    final override type From[+A] = A
    final override type To[+A] = A
    final override type Dependency = Fx
    final override type Stan = Void
    final override type !@![A, U] = NullarySem[A, U]

    final override def onInitial: Stan !! Dependency = Void.pure
    final override def onReturn(a: Unknown, s: Void): Unknown !! Ambient = !!.pure(a)


  /** Base class for any user-defined [[Interpreter]], that has no internal state.
    *
    * User-defined [[Stateless]] interpreter must also inherit one of
    * [[Features.Sequential Sequential]] or
    * [[Mixins.Parallel Parallel]] mixins.
    *
    * @tparam F Input of this interpreter.
    * @tparam G Output of this interpreter.
    * @tparam Fx dependencies of this interpreter.
    */
  abstract class Stateless[F[+_], G[+_], Fx] extends Interpreter: //// subclassed by Effect
    final override type From[+A] = F[A]
    final override type To[+A] = G[A]
    final override type Dependency = Fx
    override type Ambient <: Fx
    final override type Stan = Void
    final override type !@![A, U] = NullarySem[A, U] | UnarySem[A, U]

    final override def onInitial: Stan !! Dependency = Void.pure
    final override def onReturn(aa: From[Unknown], s: Void): To[Unknown] !! Ambient = onReturn(aa)
    def onReturn(aa: From[Unknown]): To[Unknown] !! Ambient


  /** Base class for any user-defined stateful [[Interpreter]] Interpreter, that has internal state.
    * 
    * User-defined [[Stateful]] interpreter must also inherit one of
    * [[Features.Sequential Sequential]] or
    * [[Mixins.Parallel Parallel]] mixins.
    *
    * @tparam F Input of this interpreter.
    * @tparam G Output of this interpreter.
    * @tparam Fx dependencies of this interpreter.
    */
  abstract class Stateful[F[+_], G[+_], Fx] extends Interpreter: //// subclassed by Effect
    final override type From[+A] = F[A]
    final override type To[+A] = G[A]
    final override type Dependency = Fx
    override type Ambient <: Fx
    final override type !@![A, U] = NullarySem[A, U] | BinarySem[A, U]


  private[turbolift] case object Io extends Mixins.Parallel.Trivial:
    final override type From[+A] = A
    final override type To[+A] = A
    final override type Dependency = Any
    final override type Stan = Any
    final override type Ambient = Any

    final override def onInitial: Stan !! Dependency = Mixins.unimplemented
    final override def onReturn(a: Unknown, s: Any): Unknown !! Any = !!.pure(a)
    override def enumSignatures = Array(IO) //// `IO` = interface, `Io` = implementation
