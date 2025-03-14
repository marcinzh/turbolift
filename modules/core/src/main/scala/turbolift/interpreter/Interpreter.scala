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

  /** Set of effects eliminated from computation by this interpreter. */
  type Elim

  /** Set of effects introduced into computation by this interpreter (a.k.a. dependencies). */
  type Intro

  /** Local state of this interpreter. */
  type Local

  /** Phantom type, representing universally quantified type of the corresponding handler.
    *
    * A [[turbolift.Handler Handler]] acts as a higher-rank function of type:
    * {{{
    * [A, U] => (From[A] !! (Elim & U)) => To[A] !! (Intro & U)
    * }}}
    *
    * The [[Unknown]] type of this interpreter corresponds to the `A` type parameter above.
    * See also [[Ambient]].
    */
  type Unknown

  /** Phantom type, representing universally quantified type of the corresponding handler.
    *
    * A [[turbolift.Handler Handler]] acts as a higher-rank function of type:
    * {{{
    * [A, U] => (From[A] !! (Elim & U)) => To[A] !! (Intro & U)
    * }}}
    *
    * The [[Ambient]] type of this interpreter corresponds to the `U` type parameter above.
    * See also [[Unknown]].
    */
  type Ambient

  final type ThisEffect = Intro & Ambient


  /** An instance of [[Local]] dedicated for this interpreter. */
  val Local = new turbolift.interpreter.Local[Local, ThisEffect](prompt)

  /** An instance of [[Control]] dedicated for this interpreter. */
  val Control = new turbolift.interpreter.Control[Local, Unknown, To, Elim, Intro, Ambient](prompt)

  def onInitial: Local !! Intro
  def onReturn(aa: From[Unknown], s: Local): To[Unknown] !! ThisEffect
  def onRestart(aa: To[Unknown]): Unknown !! (Elim & ThisEffect)
  def onUnknown(aa: To[Unknown]): Option[Unknown]
  def onZip[A, B, C](aa: To[A], bb: To[B], k: (A, B) => C): To[C]
  def onFork(s: Local): (Local, Local)
  def onJoin(s1: Local, s2: Local): Local

  /** Microoptimization hint, stating intent to use continuations.
   *
   *  Many effect handlers never capture the continuation, and this is the default expectation.
   *
   *  The `capture` will work regardless of value of this hint.
   *  However, if not overriden to [[true]], the first `capture` will be more costly.
   */
  def captureHint: Boolean = false

  /** Creates a [[turbolift.Handler Handler]] from this interpreter. */
  final def toHandler: Handler[From, To, Elim, Intro] = HC.Primitive[From, To, Elim, Intro](this)

  private[turbolift] final val features: Features =
    val primary = Seq(
      Features.cond(Features.Choice, isInstanceOf[ChoiceSignature]),
      Features.cond(Features.Stateful, isInstanceOf[Interpreter.Stateful[?, ?, ?]]),
      Features.cond(Features.Io, this eq Interpreter.Io),
      Features.cond(Features.Restart, !isInstanceOf[Mixins.HasNotRestart]),
      Features.cond(Features.ForkJoin, !isInstanceOf[Mixins.HasNotForkJoin]),
      Features.cond(Features.Zip, !isInstanceOf[Mixins.HasNotZip]),
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

  override def toString = enumSignatures.mkString("+")

  private[turbolift] final def untyped: Interpreter.Untyped = asInstanceOf[Interpreter.Untyped]

  final inline def prompt: Prompt = asInstanceOf[Interpreter.Untyped]
  final inline def localCount: Int = if features.isStateful then 1 else 0
  final inline def isIo: Boolean = features.isIo
  final inline def isStateful: Boolean = features.isStateful
  final inline def isStateless: Boolean = features.isStateless
  final inline def isParallel: Boolean = features.isParallel
  final inline def hasRestart: Boolean = features.hasRestart
  final inline def hasZip: Boolean = features.hasZip
  final inline def hasForkJoin: Boolean = features.hasForkJoin


object Interpreter:
  private[turbolift] type Apply[F[+_], G[+_], L, N] = Interpreter {
    type From[+X] = F[X]
    type To[+X] = G[X]
    type Elim = L
    type Intro = N
  }

  private[turbolift] type Untyped = Interpreter {
    type From[+X] = X
    type To[+X] = X
    type Elim = Any
    type Intro = Any
    type Local = Any
    type Unknown = Any
    type Ambient = Any
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
    final override type Intro = Fx
    final override type Local = Void

    final override def onInitial: Local !! Intro = Void.pure
    final override def onReturn(a: Unknown, s: Void): Unknown !! Any = !!.pure(a)


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
    final override type Intro = Fx
    final override type Local = Void

    final override def onInitial: Local !! Intro = Void.pure
    final override def onReturn(aa: From[Unknown], s: Void): To[Unknown] !! ThisEffect = onReturn(aa)
    def onReturn(aa: From[Unknown]): To[Unknown] !! ThisEffect


  /** Base class for any user-defined [[Interpreter]] Interpreter, that has local state.
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
    final override type Intro = Fx


  private[turbolift] case object Io extends Mixins.Parallel.Trivial:
    final override type From[+A] = A
    final override type To[+A] = A
    final override type Intro = Any
    final override type Local = Any

    final override def onInitial: Local !! Intro = Mixins.unimplemented
    final override def onReturn(a: Unknown, s: Any): Unknown !! Any = !!.pure(a)
    override def enumSignatures = Array(IO) //// `IO` = interface, `Io` = implementation
