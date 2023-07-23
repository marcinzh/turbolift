package turbolift.internals.interpreter
import turbolift.{!!, Signature, Handler, HandlerCases => HC}
import turbolift.effects.{ChoiceSignature, IO}


/** Super trait for any user-defined [[Interpreter Interpreter]].
  *
  * 1. Interpreters are not meant to be **created directly**.
  * Instead, one of abstract interpreters, provided by [[turbolift.Effect Effect]], should be inherited:
  *
  *    - [[turbolift.Effect.Proxy Effect.Proxy]]
  *    - [[turbolift.Effect.Stateless Effect.Stateless]]
  *    - [[turbolift.Effect.Stateful Effect.Stateful]]
  *
  * 2. Interpreters are not meant to be **used directly**.
  * Instead, a [[turbolift.Handler Handler]] should be created from the interpreter,
  * by calling `toHandler` method on it.
  */
sealed trait Interpreter extends Signature:
  type Result[+A]
  type Dependency

  /** Alias for [[Handler]], specialized for this interperter. */
  final type ThisHandler = Handler[Result, ThisEffect, Dependency]

  private[turbolift] val signatures: Array[Signature]
  private[internals] final val bits: Int = makeBits

  private[internals] final def isFlow: Boolean            = !isProxy
  private[internals] final def isProxy: Boolean           = (bits & Bits.IsProxy) != 0
  private[internals] final def isProxyIO: Boolean         = (bits & Bits.IsProxyIO) != 0
  private[internals] final def isStateful: Boolean        = (bits & Bits.IsStateful) != 0
  private[internals] final def isStateless: Boolean       = !isStateful
  private[internals] final def isChoice: Boolean          = (bits & Bits.IsChoice) != 0
  private[internals] final def isSequential: Boolean      = (bits & Bits.IsSequential) != 0
  private[internals] final def isParallelizable: Boolean  = !isSequential
  private[internals] final def hasZip: Boolean            = (bits & Bits.HasZip) != 0
  private[internals] final def hasForkJoin: Boolean       = (bits & Bits.HasForkJoin) != 0
  private[internals] final def hasUnpure: Boolean         = (bits & Bits.HasUnpure) != 0

  private[internals] def makeBits: Int = if isInstanceOf[ChoiceSignature] then Bits.IsChoice else 0    

  private[internals] final def untyped: Interpreter.Untyped = asInstanceOf[Interpreter.Untyped]


object Interpreter:
  private[turbolift] type Apply[F[+_], L, N] = Interpreter {
    type Result[+X] = F[X]
    type ThisEffect = L
    type Dependency = N
  }

  private[turbolift] type Untyped = Interpreter {
    type Result[+X] = X
    type Dependency = Any
    type ThisEffect = Any
    type Stan = Any
  }

  private[internals] type FlowUntyped = Flow {
    type Result[+X] = X
    type Dependency = Any
    type ThisEffect = Any
    type Stan = Any
  }

  private[turbolift] trait Unsealed extends Interpreter //// subclassed by Effect

  /** Super trait for any user-defined proxy [[Interpreter]].
    *
    * [[Proxy]] translates operations of this effect, into operations of some other effects (dependencies).
    * This is also known as "reinterpretation" in some effect systems.
    * 
    * @tparam Fx Type-level set of effects, specifying dependencies of this proxy interpreter.
    */
  abstract class Proxy[Fx] extends Interpreter:
    final override type Dependency = Fx
    final override type Result[+A] = A
    final override type !@![A, U] = Control.Proxy[U, Dependency] => A !! Dependency
    private[internals] final override def makeBits = super.makeBits | Bits.IsProxy

    final def toHandler: ThisHandler = HC.Primitive[Result, ThisEffect, Dependency](this, Void)


  /** Like `Proxy[IO]`, but with less overhead. */
  abstract class ProxyIO extends Interpreter:
    final override type Dependency = IO
    final override type Result[+A] = A
    final override type !@![A, U] = A !! Dependency
    private[internals] final override def makeBits = super.makeBits | Bits.IsProxy | Bits.IsProxyIO

    final def toHandler: ThisHandler = HC.Primitive[Result, ThisEffect, Dependency](this, Void)


  abstract class FlowFeatures extends Interpreter: //// subclassed by Effect
    private[internals] def makeFeatureBits: Int
    private[internals] override def makeBits = super.makeBits | makeFeatureBits

    /** State of this interpreter. Named `Stan`, to avoid confusion with `State` effect. */
    type Stan

    def onPure[A](a: A, s: Stan): Result[A]
    def onUnpure[A](aa: Result[A]): A !! ThisEffect
    def onZip[A, B, C](aa: Result[A], bb: Result[B], k: (A, B) => C): Result[C]
    def onFork(s: Stan): (Stan, Stan)
    def onJoin(s1: Stan, s2: Stan): Stan


  /** Super class for interpreters using delimited continuation. */
  sealed abstract class Flow extends FlowFeatures:
    enclosing =>
    final override type Dependency = Any

    /** Free variable, meaning the unknown part of the continuation's answer type.
      *
      * The full answer type is `Result[Unknown]`.
      */
    type Unknown

    /** Alias for [[Control]], specialized for this interpreter. */
    type ThisControl[-A, U] = Control.Flow[A, Unknown, Stan, Result, U, Any]


  /** Super class for any user-defined [[Flow Flow]] Interpreter, that has no internal state.
    * 
    * @tparam F Result for this interpreter.
    */
  abstract class Stateless[F[+_]] extends Flow: //// subclassed by Effect
    final override type Result[+A] = F[A]
    final override type !@![A, U] = ThisControl[A, U] => Result[Unknown] !! Any
    final override type Stan = Void
    private[internals] final override def makeBits = super.makeBits

    final override def onPure[A](a: A, s: Void): Result[A] = onPure(a)
    def onPure[A](a: A): Result[A]

    /** Creates a [[turbolift.Handler Handler]] from this interpreter. */
    final def toHandler: ThisHandler = HC.Primitive[Result, ThisEffect, Dependency](this, Void)


  /** Super class for any user-defined stateful [[Flow Flow]] Interpreter, that has internal state.
    * 
    * @tparam S State for this interpreter.
    * @tparam F Result for this interpreter.
    */
  abstract class Stateful[S, F[+_]] extends Flow: //// subclassed by Effect
    final override type Result[+A] = F[A]
    final override type !@![A, U] = (ThisControl[A, U], S) => Result[Unknown] !! Any
    final override type Stan = S
    private[internals] final override def makeBits = super.makeBits | Bits.IsStateful

    /** Creates a [[turbolift.Handler Handler]] from this interpreter. */
    final def toHandler(initial: Stan): ThisHandler = HC.Primitive[Result, ThisEffect, Dependency](this, initial)
