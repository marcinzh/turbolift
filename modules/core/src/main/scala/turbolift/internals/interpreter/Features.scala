package turbolift.internals.interpreter
import turbolift.!!

/** Mixin traits for `Interpreter`.
 *
 *  User-defined interpreters choose to inherit from one of those mixins,
 *  depending on their intended approach to parallellism.
 */
object Features:
  private def unimplemented: Nothing = sys.error("Intentionally unimplemented")

  protected sealed trait HasNotUnpure extends Interpreter.FlowFeatures:
    final override def onUnpure[A](aa: To[A]): A !! ThisEffect = unimplemented

  protected sealed trait HasNotForkJoin extends Interpreter.FlowFeatures:
    final override def onFork(s: Stan): (Stan, Stan) = unimplemented
    final override def onJoin(s1: Stan, s2: Stan): Stan = unimplemented

  protected sealed trait HasNotZip extends Interpreter.FlowFeatures:
    final override def onZip[A, B, C](aa: To[A], bb: To[B], k: (A, B) => C): To[C] = unimplemented
  

  /** Mixin trait for interpreters, that prohibit parallelism. */
  trait Sequential extends HasNotZip with HasNotForkJoin with HasNotUnpure:
    private[internals] final override def makeFeatureBits: Int = Bits.IsSequential


  /** Mixin trait for interpreters, that allow parallelism. */
  trait Parallel extends Parallel.Default

  object Parallel:
    trait Default extends HasNotForkJoin:
      private[internals] final override def makeFeatureBits: Int = Bits.HasZip | Bits.HasUnpure

    trait Trivial extends HasNotZip with HasNotForkJoin with HasNotUnpure:
      private[internals] final override def makeFeatureBits: Int = 0

    trait ForkJoin extends Interpreter.FlowFeatures:
      private[internals] final override def makeFeatureBits: Int = Bits.HasZip | Bits.HasUnpure | Bits.HasForkJoin
      //@#@TODO
      final override def onJoin(s1: Stan, s2: Stan): Stan = unimplemented
