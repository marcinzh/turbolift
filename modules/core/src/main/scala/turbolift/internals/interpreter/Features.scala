package turbolift.internals.interpreter
import turbolift.!!

/** Mixin traits for `Interpreter`.
 *
 *  User-defined interpreters choose to inherit from one of those mixins,
 *  depending on their intended approach to parallellism.
 */
object Features:
  private def unimplemented: Nothing = sys.error("Intentionally unimplemented")


  protected sealed trait HasUnpure extends Interpreter.FlowFeatures:
    final override val hasUnpure: Boolean = true

  protected sealed trait HasNotUnpure extends Interpreter.FlowFeatures:
    final override val hasUnpure: Boolean = false
    final override def onUnpure[A](aa: Result[A]): A !! ThisEffect = unimplemented


  protected sealed trait HasForkJoin extends Interpreter.FlowFeatures:
    final override val hasForkJoin: Boolean = true
    //@#@TODO
    final override def onJoin(s1: Stan, s2: Stan): Stan = unimplemented

  protected sealed trait HasNotForkJoin extends Interpreter.FlowFeatures:
    final override val hasForkJoin: Boolean = false
    final override def onFork(s: Stan): (Stan, Stan) = unimplemented
    final override def onJoin(s1: Stan, s2: Stan): Stan = unimplemented


  protected sealed trait HasZip extends Interpreter.FlowFeatures:
    final override val hasZip: Boolean = true

  protected sealed trait HasNotZip extends Interpreter.FlowFeatures:
    final override val hasZip: Boolean = false
    final override def onZip[A, B, C](aa: Result[A], bb: Result[B], k: (A, B) => C): Result[C] = unimplemented
  

  protected sealed trait IsParallelizable extends Interpreter.FlowFeatures:
    final override val isParallelizable: Boolean = true

  protected sealed trait IsNotParallelizable extends Interpreter.FlowFeatures:
    final override val isParallelizable: Boolean = false


  /** Mixin trait for interpreters, that prohibit parallelism. */
  trait Sequential extends IsNotParallelizable with HasNotZip with HasNotForkJoin with HasNotUnpure

  /** Mixin trait for interpreters, that allow parallelism. */
  trait Parallel extends Parallel.Default

  object Parallel:
    trait Default extends IsParallelizable with HasZip with HasNotForkJoin with HasUnpure

    trait Trivial extends IsParallelizable with HasNotZip with HasNotForkJoin with HasNotUnpure

    trait ForkJoin extends IsParallelizable with HasZip with HasForkJoin with HasUnpure
