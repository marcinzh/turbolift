package turbolift.interpreter
import turbolift.!!


/** Mixin traits for `Interpreter`.
 *
 *  User-defined interpreters choose to inherit from one of those mixins,
 *  depending on their intended approach to parallellism.
 *  
 *  No need to import these definitions, because [[turbolift.Effect]] trait exports them for convenience.
 */
object Mixins:
  private def unimplemented: Nothing = sys.error("Intentionally unimplemented")

  protected sealed trait HasNotReintro extends Interpreter.Unsealed:
    final override def onReintro(aa: To[Unknown]): Unknown !! ThisEffect = unimplemented

  protected sealed trait HasNotForkJoin extends Interpreter.Unsealed:
    final override def onFork(s: Stan): (Stan, Stan) = unimplemented
    final override def onJoin(s1: Stan, s2: Stan): Stan = unimplemented

  protected sealed trait HasNotZip extends Interpreter.Unsealed:
    final override def onZip[A, B, C](aa: To[A], bb: To[B], k: (A, B) => C): To[C] = unimplemented
  
  //@#@TODO remove
  private[interpreter] trait Root extends HasNotZip with HasNotForkJoin with HasNotReintro:
    private[turbolift] final override def makeFeatures: Features = Features.Root | Features.Stateful
    final override def onInitial: Stan !! Dependency = unimplemented


  /** Mixin trait for interpreters, that prohibit parallelism. */
  trait Sequential extends HasNotZip with HasNotForkJoin with HasNotReintro:
    private[turbolift] final override def makeFeatures: Features = Features.Sequential


  /** Mixin trait for interpreters, that allow parallelism. */
  trait Parallel extends Parallel.Default

  object Parallel:
    trait Default extends HasNotForkJoin:
      private[turbolift] final override def makeFeatures: Features = Features.Zip | Features.Reintro

    trait Trivial extends HasNotZip with HasNotForkJoin with HasNotReintro:
      private[turbolift] final override def makeFeatures: Features = Features.Empty

    trait ForkJoin extends Interpreter.Unsealed:
      private[turbolift] final override def makeFeatures: Features = Features.Zip | Features.Reintro | Features.ForkJoin
      //@#@TODO
      final override def onJoin(s1: Stan, s2: Stan): Stan = unimplemented
