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
  private[interpreter] def unimplemented: Nothing = sys.error("Intentionally unimplemented")

  private[interpreter] sealed trait HasNotRestart extends Interpreter.Unsealed:
    final override def onRestart(aa: To[Unknown]): Unknown !! Elim = unimplemented
    final override def onOnce(aa: To[Unknown]): Option[Unknown] = unimplemented

  private[interpreter] sealed trait HasNotForkJoin extends Interpreter.Unsealed:
    final override def onFork(s: Local): (Local, Local) = unimplemented
    final override def onJoin(s1: Local, s2: Local): Local = unimplemented

  private[interpreter] sealed trait HasNotZip extends Interpreter.Unsealed:
    final override def onZip[A, B, C](aa: To[A], bb: To[B], k: (A, B) => C): To[C] = unimplemented
  

  /** Mixin trait for interpreters, that prohibit parallelism. */
  trait Sequential extends Sequential.Default

  object Sequential:
    trait Default extends HasNotZip with HasNotForkJoin with HasNotRestart

    trait Restartable extends HasNotZip with HasNotForkJoin


  /** Mixin trait for interpreters, that allow parallelism. */
  trait Parallel extends Parallel.Default

  object Parallel:
    trait Default extends HasNotForkJoin

    trait Trivial extends HasNotZip with HasNotForkJoin with HasNotRestart

    trait ForkJoin extends Interpreter.Unsealed:
      //@#@TODO
      final override def onJoin(s1: Local, s2: Local): Local = unimplemented
