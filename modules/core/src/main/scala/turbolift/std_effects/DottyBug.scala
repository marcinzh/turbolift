package turbolift.std_effects
import turbolift.abstraction.typeclass.Accum


object DottyBug {
  // Copied from `turbolift/std_effects/Except.scala`, as a stopgap.
  // Importing from the original location triggers Dotty compiler exception:
  // "java.lang.AssertionError: assertion failed: duplicate type E#7784; previous was type E#7777"
  // Importing from here works.

  trait Except[E] extends ExceptExt[E, E] {
    def handler = handlers.one
  }

  // This one didn't trigger the exception
  trait ExceptK[F[_], E] extends ExceptExt[F[E], E]

  trait Validation[E] extends ExceptExt[E, E] {
    def handler(implicit E: Accum[E, E]) = handlers.many
  }

  trait ValidationK[F[_], E] extends ExceptExt[F[E], E] {
    def handler(implicit E: Accum[F[E], E]) = handlers.many
  }
}
