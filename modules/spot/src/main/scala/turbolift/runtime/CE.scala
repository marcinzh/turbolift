package turbolift.runtime


/** Import this, to `run` computations on Cats-Effect's executor. */
given CE: RuntimeConfig = RuntimeConfig.fromScala(cats.effect.unsafe.implicits.global.compute)
