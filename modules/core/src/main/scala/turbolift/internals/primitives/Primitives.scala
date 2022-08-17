package turbolift.internals.primitives
import turbolift.!!
import turbolift.internals.primitives.{ComputationCases => CC}
import turbolift.internals.engine.Config


private[turbolift] object Primitives:
  inline def pure[A](a: A): A !! Any = new CC.Pure(a)
  inline def impure[A](f: () => A): A !! Any = new CC.Impure(f)
  inline def flatMap[A, B, U](thiz: A !! U, f: A => B !! U): B !! U = new CC.Map(Tags.MapFlat, thiz.untyped, f.asInstanceOf[Any => Any])
  inline def map[A, B, U](thiz: A !! U, f: A => B): B !! U = new CC.Map(Tags.MapPure, thiz.untyped, f.asInstanceOf[Any => Any])
  inline def zipPar[A, B, U](thiz: A !! U, that: B !! U): (A, B) !! U = zipWithPar(thiz, that, pairCtorFun)
  inline def zipWithPar[A, B, C, U](thiz: A !! U, that: B !! U, f: (A, B) => C): C !! U = new CC.ZipWithPar(thiz, that, f)

  inline def configAsk[A](f: Config => A): A !! Any = CC.ConfigAsk(f)
  inline def configMod[A, U](f: Config => Config, body: A !! U): A !! U = CC.ConfigMod(f, body)

  private def pairCtorFun[A, B]: (A, B) => (A, B) = pairCtorVal.asInstanceOf[(A, B) => (A, B)]
  private val pairCtorVal: (Any, Any) => Any = (_, _) 
  