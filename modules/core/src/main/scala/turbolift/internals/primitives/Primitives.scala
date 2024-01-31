package turbolift.internals.primitives
import turbolift.!!
import turbolift.io.Snap
import turbolift.internals.primitives.{ComputationCases => CC}
import turbolift.internals.engine.Env


private[turbolift] object Primitives:
  inline def pure[A](a: A): A !! Any = new CC.Pure(a)
  inline def impure[A](f: () => A): A !! Any = new CC.Impure(f)
  inline def flatMap[A, B, U](thiz: A !! U, f: A => B !! U): B !! U = new CC.Map(Tags.MapFlat, thiz.untyped, f.asInstanceOf[Any => Any])
  inline def map[A, B, U](thiz: A !! U, f: A => B): B !! U = new CC.Map(Tags.MapPure, thiz.untyped, f.asInstanceOf[Any => Any])
  inline def zipPar[A, B, U](thiz: A !! U, that: B !! U): (A, B) !! U = zipWithPar(thiz, that, pairCtorFun)
  inline def zipWithPar[A, B, C, U](thiz: A !! U, that: B !! U, f: (A, B) => C): C !! U = new CC.ZipPar(thiz, that, f)
  inline def zipWithSeq[A, B, C, U](thiz: A !! U, that: () => B !! U, f: (A, B) => C): C !! U = new CC.ZipSeq(thiz, that, f)
  inline def orPar[A, U](thiz: A !! U, that: A !! U): A !! U = new CC.OrPar(thiz, that)
  inline def orSeq[A, U](thiz: A !! U, that: () => A !! U): A !! U = new CC.OrSeq(thiz, that)

  inline def envAsk[A](f: Env => A): A !! Any = CC.EnvAsk(f)
  inline def envMod[A, U](f: Env => Env, body: A !! U): A !! U = CC.EnvMod(f, body)
  
  inline def snap[A, B, U](body: A !! U)(fun: Snap[A] => B !! U): B !! U = CC.DoSnap(body, fun)
  inline def unsnap[A](snap: Snap[A]): A !! Any = CC.Unsnap(snap)
  inline def yeld: Unit !! Any = CC.Yield

  private def pairCtorFun[A, B]: (A, B) => (A, B) = pairCtorVal.asInstanceOf[(A, B) => (A, B)]
  private val pairCtorVal: (Any, Any) => Any = (_, _) 
