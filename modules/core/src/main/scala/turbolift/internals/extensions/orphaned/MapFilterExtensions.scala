package turbolift.internals.extensions.orphaned
import turbolift.!!
import scala.collection.BuildFrom



private[turbolift] trait AuxExtensions:
  private[orphaned] def empty0[A, U]: Vector[A] !! U = !!.pure(Vector.empty[A]).upCast[U]
  private[orphaned] def unit0[U]: Unit !! U = !!.unit.upCast[U]
  
  private[orphaned] val funAddVoidUntyped: (Unit, Any) => Unit = (_, _) => ()
  private[orphaned] val funAddOneUntyped: (Vector[Any], Any) => Vector[Any] = (as, a) => as :+ a
  private[orphaned] val funAddManyUntyped: (Vector[Any], IterableOnce[Any]) => Vector[Any] = (as1, as2) => as1 ++ as2
  private[orphaned] def funAddVoid[A] = funAddVoidUntyped.asInstanceOf[(Unit, A) => Unit]
  private[orphaned] def funAddOne[A] = funAddOneUntyped.asInstanceOf[(Vector[A], A) => Vector[A]]
  private[orphaned] def funAddMany[A] = funAddManyUntyped.asInstanceOf[(Vector[A], IterableOnce[A]) => Vector[A]]
  private[orphaned] val fallbackVal: Any !! Any = !!.pure(null)
  private[orphaned] val fallbackFun: Any => Any !! Any = _ => fallbackVal

  private[orphaned] type BF[A, B, S[_]] = BuildFrom[S[A], B, S[B]]
  private[orphaned] def doBuildFromVector[A, B, S[X] <: Iterable[X], U](as: S[A], mbs: Vector[B] !! U)(using bf: BF[A, B, S]): S[B] !! U = 
    mbs.map(bs => (bf.newBuilder(as) ++= bs).result())


private[turbolift] trait MapFilterExtensions extends AuxExtensions:
  extension [A](thiz: Iterator[A])
    /** Like `map` from the standard library, but using effectful function. */
    def map_!![B, U](f: A => B !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWith(f(a))(funAddOne))

    /** Like [[map_!!]], but executed parallelly for each element. */
    def mapPar_!![B, U](f: A => B !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWithPar(f(a))(funAddOne))

    /** Like `flatMap` from the standard library, but using effectful function. */
    def flatMap_!![B, U](f: A => IterableOnce[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWith(f(a))(funAddMany))

    /** Like [[flatMap_!!]], but executed parallelly for each element. */
    def flatMapPar_!![B, U](f: A => IterableOnce[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWithPar(f(a))(funAddMany))

    /** Like `filter` from the standard library, but using effectful function. */
    def filter_!![U](f: A => Boolean !! U): Vector[A] !! U =
      thiz.flatMap_!!(a => f(a).map(if _ then Some(a) else None))

    /** Like [[filter_!!]], but executed parallelly for each element. */
    def filterPar_!![U](f: A => Boolean !! U): Vector[A] !! U =
      thiz.flatMapPar_!!(a => f(a).map(if _ then Some(a) else None))

    /** Like `flatMap` from the standard library, but specialized for `Option`. */
    def mapFilter[B, U](f: A => Option[B]): Iterator[B] = thiz.flatMap(f)

    /** Like [[mapFilter]], but using effectful function. */
    def mapFilter_!![B, U](f: A => Option[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]) { (mbs, a) =>
        mbs.zipWith(f(a))((bs, ob) => ob match
          case Some(b) => bs :+ b
          case _ => bs
        )
      }

    /** Like [[mapFilter_!!]], but executed parallelly for each element. */
    def mapFilterPar_!![B, U](f: A => Option[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]) { (mbs, a) =>
        mbs.zipWithPar(f(a))((bs, ob) => ob match
          case Some(b) => bs :+ b
          case _ => bs
        )
      }

    /** Like `collect` from the standard library, but using effectful function. */
    def collect_!![B, U](f: PartialFunction[A, B !! U]): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]) { (mbs, a) =>
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[B !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs.zipWith(mb)(funAddOne)
      }

    /** Like [[collect_!!]], but executed parallelly for each element. */
    def collectPar_!![B, U](f: PartialFunction[A, B !! U]): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]) { (mbs, a) =>
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[B !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs.zipWithPar(mb)(funAddOne)
      }


  extension [A](thiz: IterableOnce[A])
    /** Like `foreach` from the standard library, but using effectful function. */
    def foreach_!![U](f: A => Unit !! U): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]) { (mbs, a) => mbs &&! f(a) }

    /** Like [[foreach_!!]], but executed parallelly for each element. */
    def foreachPar_!![U](f: A => Unit !! U): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]) { (mbs, a) => mbs &! f(a) }

    /** Like `foreach` from the standard library, but executed only for elements, where the partial function is defined. */
    def forsome[U](f: PartialFunction[A, Unit]): Unit =
      thiz.iterator.foreach(a => f.applyOrElse(a, _ => ()))

    /** Like [[forsome]] , but using effectful function. */
    def forsome_!![U](f: PartialFunction[A, Unit !! U]): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]) { (mbs, a) => 
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[Unit !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs &&! mb
      }

    /** Like [[forsome_!!]], but executed parallelly for each element. */
    def forsomePar_!![U](f: PartialFunction[A, Unit !! U]): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]) { (mbs, a) => 
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[Unit !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs &! mb
      }


  extension [A, S[X] <: Iterable[X]](thiz: S[A])
    // :-(
    // type BF[X] = BuildFrom[S[A], X, S[X]]

    /** Like `map` from the standard library, but using effectful function. */
    def map_!![B, U](f: A => B !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.map_!!(f))

    /** Like [[map_!!]], but executed parallelly for each element. */
    def mapPar_!![B, U](f: A => B !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.mapPar_!!(f))

    /** Like `flat` from the standard library, but using effectful function. */
    def flatMap_!![B, U](f: A => IterableOnce[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.flatMap_!!(f))

    /** Like [[flatMap_!!]], but executed parallelly for each element. */
    def flatMapPar_!![B, U](f: A => IterableOnce[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.flatMapPar_!!(f))

    /** Like `filter` from the standard library, but using effectful function. */
    def filter_!![U](f: A => Boolean !! U)(using BF[A, A, S]): S[A] !! U =
      doBuildFromVector(thiz, thiz.iterator.filter_!!(f))

    /** Like [[filter_!!]], but executed parallelly for each element. */
    def filterPar_!![U](f: A => Boolean !! U)(using BF[A, A, S]): S[A] !! U =
      doBuildFromVector(thiz, thiz.iterator.filterPar_!!(f))

    /** Like `map` from the standard library, but using effectful function. */
    def mapFilter_!![B, U](f: A => Option[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.mapFilter_!!(f))

    /** Like [[mapFilter_!!]], but executed parallelly for each element. */
    def mapFilterPar_!![B, U](f: A => Option[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.mapFilterPar_!!(f))

    /** Like `collect` from the standard library, but using effectful function. */
    def collect_!![B, U](f: PartialFunction[A, B !! U])(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.collect_!!(f))

    /** Like [[collect_!!]], but executed parallelly for each element. */
    def collectPar_!![B, U](f: PartialFunction[A, B !! U])(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.collectPar_!!(f))
