package turbolift.internals.extensions
import turbolift.!!
import scala.collection.BuildFrom


private[turbolift] trait AuxExtensions:
  private[extensions] def empty0[A, U]: Vector[A] !! U = !!.pure(Vector.empty[A]).upCast[U]
  private[extensions] def unit0[U]: Unit !! U = !!.unit.upCast[U]
  
  private[extensions] val funAddVoidUntyped: (Unit, Any) => Unit = (_, _) => ()
  private[extensions] val funAddOneUntyped: (Vector[Any], Any) => Vector[Any] = (as, a) => as :+ a
  private[extensions] val funAddManyUntyped: (Vector[Any], IterableOnce[Any]) => Vector[Any] = (as1, as2) => as1 ++ as2
  private[extensions] def funAddVoid[A] = funAddVoidUntyped.asInstanceOf[(Unit, A) => Unit]
  private[extensions] def funAddOne[A] = funAddOneUntyped.asInstanceOf[(Vector[A], A) => Vector[A]]
  private[extensions] def funAddMany[A] = funAddManyUntyped.asInstanceOf[(Vector[A], IterableOnce[A]) => Vector[A]]
  private[extensions] val fallbackVal: Any !! Any = !!.pure(null)
  private[extensions] val fallbackFun: Any => Any !! Any = _ => fallbackVal

  private[extensions] type BF[A, B, S[_]] = BuildFrom[S[A], B, S[B]]
  private[extensions] def doBuildFromVector[A, B, S[X] <: Iterable[X], U](as: S[A], mbs: Vector[B] !! U)(using bf: BF[A, B, S]): S[B] !! U = 
    mbs.map(bs => (bf.newBuilder(as) ++= bs).result())


/** No need to use this trait directly, because it's inherited by [[turbolift.Extensions Extensions]] object. */
private[turbolift] trait MapFilterExtensions extends AuxExtensions:
  extension [A](thiz: Iterator[A])
    /** Like `map` from the standard library, but using effectful function. */
    def mapEff[B, U](f: A => B !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWith(f(a))(funAddOne))

    /** Like [[mapEff]], but executed parallelly for each element. */
    def mapParEff[B, U](f: A => B !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWithPar(f(a))(funAddOne))

    /** Like `flatMap` from the standard library, but using effectful function. */
    def flatMapEff[B, U](f: A => IterableOnce[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWith(f(a))(funAddMany))

    /** Like [[flatMapEff]], but executed parallelly for each element. */
    def flatMapParEff[B, U](f: A => IterableOnce[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U])((mbs, a) => mbs.zipWithPar(f(a))(funAddMany))

    /** Like `filter` from the standard library, but using effectful function. */
    def filterEff[U](f: A => Boolean !! U): Vector[A] !! U =
      thiz.flatMapEff(a => f(a).map(if _ then Some(a) else None))

    /** Like [[filterEff]], but executed parallelly for each element. */
    def filterParEff[U](f: A => Boolean !! U): Vector[A] !! U =
      thiz.flatMapParEff(a => f(a).map(if _ then Some(a) else None))

    /** Like `flatMap` from the standard library, but specialized for `Option`. */
    def mapFilter[B, U](f: A => Option[B]): Iterator[B] = thiz.flatMap(f)

    /** Like [[mapFilter]], but using effectful function. */
    def mapFilterEff[B, U](f: A => Option[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]): (mbs, a) =>
        mbs.zipWith(f(a)): (bs, ob) =>
          ob match
            case Some(b) => bs :+ b
            case _ => bs

    /** Like [[mapFilterEff]], but executed parallelly for each element. */
    def mapFilterParEff[B, U](f: A => Option[B] !! U): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]): (mbs, a) =>
        mbs.zipWithPar(f(a)): (bs, ob) =>
          ob match
            case Some(b) => bs :+ b
            case _ => bs

    /** Like `collect` from the standard library, but using effectful function. */
    def collectEff[B, U](f: PartialFunction[A, B !! U]): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]): (mbs, a) =>
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[B !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs.zipWith(mb)(funAddOne)

    /** Like [[collectEff]], but executed parallelly for each element. */
    def collectParEff[B, U](f: PartialFunction[A, B !! U]): Vector[B] !! U =
      thiz.foldLeft(empty0[B, U]): (mbs, a) =>
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[B !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs.zipWithPar(mb)(funAddOne)


  extension [A](thiz: IterableOnce[A])
    /** Like `foreach` from the standard library, but using effectful function. */
    def foreachEff[U](f: A => Unit !! U): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]) { (mbs, a) => mbs &&! f(a) }

    /** Like [[foreachEff]], but executed parallelly for each element. */
    def foreachParEff[U](f: A => Unit !! U): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]) { (mbs, a) => mbs &! f(a) }

    /** Like `foreach` from the standard library, but executed only for elements, where the partial function is defined. */
    def forsome[U](f: PartialFunction[A, Unit]): Unit =
      thiz.iterator.foreach(a => f.applyOrElse(a, _ => ()))

    /** Like [[forsome]] , but using effectful function. */
    def forsomeEff[U](f: PartialFunction[A, Unit !! U]): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]): (mbs, a) => 
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[Unit !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs &&! mb

    /** Like [[forsomeEff]], but executed parallelly for each element. */
    def forsomeParEff[U](f: PartialFunction[A, Unit !! U]): Unit !! U =
      thiz.iterator.foldLeft(unit0[U]): (mbs, a) => 
        val mb = f.applyOrElse(a, fallbackFun).asInstanceOf[Unit !! U]
        if mb eq fallbackVal then
          mbs
        else
          mbs &! mb


  extension [A, S[X] <: Iterable[X]](thiz: S[A])
    // :-(
    // type BF[X] = BuildFrom[S[A], X, S[X]]

    /** Like `map` from the standard library, but using effectful function. */
    def mapEff[B, U](f: A => B !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.mapEff(f))

    /** Like [[mapEff]], but executed parallelly for each element. */
    def mapParEff[B, U](f: A => B !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.mapParEff(f))

    /** Like `flat` from the standard library, but using effectful function. */
    def flatMapEff[B, U](f: A => IterableOnce[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.flatMapEff(f))

    /** Like [[flatMapEff]], but executed parallelly for each element. */
    def flatMapParEff[B, U](f: A => IterableOnce[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.flatMapParEff(f))

    /** Like `filter` from the standard library, but using effectful function. */
    def filterEff[U](f: A => Boolean !! U)(using BF[A, A, S]): S[A] !! U =
      doBuildFromVector(thiz, thiz.iterator.filterEff(f))

    /** Like [[filterEff]], but executed parallelly for each element. */
    def filterParEff[U](f: A => Boolean !! U)(using BF[A, A, S]): S[A] !! U =
      doBuildFromVector(thiz, thiz.iterator.filterParEff(f))

    /** Like `map` from the standard library, but using effectful function. */
    def mapFilterEff[B, U](f: A => Option[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.mapFilterEff(f))

    /** Like [[mapFilterEff]], but executed parallelly for each element. */
    def mapFilterParEff[B, U](f: A => Option[B] !! U)(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.mapFilterParEff(f))

    /** Like `collect` from the standard library, but using effectful function. */
    def collectEff[B, U](f: PartialFunction[A, B !! U])(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.collectEff(f))

    /** Like [[collectEff]], but executed parallelly for each element. */
    def collectParEff[B, U](f: PartialFunction[A, B !! U])(using BF[A, B, S]): S[B] !! U =
      doBuildFromVector(thiz, thiz.iterator.collectParEff(f))
