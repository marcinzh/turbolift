package turbolift.extra_effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.WriterK
import turbolift.effects.Generator
import turbolift.mode.ST


class GeneratorTest extends Specification:
  val finiteNums =
    Generator.producer[Long, Any]: fx =>
      fx.yeld(1) &&!
      fx.yeld(2) &&!
      fx.yeld(3)

  val infiniteNums =
    Generator.producer[Long, Any]: fx =>
      def loop: Unit !! fx.type =
        fx.yeld(42) &&! loop
      loop

  def evenFibosForever =
    Generator.producer[Long, Any]: fx =>
      def loop(a: Long, b: Long): Unit !! fx.type =
        !!.when(b % 2 == 0)(fx.yeld(b)) &&!
        loop(b, a + b)
      loop(0, 1)

  def evenFibosUntil(limit: Long) =
    Generator.producer[Long, Any]: fx =>
      def loop(a: Long, b: Long): Unit !! fx.type =
        !!.when(b % 2 == 0)(fx.yeld(b)) &&!
        !!.when(b <= limit)(loop(b, a + b))
      loop(0, 1)

  def forEachDo[A, U](action: A => Unit !! U): Generator[A, U] => Unit !! U =
    Generator.consumer[A, U]: fx =>
      def loop: Unit !! (U & fx.type) =
        for
          a <- fx.await
          _ <- action(a)
          _ <- loop
        yield ()
      loop

  def takeSomeAndDo[A, U](count: Long)(action: A => Unit !! U): Generator[A, U] => Unit !! U =
    Generator.consumer[A, U]: fx =>
      def loop(count: Long): Unit !! (U & fx.type) =
        !!.when(count > 0):
          for
            a <- fx.await
            _ <- action(a)
            _ <- loop(count - 1)
          yield ()
      loop(count)

  "Basic ops" >> {
    "finite nums" >>{
      finiteNums.toVector
      .run === Vector(1, 2, 3)
    }

    "infinite nums" >>{
      infiniteNums.take(3).toVector
      .run === Vector(42, 42, 42)
    }

    "finite even fibos" >>{
      evenFibosUntil(1000).toVector
      .run === Vector(2, 8, 34, 144, 610)
    }

    "infinite even fibos" >>{
      evenFibosForever.take(5).toVector
      .run === Vector(2, 8, 34, 144, 610)
    }
  }


  "Combined ops" >> {
    "finite producer & infinite consumer" >>{
      case object W extends WriterK[Vector, Long]
      forEachDo(W.tell(_))(finiteNums)
      .handleWith(W.handler.justState)
      .run === Vector(1, 2, 3)
    }

    "infinite producer & finite consumer" >>{
      case object W extends WriterK[Vector, Long]
      takeSomeAndDo(3)(W.tell(_))(infiniteNums)
      .handleWith(W.handler.justState)
      .run === Vector(42, 42, 42)
    }
  }
