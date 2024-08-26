package turbolift.misc
import org.specs2.mutable._
import turbolift.typeclass._
import turbolift.typeclass.Syntax._


class AccumTest extends Specification:

  enum T:
    case A
    case B
  import T.{A, B}

  def some[T](x: T): Option[T] = Some(x)
  def none[T]: Option[T] = None

  "Plus & Accum syntax" >> {
    (() |+| ()) === ()
    (1 |+| 2) === 3
    (1 |+ 2) === 3
    ("foo" |+| "bar") === "foobar"
    ("foo" |+ 'b') === "foob"
    (none[Int] |+| some(1)) === some(1)
    (some(1) |+| none) === some(1)
    (some(1) |+| some(2)) === some(3)
    (List(A, B) |+| List(B, A)) === List(A, B, B, A)
    (List(A, B) |+ B |+ A) === List(A, B, B, A)
    (Map(1 -> "foo") |+| Map(2 -> "bar")) === Map(1 -> "foo", 2 -> "bar")
    (Map(1 -> "foo") |+| Map(1 -> "bar")) === Map(1 -> "foobar")
    (Map(1 -> "foo") |+ ((2 -> 'b'))) === Map(1 -> "foo", 2 -> "b")
    (Map(1 -> "foo") |+ ((1 -> 'b'))) === Map(1 -> "foob")
    (Map(1 -> List(10, 20)) |+| Map(1 -> List(30, 40))) === Map(1 -> List(10, 20, 30, 40))
    (Map(1 -> List(10, 20)) |+| Map(2 -> List(30, 40))) === Map(1 -> List(10, 20), 2 -> List(30, 40))
    (Map(1 -> List(10, 20)) |+ ((1 -> 30))) === Map(1 -> List(10, 20, 30))
    (Map(1 -> List(10, 20)) |+ ((2 -> 30))) === Map(1 -> List(10, 20), 2 -> List(30))
    List(
      Map(1 -> Map(A -> "a", B -> "b")),
      Map(1 -> Map(A -> "c"), 2 -> Map(A -> "d")),
      Map(1 -> Map(B -> "e"), 2 -> Map(B -> "f")),
      Map(),
      Map(1 -> Map(), 2 -> Map()),
      Map(1 -> Map(A -> "g", B -> "h"), 2 -> Map(A -> "i", B -> "j")),
    ).reduce(_ |+| _) === Map(1 -> Map(A -> "acg", B -> "beh"), 2 -> Map(A -> "di", B -> "fj"))
  }

