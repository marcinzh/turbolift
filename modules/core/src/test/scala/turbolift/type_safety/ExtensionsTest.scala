package turbolift.type_safety
import org.specs2.mutable._
import turbolift.!!
import turbolift.Extensions._


class ExtensionsTest extends Specification:
  val l = List("a", "b", "c")

  "fold" >>{
    l.foldPlus === l.fold("")(_ ++ _)
  }

  "reduce" >>{
    l.reducePlus === l.reduce(_ ++ _)
  }
