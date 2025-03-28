package stuff.thing
import turbolift.!!
import turbolift.io._
import turbolift.effects._
import turbolift.Extensions._
import stuff.Auxx._


object Thing:
  def run(args: Array[String]) =
    println:
      run1

  def run1 =
    !!.pure("hell world")
    .run

