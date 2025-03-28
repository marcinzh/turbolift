package devel


object Main:
  def main(args: Array[String]): Unit =
    args.headOption.getOrElse("") match
      case "" | "jolo" => Jolo.run
      case "thing" => stuff.thing.Thing.run(args.tail)
