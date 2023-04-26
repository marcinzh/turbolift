package examples


trait Example extends Product with Function0[Unit]:
  def description: String
  final def name = productPrefix


object Main:
  def main(args: Array[String]): Unit =
    args.headOption.flatMap(_.toIntOption).filter(cases.isDefinedAt(_)) match
      case None => describe()
      case Some(x) => cases(x.toInt)()

  val cases = List(
    HandlerShadowing,
    ReentrantRun,
  )

  def describe() =
    println("Select example to run, by providing its number as the first argument.")
    println("Available examples are:\n")
    val w1 = cases.size.toString.size
    val w2 = cases.map(_.name.size).max
    for (o, i) <- cases.zipWithIndex do
      println(s"${Console.BOLD}$i. ${o.name}:${Console.RESET}")
      for l <- Utils.paragraph(o.description, 80) do
        println(l)
      println
      // println(s" ${i.toString.padTo(w1, ' ')} | ${o.name.padTo(w2, ' ')} | ${lines(0)}")
    println
