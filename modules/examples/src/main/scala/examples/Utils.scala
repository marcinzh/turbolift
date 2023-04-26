package examples


object Utils:
  def paragraph(text: String, width: Int): Vector[String] =
    def loop(words: Vector[String], line: String, lines: Vector[String]): Vector[String] =
      words match
        case Vector() => lines :+ line
        case w +: ws =>
          val line2 = if line.isEmpty then w else s"$line $w"
          if line2.size < width
          then loop(ws.toVector, line2, lines)
          else loop(ws.toVector, w, lines :+ line)
        case _ => ???
    loop(text.split("\\s+").filter(_.nonEmpty).toVector, "", Vector())
