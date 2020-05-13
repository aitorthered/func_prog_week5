import scala.io.Source

object x {
  val in = Source.fromURL("https://lamp.epfl.ch/wp-content/uploads/2019/01/linuxwords.txt")

  val words = in.getLines

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI",
    '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS",
    '8' -> "TUV", '9' -> "WXYZ"
  )

  val charCode: Map[Char, Char] = (for {
    (number, strings) <- mnem
    letter <- strings
  } yield letter -> number)

  def wordCode(word: String): String =
    word.toUpperCase map charCode

  wordCode("TRES")
  wordCode("JAVA")
  wordCode("java")
}