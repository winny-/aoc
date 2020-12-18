import scala.io.StdIn

object Day10 extends App {
  def readLines() = {
    def f(): List[String] =
      StdIn.readLine() match {
        case s: String => s::f()
        case null => List()
      }
    f()
  }

  def part1(input: List[Int]): Int = {
    val entireChain = ((input.max + 3)::0::input).sorted
    val differences = entireChain.tail.zip(entireChain).map({ case (a, b) => a - b })

    val d1 = differences.count(_ == 1)
    val d2 = differences.count(_ == 3)

    d1 * d2
  }

  def part2(input: List[Int]): Int = {
    val entireChain = ((input.max + 3)::0::input).sorted
    0
  }

  val input = readLines().map(_.toInt)

  println(part1(input))
  println(part2(input))
}

// Local Variables:
// compile-command: "scala day10.scala < input.txt"
// End:
