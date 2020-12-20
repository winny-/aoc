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

  def part1(input: List[Long]): Long = {
    val entireChain = ((input.max + 3L)::0L::input).sorted
    val differences = entireChain.tail.zip(entireChain).map({ case (a, b) => a - b })

    val d1 = differences.count(_ == 1)
    val d2 = differences.count(_ == 3)

    d1 * d2
  }

  def part2(input: List[Long]): Long = {
    val entireChain = ((input.max + 3L)::0L::input).sorted

    def edges(plug: Long, xs: List[Long]): Long =
      xs match {
        case y::ys if (y <= plug + 3) => 1 + edges(plug, ys)
        case _ => 0
      }

    val goal = entireChain.last
    // printf("%s -> %s\n", entireChain, entireChain.last)

    def paths(xs: List[Long], traversed: List[Long]): Long = {
      // printf("-> %s\n", xs)
      xs match {
        case y::ys if y == goal => {
          // val init = traversed.last
          // val middle = traversed.dropRight(1)
          // printf("(%s), %s, (%s)\n", init, middle.reverse.map(_.toString).mkString(", "), y)
          1
        }
        case y::ys => {
          def possible(zs: List[Long], offset: Int): Option[List[Long]] = {
            if (zs.length > offset) {
              val v = zs(offset)
              if (v <= y+3) {
                Some(v::zs.drop(offset+1))
              } else {
                None
              }
            } else {
              None
            }
          }
          List(0,1,2).map(possible(ys, _)).filter(_.nonEmpty).map(o => paths(o.get, y::traversed)).sum

          // printf("ooo %s\n", List(1,2,3).map(p => ys.span(z => z == y + p)))
          // val (next, rest) = ys.span(z => z <= y+3)
          // printf("%s %s\n", next, rest)
          // next.map(z => paths(z::rest, y::traversed)).sum
        }
      }
    }


    paths(entireChain, Nil)
    // entireChain.tails.map({
    //   case x::xs => edges(x, xs)
    //   case _ => 0
    // }).sum
  }

  val input = readLines().map(_.toLong)

  println(part1(input))
  println(part2(input))
}

// Local Variables:
// compile-command: "scala day10.scala < input.txt"
// End:
