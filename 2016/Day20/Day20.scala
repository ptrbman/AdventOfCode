object Day20 {
  val INTMAX = BigInt("4294967295")
  def part12(input : List[String]) = {
    val iPattern = "(\\d+)-(\\d+)".r
    val intervals =
      for (l <- input) yield {
        val iPattern(low, high) = l
        (BigInt(low), BigInt(high))
      }

    val sortedBigIntervals = intervals.sortWith((x1, x2) => x1._1 < x2._1)

    def mergeBigIntervals(intervals : List[(BigInt, BigInt)]) : List[(BigInt, BigInt)] = {
      intervals match {
        case Nil => Nil
        case i :: Nil => List(i)
        case (l1, h1) :: (l2, h2) :: tail => {
          if (h1 + 1 >= l2) {
            mergeBigIntervals((l1, (h1 max h2)) :: tail)
          } else {
            (l1, h1) :: mergeBigIntervals((l2, h2) :: tail)
          }
        }
      }
    }
    val mergedBigIntervals = mergeBigIntervals(sortedBigIntervals)
    val lowestBigInterval = mergedBigIntervals.head._2+1

    println(lowestBigInterval)

    def countAllowed(idx : BigInt, intervals : List[(BigInt, BigInt)]) : BigInt = {
      intervals match {
        case Nil => INTMAX - idx + 1
        case (l, h) :: tail => l - idx + countAllowed(h+1, tail)
      }
    }
    println(countAllowed(0, mergedBigIntervals))
  }

  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part12(input)
  }
}  
