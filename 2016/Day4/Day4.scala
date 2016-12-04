object Day4 {
  def occurrences(c : Char, list : List[Char]) :Int = {
    list match {
      case Nil => 0
      case cc :: tail => (if (c == cc) 1 else 0) + occurrences(c, tail)
    }
  }

  def part1(input : List[String]) = {
    val sum =
      for (i <- input) yield {
        val strings = i.split("-").reverse
        val pattern = "(\\d+)\\[([a-z]+)\\]".r
        val pattern(checksum,common) = strings.head

        val str = strings.tail.map(_.toList).flatten.toList
        val occ = str.map(x => (x, occurrences(x, str))).toSet.toList.sortWith{
          (x, y) => {
            val (x1, x2) = x
            val (y1, y2) = y
            if (x2 == y2) {
              x1 < y1
            } else {
              x2 > y2
            }
          }
        }.take(5).map(_._1).mkString("")

        if (occ == common)
          checksum.toInt
        else
          0
      }
    println(sum.sum)
  }

  def rotate(string : String) = {
    def rt(chars : List[Char]) : List[Char] = {
      chars match {
        case Nil => Nil
        case ' ' :: tail => ' ' :: rt(tail)
        case '-' :: tail => ' ' :: rt(tail)
        case 'z' :: tail => 'a' :: rt(tail)
        case c :: tail => (c.toInt + 1).toChar :: rt(tail)
      }
    }
    rt(string.toList).mkString("")
  }

  def part2(input : List[String]) = {
    for (i <- input) {
      val strings = i.split("-").reverse
      val pattern = "(\\d+)\\[([a-z]+)\\]".r
      val pattern(checksum,common) = strings.head
      val str = strings.tail.reverse.mkString("-")
      var rtstr = str
      for (i <- 0 until checksum.toInt)
        rtstr = rotate(rtstr)
      if (rtstr contains "north")
        println(checksum)
    }
  }

  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input)
    part2(input)    
  }
}  
