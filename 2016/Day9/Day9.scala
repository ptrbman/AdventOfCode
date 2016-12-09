object Day5 {
  def part1(input : String) = {
    val pattern = "([A-Z]*)\\((\\d+)x(\\d+)\\)(.*)".r
    var s = input
    var count = 0
    while (!s.isEmpty) {
      s match {
        case pattern(head, d1, d2, tail) => {
          val mult = d1.toInt * d2.toInt
          count += head.length + mult
          s = tail.drop(d1.toInt)
        }
        case nomatch => {
          count += nomatch.length
          s = ""
        }
      }
    }
    println(count)
  }


  def part2(input : String) = {
    // Scan from left to right
    val pattern = "([A-Z]*)\\)(\\d+)x(\\d+)\\((.*)".r
    var s = input.reverse
    var ss = List() : List[BigInt]
    while (!s.isEmpty) {
      s match {
        case pattern(head, dd1, dd2, tail) => {
          val d1 = dd1.reverse.toInt
          val d2 = dd2.reverse.toInt
          ss = head.map(x => BigInt(1)).toList ++ ss
          val c = ss.take(d2).sum*d1
          ss = List.fill(d2)(BigInt(0)) ++ ss.drop(d2)
          ss = c :: List.fill(1 + dd1.length + 1 + dd2.length)(BigInt(0)) ++ ss
          s = tail
        }
        case nomatch => {
          ss = List.fill(nomatch.length)(BigInt(1)) ++ ss
          s = ""
        }
      }
    }

    println(ss.sum)
  }


  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input.head)
    part2(input.head)
  }
}  
