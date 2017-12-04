object Day1 {
  def part1(input : String) = {
    def sum(list : List[Int]) : Int = {
      list match {
        case List(i) => 0
        case i :: j :: tail if (i == j) => i + sum(j :: tail)
        case i :: tail => sum(tail)
      }
    }

    val intList = input.map(_.toInt - 48).toList
    sum(intList ++ List(intList.head))
  }

  def part2(input : String) = {
    val length = input.length
    val intList = input.map(_.toInt - 48).toList    
    val half = length/2
    val list = intList ++ intList
    (for (i <- 0 until length if (list(i) == list(i+half))) yield list(i)).sum
  }


  def main(args : Array[String]) = {
    val input = io.Source.fromFile("Day1.txt").getLines.toList
    println(part1(input.head))
    println(part2(input.head))
  }
}
