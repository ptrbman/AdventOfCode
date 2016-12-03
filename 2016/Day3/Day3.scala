object Day3 {
  def validTriangle(a : Int, b : Int, c : Int) = {
    (a < b + c && b < a + c && c < a + b)
  }

  def part1(input : List[String]) = {
    val count = 
      for (i <- input) yield {
        val pattern = "\\d+".r
        val strings = pattern.findAllIn(i)
        val (a, b, c) = (strings.next, strings.next, strings.next)
        if (validTriangle(a.toInt, b.toInt, c.toInt))
          1
        else
          0
      }
      println(count.sum)
  }

  def part2(input : List[String]) = {
    import scala.collection.mutable.ListBuffer
    var aList, bList, cList = ListBuffer() : ListBuffer[Int]
    for (i <- input) yield {
      val pattern = "\\d+".r
      val strings = pattern.findAllIn(i)
      aList += strings.next.toInt
      bList += strings.next.toInt
      cList += strings.next.toInt
    }
    val iter = (aList ++ bList ++ cList).iterator
    var count = 0
    while (iter.hasNext) {
      val (a, b, c) = (iter.next, iter.next, iter.next)
      if (validTriangle(a, b, c)) 
        count += 1
    }
    println(count)
  }

  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input)
    part2(input)    
  }
}  
