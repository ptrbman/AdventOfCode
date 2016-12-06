object Day5 {
  import scala.collection.mutable.Map

  def part1(input : List[String]) = {
    val map = Map() : Map[(Int, Char), Int]
    val len = input.head.length
    for (str <- input) {
      for (i <- 0 until len) {
        val idx = (i, str(i))
        map += idx -> ((map.getOrElse(idx, 0)) + 1)
      }
    }

    val str = 
      (for (i <- 0 until len) yield {
        val fmap = map.filter(_._1._1 == i)
        val max = fmap.keys.toList.sortWith((x, y) => fmap(x) > fmap(y)).head
        max._2
      }).mkString("")
    println(str)
  }


  def part2(input : List[String]) = {
    val map = Map() : Map[(Int, Char), Int]
    val len = input.head.length
    for (str <- input) {
      for (i <- 0 until len) {
        val idx = (i, str(i))
        map += idx -> ((map.getOrElse(idx, 0)) + 1)
      }
    }

    val str = 
      (for (i <- 0 until len) yield {
        val fmap = map.filter(_._1._1 == i)
        val max = fmap.keys.toList.sortWith((x, y) => fmap(x) < fmap(y)).head
        max._2
      }).mkString("")
    println(str)    
  }


  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input)
    part2(input)    
  }
}  
