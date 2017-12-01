object Day22 {
  
  def part1(table : List[String]) = {
    val pattern = "/dev/grid/node-x(\\d+)-y(\\d+) *(\\d+)T *(\\d+)T *(\\d+)T *(\\d+)%".r
    val nodes = 
      for (t <- table) yield {
        val pattern(x, y, size, used, avail, use) = t
        println((x, y, size, used, avail, use))
        (x.toInt, y.toInt, size.toInt, used.toInt, avail.toInt, use.toInt)
      }

    val viablePairs = 
      for ((x1, y1, size1, used1, avail1, use1) <- nodes; (x2, y2, size2, used2, avail2, use2) <- nodes;
        if (x1 != x2 || y1 != y2);
        if (used1 > 0);
        if (used1 <= avail2)) yield
        ((x1, y1), (x2, y2))

    println(nodes.length)
    println(viablePairs.length)
  }

  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    val table = input.drop(2)
    part1(table)
  }
}  
