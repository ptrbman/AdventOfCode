object Day19 {
  def part1(count : Int) = {
    def doMove(elves : Int, mult : Int, min : Int, max : Int) : Int = {
      if (elves == 1) {
        if (min != max)
          throw new Exception("min != max?")
        else
          min
      } else if (elves % 2 == 0) {
        doMove(elves/2, mult*2, min, max - mult/2)        
      } else {
        doMove((elves-1)/2, mult*2, min + mult, max)
      }
    }
    println(doMove(count, 2, 1, count))
  }

  def part2(count : Int) = {
    // def doMove(elves : Int) : Int = {
    //   if (elves == 1) {
    //     if (min != max)
    //       throw new Exception("min != max?")
    //     else
    //       min
    //   } else if (elves % 2 == 0) {
    //     doMove(elves/2, mult*2, min, max - mult/2)        
    //   } else {
    //     doMove((elves-1)/2, mult*2, min + mult, max)
    //   }
    // }
    // println(doMove(count, 2, 1, count))
    import scala.collection.mutable.ListBuffer
    println("init")
    val elves =
      (for (i <- 1 to count) yield i).to[ListBuffer]
    while (elves.length > 1) {
      var idx = 0
      while (idx < elves.length) {
      println(elves.length)        
        val remove = 
        if (elves.length % 2 == 0) {
          (idx + (elves.length/2)) % elves.length
        } else {
          (idx + ((elves.length-1)/2)) % elves.length
        }
        elves.remove(remove)
        if (idx < remove)
          idx += 1
      }
    }
    println(elves)

  }  


  def main (args : Array[String]) = {
    // part1(3014387)
    part2(3014387)
  }
}  
