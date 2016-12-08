object Day5 {
  import scala.collection.mutable.Map


  class Rectangle(w : Int, h : Int) {
    // Indexing by row first
    val rr = Array.ofDim[Int](h, w)

    def rect(x : Int, y : Int) = {
      for (i <- 0 until y; j <- 0 until x)
        rr(i)(j) = 1
    }

    def col(c : Int, s : Int) = {
      val oldCol = for (y <- 0 until h) yield rr(y)(c)
      for (y <- 0 until h) {
        rr(y)(c) = oldCol(if (y-s < 0) y-s+h else y-s)
      }
    }

    def row(r : Int, s : Int) = {
      val oldRow = for (x <- 0 until w) yield rr(r)(x)
      for (x <- 0 until w) {
        rr(r)(x) = oldRow(if (x-s < 0) x-s+w else x-s)
      }
    }    

    def countPixels = {
      (for (i <- 0 until h) yield {
        (for (j <- 0 until w) yield
          rr(i)(j)).sum
      }).sum
    }

    // Incomplete since I do not know what the alphabet looks like
    def getLetter(x : Int) = {
      if (rr(0)(x+1) == 1 &&
        rr(0)(x+2) == 1 &&
        rr(3)(x+1) == 1 &&
        rr(3)(x+1) == 1 &&
        rr(5)(x+3) == 0)
        "P"                  
      else if (rr(0)(x+1) == 1 &&
        rr(0)(x+2) == 1 &&
        rr(3)(x+1) == 1 &&
        rr(3)(x+1) == 1 &&
        rr(4)(x+2) == 1)
        "R"            
      else if (rr(0)(x+1) == 1 &&
        rr(0)(x+2) == 1 &&
        rr(3)(x+1) == 1 &&
        rr(3)(x+1) == 1)
        "A"      
      else if (rr(0)(x) == 1 &&
        rr(0)(x+1) == 1 &&
        rr(0)(x+2) == 1 &&
        rr(0)(x+3) == 1)
        "E"
      else if (rr(0)(x+1) == 1 &&
        rr(0)(x+2) == 1 &&
        rr(1)(x) == 1 &&
        rr(1)(x+3) == 1 &&
        rr(3)(x+2) == 1)
        "G"
      else if (rr(0)(x+1) == 1 &&
        rr(0)(x+2) == 1 &&
        rr(1)(x) == 1 &&
        rr(1)(x+3) == 1)
        "O"
      else if (rr(2)(x+1) == 1 &&
        rr(2)(x+2) == 1)
        "H"
      else if (rr(2)(x+1) == 1 &&
        rr(2)(x+3) == 1)
        "Y"      
      else
        "!"
    }

    def letters = {
      (for (i <- 0 until w / 5) yield {
        getLetter(i*5)
      }).mkString("")
    }

    override def toString = {
      (for (i <- 0 until h) yield {
        (for (j <- 0 until w) yield
          (if (rr(i)(j) == 1) "#" else ".")).mkString(" ")
      }).mkString("\n")
    }
  }


  def part1(input : List[String]) = {
    var r = new Rectangle(50, 6)

    val rect = "rect (\\d+)x(\\d+)".r
    val column = "rotate column x=(\\d+) by (\\d+)".r
    val row = "rotate row y=(\\d+) by (\\d+)".r

    for (inst <- input) {
      inst match {
        case rect(x, y) => r.rect(x.toInt, y.toInt)
        case column(x, s) => r.col(x.toInt, s.toInt)
        case row(y, s) => r.row(y.toInt, s.toInt)
      }
    }
    println(r.countPixels)
  }

  def part2(input : List[String]) = {
    var r = new Rectangle(50, 6)

    val rect = "rect (\\d+)x(\\d+)".r
    val column = "rotate column x=(\\d+) by (\\d+)".r
    val row = "rotate row y=(\\d+) by (\\d+)".r

    for (inst <- input) {
      inst match {
        case rect(x, y) => r.rect(x.toInt, y.toInt)
        case column(x, s) => r.col(x.toInt, s.toInt)
        case row(y, s) => r.row(y.toInt, s.toInt)
      }
    }
    println(r)
    println(r.letters)    
  }  


  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input)
    part2(input)    
  }
}  
