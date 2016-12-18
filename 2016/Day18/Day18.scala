object Day18 {
  def part1(input : String, count : Int) = {
    def gc(str : String, i : Int) = {
      val ret =
        if (i < 0 || i >= str.length) '.'
        else str(i).toChar
      ret
    }

    def genRow(str : String) = {
      val ret = 
        (for (i <- 0 until str.length) yield {
        (gc(str, i-1), gc(str, i), gc(str, i+1)) match {
          case ('^', '^', '.') => '^'
          case ('.', '^', '^') => '^'
          case ('^', '.', '.') => '^'
          case ('.', '.', '^') => '^'
          case _ => '.'
        }}).mkString("")
      ret
    }

    // Minus becase the first row is equal to input
    val map = (0 until (count-1)).foldLeft(List(input))((ack, _) => genRow(ack.head) :: ack)
    println((for (l <- map) yield l.count(_ == '.')).sum)
  }


  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input.head, 40)
    part1(input.head, 400000)    
  }
}  
