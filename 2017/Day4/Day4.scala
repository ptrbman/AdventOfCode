object Day4 {

  def part1(input : List[String]) = {
    def isValid(str : String) = {
      val arr = str.split(" ")
      arr.toSet.size == arr.length
    }
    input.count(isValid)
  }

  def part2(input : List[String]) = {
    def isValid(str : String) = {
      val arr = str.split(" ").map(_.toSet)
      arr.toSet.size == arr.length
    }
    input.count(isValid)
  }  

  def main(args : Array[String]) = {
    val input = io.Source.fromFile("Day4.txt").getLines.toList    
    println(part1(input))
    println(part2(input))    
  }
}
