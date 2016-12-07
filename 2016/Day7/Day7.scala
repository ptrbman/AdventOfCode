object Day5 {
  import scala.collection.mutable.Map

  def part1(input : List[String]) = {
    def outside(input : String, outsideDouble : Boolean, insideDouble : Boolean) : (Boolean, Boolean) = {
      if ((outsideDouble && insideDouble) || input.isEmpty) {
        (outsideDouble, insideDouble)
      } else {
        val (s1, s2) = input.span(_ != '[')
        if (outsideDouble || !s1.sliding(4).find(str => (str == str.reverse) && (str(0) != str(1))).isEmpty)        
          inside(s2, true, insideDouble)
        else
          inside(s2, false, insideDouble)
      }
    }

    def inside(input : String, outsideDouble : Boolean, insideDouble : Boolean) : (Boolean, Boolean) = {
      if ((outsideDouble && insideDouble) || input.isEmpty) {
        (outsideDouble, insideDouble)
      } else {
        val (s1, s2) = input.span(_ != ']')
        if (insideDouble || !s1.sliding(4).find(str => (str == str.reverse) && (str(0) != str(1))).isEmpty)
          outside(s2, outsideDouble, true)
        else
          outside(s2, outsideDouble, false)
      }
    }

    val count =
      for (str <- input) yield {
        val (out, in) = outside(str, false, false)
        out && !in
      }
    println(count.count(_ == true))
  }

  def part2(input : List[String]) = {
    def outside(input : String, outsideStrings : List[String], insideStrings : List[String]) : (List[String], List[String]) = {
      if (input.isEmpty) {
        (insideStrings, outsideStrings)
      } else {
        val (s1, s2) = input.span(_ != '[')
        val newOutsideStrings = s1.sliding(3).filter(str => str(0) == str(2) && str(0) != str(1)).toList
        inside(s2, newOutsideStrings ++ outsideStrings, insideStrings)
      }
    }

    def inside(input : String, outsideStrings : List[String], insideStrings : List[String]) : (List[String], List[String]) = {
      if (input.isEmpty) {
        (insideStrings, outsideStrings)
      } else {
        val (s1, s2) = input.span(_ != ']')
        val newInsideStrings = s1.sliding(3).filter(str => str(0) == str(2) && str(0) != str(1)).toList
        outside(s2, outsideStrings, newInsideStrings ++ insideStrings)
      }
    }    

    val count =
      for (str <- input) yield {
        val (out, in) = outside(str, List(), List())
        out.find(o => !in.find(i => i(0) == o(1) && i(1) == o(0)).isEmpty).isEmpty
      }
    println(count.count(_ == true))
  }  


  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input)
    part2(input)    
  }
}  
