object Day5 {

  def part1(input : List[String]) = {
    var steps = 0
    var curState = 0
    var states = input.map(_.toInt).toArray
    while (curState >= 0 && curState < states.length) {
      steps += 1
      val newState = curState + states(curState)
      states(curState) += 1
      curState = newState
    }
    steps
  }


  def part2(input : List[String]) = {
    var steps = 0
    var curState = 0
    var states = input.map(_.toInt).toArray
    while (curState >= 0 && curState < states.length) {
      steps += 1
      val newState = curState + states(curState)
      if (states(curState) >= 3)
        states(curState) -= 1
      else
        states(curState) += 1        
      curState = newState
    }
    steps
  }  


  def main(args : Array[String]) = {
    val input = io.Source.fromFile("Day5.txt").getLines.toList    
    println(part1(input))
    println(part2(input))    
  }
}
