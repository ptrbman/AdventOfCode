object Day1 {

  object Direction extends Enumeration {
    type Direction = Value
    val North, South, West, East = Value
  }

  import Direction._

  def calcDir(moveCommand : Char, dir : Direction) = {
    (moveCommand, dir) match {
      case ('L', North) => West
      case ('L', West) => South
      case ('L', South) => East
      case ('L', East) => North
      case ('R', West) => North
      case ('R', South) => West
      case ('R', East) => South
      case ('R', North) => East
    }
  }

  def move(dir : Direction, x : Int, y : Int, command : String) = {
    val moveCommand = command.head
    val moveDistance = command.tail.toInt
    val newDir = calcDir(moveCommand, dir)
    val (newX, newY) =
      newDir match {
        case North => (x, y-moveDistance)
        case South => (x, y+moveDistance)
        case West => (x-moveDistance, y)
        case East => (x+moveDistance, y)          
      }
    (newDir, newX, newY)
  }


  def part1(input : List[String]) = {
    val commands = input(0).split(",").map(_.trim)
    val result = commands.foldLeft((North, 0, 0)){
      (old, cmd) =>
      val (dir, x, y) = old
      move(dir, x, y, cmd)
    }
    val (finalDir, finalX, finalY) = result
    println(finalX.abs + finalY.abs)    
  }

  def part2(input : List[String]) = {
    var goal = None : Option[(Int, Int)]
    var curDir = North
    var curX, curY = 0
    var commands = input(0).split(",").map(_.trim)
    var visited = Map() : Map[(Int, Int), Boolean]
    visited += (0,0) -> true
    while (goal.isEmpty) {
      val (dir, dist) = (commands.head.head, commands.head.tail.toInt)
      val newDir = calcDir(dir, curDir)
      newDir match {
        case North => {
          for (i <- 0 until dist) {
            curY = curY - 1
            if (visited getOrElse ((curX, curY),false))
              goal = Some((curX, curY))
            else
              visited += (curX, curY) -> true
          }
        }
        case South => {
          for (i <- 0 until dist) {
            curY = curY + 1
            if (visited getOrElse ((curX, curY),false))
              goal = Some((curX, curY))
            else
              visited += (curX, curY) -> true
          }
        }
        case West => {
          for (i <- 0 until dist) {
            curX = curX - 1
            if (visited getOrElse ((curX, curY),false))
              goal = Some((curX, curY))
            else
              visited += (curX, curY) -> true
          }
        }
        case East => {
          for (i <- 0 until dist) {
            curX = curX + 1
            if (visited getOrElse ((curX, curY),false))
              goal = Some((curX, curY))
            else
              visited += (curX, curY) -> true
          }
        }
      }

      curDir = newDir
      commands = commands.tail      
    }
    val (finalX, finalY) = goal.get
    println(finalX.abs + finalY.abs)        
  }

  def main(args : Array[String]) = {
    val input = io.Source.fromFile("inputs/Day1.txt").getLines.toList
    part1(input)
    part2(input)    
  }
}
