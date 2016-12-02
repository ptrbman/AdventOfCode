object Day2 {

  def move(letter : Char, x : Int, y : Int) = {
    val (xx, yy) = 
      letter match {
        case 'U' => (x, y-1)
        case 'D' => (x, y+1)
        case 'L' => (x-1, y)
        case 'R' => (x+1, y)
      }

    val (fx, fy) = (if (xx < 0 || xx > 2) x else xx, if (yy < 0 || yy > 2) y else yy)
    // println((x, y) + " (" + posToInt(x, y) + ") == " + letter + " ==>" + (fx, fy) + " (" + posToInt(fx, fy) + ")")
    (fx, fy)
  }


  def moveString(str : String, x : Int, y : Int) = {
    val (finX, finY) = 
      str.foldLeft((x,y)){
        (pos, char) => {
          val (x, y) = pos
          move(char, x, y)
        }
      }
    (finX, finY)
  }

  def posToInt(x : Int, y : Int) = {
    (x, y) match {
      case (0,0) => 1
      case (1,0) => 2
      case (2,0) => 3

      case (0,1) => 4
      case (1,1) => 5
      case (2,1) => 6

      case (0,2) => 7
      case (1,2) => 8
      case (2,2) => 9
    }
  }

  def part1(input : List[String]) = {
    var x, y = 1
    for (str <- input) {
      val (newX, newY) = moveString(str, x, y)
      x = newX
      y = newY
      print(posToInt(x, y))
    }
    println()
  }


  def move2(letter : Char, x : Int, y : Int) = {
    val (xx, yy) = 
      letter match {
        case 'U' => (x, y-1)
        case 'D' => (x, y+1)
        case 'L' => (x-1, y)
        case 'R' => (x+1, y)
      }

    val badPositions = List(
      (0,0), (1,0),       (3,0), (4,0),
      (0,1),                     (4,1),

      (0,3),                     (4,3),
      (0,4), (1,4),       (3,4), (4,4)
    )

    val (fx, fy) = 
      if ((badPositions contains (xx, yy)) || xx < 0 || xx > 4 || yy < 0 || yy > 4)
        (x, y)
      else
        (xx, yy)
    // println((x, y) + " (" + posToChar2(x, y) + ") == " + letter + " ==>" + (fx, fy) + " (" + posToChar2(fx, fy) + ")")
    (fx, fy)
  }


  def moveString2(str : String, x : Int, y : Int) = {
    val (finX, finY) = 
      str.foldLeft((x,y)){
        (pos, char) => {
          val (x, y) = pos
          move2(char, x, y)
        }
      }
    (finX, finY)
  }

  def posToChar2(x : Int, y : Int) = {
    (x, y) match {
      case (2,0) => '1'

      case (1,1) => '2'
      case (2,1) => '3'
      case (3,1) => '4'

      case (0,2) => '5'
      case (1,2) => '6'
      case (2,2) => '7'
      case (3,2) => '8'
      case (4,2) => '9'

      case (1,3) => 'A'
      case (2,3) => 'B'
      case (3,3) => 'C'

      case (2,4) => 'D'
    }
  }  

  def part2(input : List[String]) = {
    var x = 0
    var y = 2
    for (str <- input) {
      val (newX, newY) = moveString2(str, x, y)
      x = newX
      y = newY
      print(posToChar2(x, y))
    }
    println()
  }

  def main (args : Array[String]) = {
    val input = io.Source.fromFile("Day2.txt").getLines.toList
    part1(input)
    part2(input)
  }
}  
