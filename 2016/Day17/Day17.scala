object Day17 {

  import java.security.MessageDigest

  def md5(s: String) : String= {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }

  def solve(input : String, maximize : Boolean) = {

    var bestSolution =
      if (maximize) ""
      else "a"*99999

    def move(x : Int, y : Int, str : String) : Unit = {
      if ((x,y) == (3,3)) {
        if ((maximize && str.length > bestSolution.length) || (!maximize && str.length < bestSolution.length))
          bestSolution = str
      } else {
        var possibleMoves =
          (x,y) match {
            case (0,0) => List("R", "D")
            case (1,0) => List("R", "D", "L")
            case (2,0) => List("R", "D", "L")
            case (3,0) => List("D", "L")
              
            case (0,1) => List("U", "R", "D")
            case (3,1) => List("U", "L", "D")

            case (0,2) => List("U", "R", "D")
            case (3,2) => List("U", "L", "D")

            case (0,3) => List("R", "U")
            case (1,3) => List("R", "U", "L")
            case (2,3) => List("R", "U", "L")
            case (3,3) => List("U", "L")

            case _ => List("U", "R", "D", "L")
          }

        val md5str = md5(str)
        if (md5str(0) != 'b' && md5str(0) != 'c' && md5str(0) != 'd' && md5str(0) != 'e' && md5str(0) != 'f')
          possibleMoves = possibleMoves diff List("U")
        if (md5str(1) != 'b' && md5str(1) != 'c' && md5str(1) != 'd' && md5str(1) != 'e' && md5str(1) != 'f')
          possibleMoves = possibleMoves diff List("D")
        if (md5str(2) != 'b' && md5str(2) != 'c' && md5str(2) != 'd' && md5str(2) != 'e' && md5str(2) != 'f')
          possibleMoves = possibleMoves diff List("L")
        if (md5str(3) != 'b' && md5str(3) != 'c' && md5str(3) != 'd' && md5str(3) != 'e' && md5str(3) != 'f')
          possibleMoves = possibleMoves diff List("R")

        for (m <- possibleMoves) {
          val (xx, yy) =
            m match {
              case "U" => (x, y - 1)
              case "D" => (x, y + 1)
              case "L" => (x - 1, y)
              case "R" => (x + 1, y)
            }
          move(xx, yy, str + m)
        }
      }
    }

    move(0,0,input)
    bestSolution
  }

  def main (args : Array[String]) = {
    val input = "pvhmgsws"
    println(solve(input, false).drop(input.length))
    println(solve(input, true).length - input.length)
  }
}  
