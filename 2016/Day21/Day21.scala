object Day20 {
  def part1(program : List[String], code : String) = {
    val swapP = "swap position (\\d+) with position (\\d+)".r
    val swapL = "swap letter (\\S+) with letter (\\S+)".r
    val revP = "reverse positions (\\d+) through (\\d+)".r
    val rotLP = "rotate left (\\d+) steps?".r
    val rotRP = "rotate right (\\d+) steps?".r    
    val moveP = "move position (\\d+) to position (\\d+)".r
    val rotP = "rotate based on position of letter (\\S+)".r
    def runCmd(cmd : String, input : Array[Char]) = {
      def swap(i1 : Int, i2 : Int) = {
        val tmp = input(i1)
        input(i1) = input(i2)
        input(i2) = tmp
      }

      def rev(s : Int, e : Int) = {
        val tmp = 
          (for (i <- s to e) yield {
            input(i)
          }).toList.reverse

        for (i <- s to e)
          input(i) = tmp(i-s)
      }

      def rotL(s : Int) = {
        val tmp = input.toList
        val steps = if (s >= tmp.length) s - tmp.length else s        
        val tmp2 = tmp.drop(steps) ++ tmp.take(steps)
        for (i <- 0 until input.length)
          input(i) = tmp2(i)
      }

      def rotR(s : Int) = {
        val tmp = input.toList
        val steps = if (s >= tmp.length) s - tmp.length else s        
        val tmp2 = tmp.drop(tmp.length - steps) ++ tmp.take(tmp.length - steps)
        for (i <- 0 until input.length)
          input(i) = tmp2(i)
      }

      def move(i1 : Int, i2 : Int) = {
        val tmp = input.toList
        val c = tmp(i1)
        val tmp2 = tmp.take(i1) ++ tmp.drop(i1+1)
        val tmp3 = tmp2.take(i2) ++ List(c) ++ tmp2.drop(i2)
        for (i <- 0 until input.length)
          input(i) = tmp3(i)
      }

      def rotB(i : Int) = {
        if (i >= 4)
          rotR(i + 2)
        else
          rotR(i + 1)
      }

      cmd match {
        case swapP(p1, p2) => swap(p1.toInt, p2.toInt)
        case swapL(l1, l2) => swap(input indexOf l1.head, input indexOf l2.head)
        case revP(p1, p2) => rev(p1.toInt, p2.toInt)
        case rotLP(s) => rotL(s.toInt)
        case rotRP(s) => rotR(s.toInt)
        case moveP(p1, p2) => move(p1.toInt, p2.toInt)
        case rotP(l) => rotB(input indexOf l.head)
      }
    }

    val data =  code.toArray
    for (p <- program)
      runCmd(p, data)
    println(data.mkString(""))
  }


  def part2(program : List[String], code : String) = {
    val swapP = "swap position (\\d+) with position (\\d+)".r
    val swapL = "swap letter (\\S+) with letter (\\S+)".r
    val revP = "reverse positions (\\d+) through (\\d+)".r
    val rotLP = "rotate left (\\d+) steps?".r
    val rotRP = "rotate right (\\d+) steps?".r    
    val moveP = "move position (\\d+) to position (\\d+)".r
    val rotP = "rotate based on position of letter (\\S+)".r
    def runCmd(cmd : String, input : Array[Char]) = {

      def swap(i1 : Int, i2 : Int) = {
        val tmp = input(i1)
        input(i1) = input(i2)
        input(i2) = tmp
      }

      def rev(s : Int, e : Int) = {
        val tmp = 
          (for (i <- s to e) yield {
            input(i)
          }).toList.reverse

        for (i <- s to e)
          input(i) = tmp(i-s)
      }

      def rotL(s : Int) = {
        val tmp = input.toList
        val steps = if (s >= tmp.length) s - tmp.length else s        
        val tmp2 = tmp.drop(steps) ++ tmp.take(steps)
        for (i <- 0 until input.length)
          input(i) = tmp2(i)
      }

      def rotR(s : Int) = {
        val tmp = input.toList
        val steps = if (s >= tmp.length) s - tmp.length else s        
        val tmp2 = tmp.drop(tmp.length - steps) ++ tmp.take(tmp.length - steps)
        for (i <- 0 until input.length)
          input(i) = tmp2(i)
      }

      def move(i1 : Int, i2 : Int) = {
        val tmp = input.toList
        val c = tmp(i1)
        val tmp2 = tmp.take(i1) ++ tmp.drop(i1+1)
        val tmp3 = tmp2.take(i2) ++ List(c) ++ tmp2.drop(i2)
        for (i <- 0 until input.length)
          input(i) = tmp3(i)
      }

      def reverseRotB(c : Char) = {
        var rots = 0
        var cur = input.toList
        var cont = true
        while (cont) {
          rots += 1
          val tmp = cur.drop(1) ++ cur.take(1)

          val idx = tmp indexOf c
          if ((idx >= 4) && (idx + 2) == rots)
            cont = false
          else if (idx < 4 && (idx + 1) == rots)
            cont = false
          cur = tmp
        }

        for (i <- 0 until input.length)
          input(i) = cur(i)        
      }

      cmd match {
        case swapP(p1, p2) => swap(p1.toInt, p2.toInt) // SAME
        case swapL(l1, l2) => swap(input indexOf l1.head, input indexOf l2.head) // SAME
        case revP(p1, p2) => rev(p1.toInt, p2.toInt) // SAME
        case rotLP(s) => rotR(s.toInt) // other direction
        case rotRP(s) => rotL(s.toInt) // other direction
        case moveP(p1, p2) => move(p2.toInt, p1.toInt) // Just change arguments
        case rotP(l) => reverseRotB(l.head)
      }
    }

    val data =  code.toArray
    for (p <- program.reverse)
      runCmd(p, data)
    println(data.mkString(""))
  }  

  def main (args : Array[String]) = {
    val program = io.Source.fromFile("input.txt").getLines.toList
    val code = "abcdefgh"
    val code2 = "fbgdceah"
    part1(program, code)
    part2(program, code2)
  }
}  
