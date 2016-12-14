    import scala.collection.mutable.Map

object Day13 {
  def part12(input : Int) = {
    val target = (31, 39)
    def isWall(x : Int, y : Int) = {
      if (x < 0 || y < 0)
        true
      else 
        (x*x + 3*x + 2*x*y + y + y*y + input).toBinaryString.filter(_ == '1').length % 2 == 1
    }

    object MinOrder extends Ordering[(Int, Int, Int)] {
      def compare(t1 : (Int, Int, Int), t2 : (Int, Int, Int)) = {
        t2._1 compare t1._1
      }
    }    


    val todo = scala.collection.mutable.PriorityQueue[(Int, Int, Int)]()(MinOrder)
    val distance = Map() : Map[(Int, Int), Int]
    todo.enqueue((0, 1, 1))

    while (!(distance contains target)) {
      val (d, x, y) = todo.dequeue
      distance += (x,y) -> d

      def tryAdd(xx : Int, yy : Int) = {
        if (!isWall(xx, yy) && (!(distance contains (xx, yy)) || distance((xx,yy)) > d + 1))
          todo.enqueue((d+1, xx, yy))
      }

      tryAdd(x -1 , y)
      tryAdd(x +1 , y)
      tryAdd(x, y - 1)      
      tryAdd(x, y + 1)
    }

    println(distance(target))
    println(distance.values.filter(_ <= 50).size)
  }

  def main (args : Array[String]) = {
    val input = 1364
    part12(input)
  }
}  
