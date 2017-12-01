object Day11 {
  import scala.collection.mutable.Map

  // CHIP +, GENERATOR -
  // HYDROGEN = 1/-1
  // LITHIUM = 2/-2
  // ELEVATOR = 0
  type Floors = List[List[Int]]

  def floorToString(f : List[Int]) = {
    if (f.isEmpty)
      "."
    else
      (for (ff <- f) yield {
        ff match {
          case 0 => "E"
          case i => " " + i
          // case 1 => "HM"
          // case -1 => "HG"
          // case 2 => "LM"
          // case -2 => "LG"
        }}).mkString(" ")
  }


  def floorsToString(f : Floors) =
    (for (i <- 0 until 4) yield {
      floorToString(f(3-i))
    }).mkString("\n")

  def doMove(f : Floors, items : List[Int], from : Int, to : Int) = {
    val floors = 
      for (i <- 0 until f.length) yield {
        if (i == from) {
          (f(i) diff items).filter(_ != 0).sorted
        } else if (i == to) {
          (0 :: (f(i) ++ items)).sorted
        } else {
          f(i) : List[Int]
        }
      }
    floors.toList
  }

  def genMoves(f : Floors) = {
    // We can move elevator either up or down
    // And we can take one or two things with us
    val elevatorFloor = (0 to 3).find(f(_) contains 0).get
    val targetFloors= 
      if (elevatorFloor == 0)
        List(1)
      else if (elevatorFloor == 3)
        List(2)
      else
        List(elevatorFloor - 1, elevatorFloor + 1)
    val tmpItems =  f(elevatorFloor).filter(_ != 0) : List[Int]
    val singleItems = tmpItems.map(List(_)) : List[List[Int]]

    def filterItems(items : List[Int]) = {
      items match {
        case List(i) => true
        case List(i, j) => {
          if (i > 0 && j > 0)
            true
          else if (i < 0 && j < 0)
            true
          else if (i == -j)
            true
          else
            false
        }
      }
    }

    val doubleItems = (for (i1 <- tmpItems; i2 <- tmpItems; if i1 < i2) yield List(i1, i2)) : List[List[Int]]
    val items = (singleItems ++ doubleItems).filter(filterItems)

    // println("Elevator on: " + elevatorFloor)
    // println("Target Floors: " + targetFloors)
    // println("Item pairs: " + items.mkString(", "))

    for (i <- items; tf <- targetFloors) yield
      doMove(f, i, elevatorFloor, tf)
  }

  def checkFloors(fs : Floors) : Boolean = {
    for (f <- fs) {
      // Is there a chip with generators and not its own?
      for (item <- f) {
        item match {
          case 0 => 
          case chip if chip > 0 => {
            if (!(f contains -chip) && !(f.filter(_ < 0).isEmpty))
              return false
          }
          case gen if gen < 0 =>
        }
      }
    }
    true
  }

  def floorDone(fs : Floors) : Boolean = 
    fs(0).isEmpty && fs(1).isEmpty && fs(2).isEmpty

  def part1() = {
    // HARDCODED input
    val floors = List(List(0, 1, 2),
      List(-1),
      List(-2),
      List())

    // THULIUM 1
    // PLUTONIUM 2
    // STRONTIUM 3
    // PROMETHIUM 4
    // RUTHENIUM 5
    // val floors = List(List(0, -1, 1, -2, -3),
    //   List(2, 3),
    //   List(-4, 4, -5, 5),
    //   List())



    val checkedFloors = Map() : Map[Floors, Int]
    var solutions = List() : List[(Floors, Int)]

    object MinOrder extends Ordering[(Floors, Int)] {
      def compare(t1 : (Floors, Int), t2 : (Floors, Int)) = {
        t2._2 compare t1._2
      }
    }

    val todo = scala.collection.mutable.PriorityQueue[(Floors, Int)]()(MinOrder)
    todo.enqueue((floors, 0))
    checkedFloors += floors -> 0
    var iterations = 0
    var found = false
    while (!todo.isEmpty && !found) {
      val (fs, moves) = todo.dequeue
      if (iterations % 1000 == 0)
        println(">>>>>" + iterations + " <<<<< " + moves + " >>> " + checkedFloors.keys.size)
      iterations += 1
      println("$$$$" + moves + "$$$$$")
      println(floorsToString(fs))

      if (floorDone(fs)) {
        println((fs, moves))
        found = true
        solutions = (fs, moves) :: solutions
      } else {
        val newFloors = genMoves(fs)
        for (nf <- newFloors; if checkFloors(nf)) {
          if (!(checkedFloors contains nf) ||
            checkedFloors(nf) > moves + 1)
            checkedFloors += fs -> (moves + 1)
            todo.enqueue((nf, moves + 1))
        }
      }
    }
    println("--END--")
    println(iterations)    
    println(solutions.map(_._2).min)
  }

  def main (args : Array[String]) = {
    part1()
  }
}  
