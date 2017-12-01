object Day11 {
  import scala.collection.mutable.Set

  // CHIP +, GENERATOR -
  // HYDROGEN = 1/-1
  // LITHIUM = 2/-2
  // ELEVATOR = 0
  type Floors = Array[Array[Boolean]]

  def printFloors(f : Floors) = {
    for (i <- 0 until f.length) {
      for (j <- 0 until f(i).length) {
        print(f(i)(j) + " ")
      }
      println("")
    }
  }

  // def floorToString(f : List[Int]) = {
  //   if (f.isEmpty)
  //     "."
  //   else
  //     (for (ff <- f) yield {
  //       ff match {
  //         case 0 => "E"
  //         case i => " " + i
  //         // case 1 => "HM"
  //         // case -1 => "HG"
  //         // case 2 => "LM"
  //         // case -2 => "LG"
  //       }}).mkString(" ")
  // }


  // def floorsToString(f : Floors) =
  //   (for (i <- 0 until 4) yield {
  //     floorToString(f(3-i))
  //   }).mkString("\n")

  def doMove(f : Floors, items : List[Int], from : Int, to : Int) = {
    val newArray = Array.ofDim[Boolean](f.length, f(0).length) : Floors
    for (i <- items) {
      newArray(from)(i) = false
      newArray(to)(i) = true
    }
    newArray(from)(0) = false
    newArray(to)(0) = true
    newArray
  }

  def genMoves(f : Floors) = {
    // We can move elevator either up or down
    // And we can take one or two things with us
    def findElevatorFloor : Int = {
      for (i <- 0 until f.length)
        if (f(i)(0))
          return i
      printFloors(f)
      throw new Exception("No elevator?")
    }
    val elevatorFloor = findElevatorFloor
    val targetFloors= 
      if (elevatorFloor == 0)
        List(1)
      else if (elevatorFloor == 3)
        List(2)
      else
        List(elevatorFloor - 1, elevatorFloor + 1)
    // Ignore elevator
    val tmpItems =
      (for (i <- 1 until f(elevatorFloor).length if f(elevatorFloor)(i)) yield i).toList
    val singleItems = tmpItems.map(List(_)).toList : List[List[Int]]

    def filterItems(items : List[Int]) = {
      items match {
        case List(i) => true
        case List(i, j) => {
          val iGenerator = (i % 2 == 0)
          val jGenerator = (j % 2 == 0)

          if (iGenerator && jGenerator)
            true
          else if (!iGenerator && !jGenerator)
            true
          else if (iGenerator)
            false
          else if (i + 1 == j)
            true
          else
            false
        }
      }
    }

    val doubleItems = (for (i1 <- tmpItems; i2 <- tmpItems; if i1 < i2) yield List(i1, i2)) : List[List[Int]]
    val items = (singleItems ++ doubleItems).filter(filterItems)

    for (i <- items; tf <- targetFloors) yield
      doMove(f, i, elevatorFloor, tf)
  }

  def checkFloors(fs : Floors) : Boolean = {
    for (f <- fs) {
      // Is there a chip with generators and not its own?
      val isGenerator = (for (i <- 2 until fs.length by 2) yield f(i)).find(x => x == true).isDefined
      if (isGenerator) {
        for (chip <- 1 to fs.length by 2) {
          if (f(chip) && !f(chip+1))
            return false
        }
      }
    }
    true
  }

  def part1() = {
    // HARDCODED input

    def simple = { 
      val floors = Array.ofDim[Boolean](4, 5)
      floors(0)(0) = true
      floors(0)(1) = true
      floors(0)(3) = true

      floors(1)(2) = true

      floors(2)(4) = true

      val finalFloors = Array.ofDim[Boolean](4, 5)
      finalFloors(3)(0) = true
      finalFloors(3)(1) = true
      finalFloors(3)(2) = true
      finalFloors(3)(3) = true
      finalFloors(3)(4) = true      

      (floors, finalFloors)
    }


    // thulium 1
    // PLUTONIUM 2
    // STRONTIUM 3
    // PROMETHIUM 4
    // RUTHENIUM 5
    // ELERIUM 6
    // DILITHIUM 7

    val input1 = {
      val floors = List(List(0, -1, 1, -2, -3).sorted,
        List(2, 3).sorted,
        List(-4, 4, -5, 5).sorted,
        List())


      val finalFloors = List(List(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5),
        List(),
        List(),
        List()).reverse
      (floors, finalFloors)
    }

    val input2 = {
      val floors = List(List(0, -1, 1, -2, -3, -6, 6, -7, 7).sorted,
        List(2, 3).sorted,
        List(-4, 4, -5, 5).sorted,
        List())


      val finalFloors = List(List(-7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7),
        List(),
        List(),
        List()).reverse
      (floors, finalFloors)
    }    

    val (floors, finalFloors) = simple

    val goal = floors.map(_.toList).toList
    println(goal)
    type S = List[List[Boolean]]
    val states = Set() : Set[S]
    states +=  finalFloors.map(_.toList).toList
    import scala.collection.mutable.ListBuffer
    val toCheck = ListBuffer(finalFloors) : ListBuffer[Floors]
    var iteration = 0

    while (!toCheck.isEmpty && !(states contains goal)) {
      println(iteration)
      val checkStates = toCheck.toList
      toCheck.clear      
      for (s <- checkStates) {
        for (nf <- genMoves(s).toList; if (!(states contains nf.map(_.toList).toList) && checkFloors(nf))) {
          states += nf.map(_.toList).toList
          toCheck += nf
        }
      }
      iteration += 1
    }
    println(toCheck)
    println("--" + iteration + "--")
  }

  def main (args : Array[String]) = {
    part1()
  }
}  
