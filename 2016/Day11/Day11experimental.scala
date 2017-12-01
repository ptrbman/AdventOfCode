object Day11 {

  import scala.collection.mutable.{ArrayStack => Stack, HashMap, ArrayBuilder}
  import scala.util.Sorting

  object Timer {

    private var startTime : Long = _
    private val runningOps = new Stack[String]

    // accumulated time spent in each operation
    private val accumulatedTimes = new HashMap[String, Long] {
      override def default(op : String) : Long = 0
    }
    // number of call to each operation
    private val callCounters = new HashMap[String, Int] {
      override def default(op : String) : Int = 0
    }
    
    private def addTime : Unit = {
      val now = System.nanoTime
      if (!runningOps.isEmpty) {
        val op = runningOps.top
        accumulatedTimes += (op -> (accumulatedTimes(op) + now - startTime))
      }
      startTime = now
    }
    
    def measure[A](op : String)(comp : => A) : A = {
      addTime
      callCounters += (op -> (callCounters(op) + 1))
      runningOps push op
      
      val res =
        try {
          comp
        } finally {
          addTime
          runningOps.pop
        }
      
      res
    }
    
    def reset : Unit = {
      accumulatedTimes.clear
      callCounters.clear
    }
    
    override def toString : String = {
      val resBuf = ArrayBuilder.make[(String, Int, Long)]

      for ((op, time) <- accumulatedTimes)
        resBuf += ((op, callCounters(op), time))

      val resAr = resBuf.result
      Sorting.stableSort(resAr)

      val table =
        (for ((op, count, time) <- resAr) yield {
          var paddedOp = op
          // HACK: for some reason, the <code>RichString.format</code> method does
          // not work
          while (paddedOp.size < 40)
            paddedOp = paddedOp + " "
          
          val timeInMS = time.toDouble / 1000000.0
          
          (paddedOp + "\t" + count + "\t" + timeInMS + "ms")
        }) mkString "\n"
      
      val totalTime = (0l /: accumulatedTimes.valuesIterator)(_ + _)
      val totalTimeInMS = totalTime.toDouble / 1000000.0
      
      val totalCalls = (0 /: callCounters.valuesIterator)(_ + _)
      
      val total = "Total: " + totalCalls + ", " + totalTimeInMS + "ms"
      
      table + "\n" + total
    }
    
  }

  
  import scala.collection.mutable.Set

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

  def doMove(f : Floors, items : List[Int], from : Int, to : Int) = Timer.measure("doMove") {
    val floors = 
      for (i <- 0 until f.length) yield {
        if (i == from) Timer.measure("doMove.case1") {
          (f(i) diff items).filter(_ != 0)
        } else if (i == to) Timer.measure("doMove.case2") {
          (0 :: (f(i) ++ items)).sorted
        } else Timer.measure("doMove.case3") {
          f(i) : List[Int]
        }
      }
    floors.toList
  }

  def genMoves(f : Floors) = Timer.measure("genMoves") {
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

    val hasGenerator = tmpItems.find(x => x < 0).isDefined

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

  def checkFloors(fs : Floors) : Boolean =  Timer.measure("checkFloors") {
    for (f <- fs) {
      // Is there a chip with generators and not its own?
      for (item <- f) {
        item match {
          case chip if chip > 0 => {
            if (!(f contains -chip) && (f.find(_ < 0).isDefined))
              return false
          }
          case _ => 
        }
      }
    }
    true
  }

  def score(fs : Floors) : Int = Timer.measure("score") {
    (for (i <- 0 until 4) yield {
      fs(i).map(_.abs).sum*i
    }).sum
  }

  def part1() = {
    // HARDCODED input

    def simple = { 
      val floors = List(List(0, 1, 2),
        List(-1),
        List(-2),
        List())

      val finalFloors = List(List(-2, -1, 0, 1, 2),
        List(),
        List(),
        List()).reverse

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

    val (floors, finalFloors) = input1

    
    val states = Set() : Set[Floors]
    states +=  finalFloors // 
    import scala.collection.mutable.ListBuffer
    val toCheck = ListBuffer(finalFloors) : ListBuffer[Floors]
    var iteration = 0

    println(score(finalFloors))

    Timer.measure("while") { 
      while (!toCheck.isEmpty && !(states contains floors)) {
        println(iteration)
        val checkStates = toCheck.toList
        toCheck.clear
        for (s <- checkStates) {
          for (nf <- genMoves(s).toList; if (!(states contains nf) && checkFloors(nf))) {
            states += nf
            toCheck += nf
          }
        }
        iteration += 1
      }
    }
    println("--" + iteration + "--")
    println(Timer)
    println(states(floors))
    println(states.size)
  }

  def main (args : Array[String]) = {
    part1()
  }
}  
