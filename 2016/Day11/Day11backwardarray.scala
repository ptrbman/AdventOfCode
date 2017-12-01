object Day11 {

  import scala.collection.mutable.{ArrayStack => Stack, HashMap, ArrayBuilder}
  import scala.util.Sorting
  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.Set  

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












  type Floors = Array[Int]

  def doMove(f : Floors, items : List[Int], to : Int) = Timer.measure("doMove") {
    val newFloors = Array.ofDim[Int](f.length)
    f.copyToArray(newFloors)
    for (i <- items)
      newFloors(i) = to
    newFloors(0) = to
    newFloors
  }

  def genMoves(f : Floors) = Timer.measure("genMoves") {
    // We can move elevator either up or down
    // And we can take one or two things with us
    val elevatorFloor = f(0)
    val tmpItems =  Timer.measure("genMoves.tmpItems") { for (i <- 1 until f.length; if f(i) == elevatorFloor) yield i }
    val singleItems = Timer.measure("genMoves.singleItems") { tmpItems.map(List(_)) }
    val targetFloors = Timer.measure("genMoves.targetFloors") {
      elevatorFloor match {
        case 0 => List(1)
        case 1 => List(0, 2)
        case 2 => List(1, 3)
        case 3 => List(2)
      }
    }

    def compatible(i1 : Int, i2 : Int) = Timer.measure("genMoves.compatible") {
      val i1Gen = (i1 % 2 == 0)
      val i2Gen = (i2 % 2 == 0)
      (i1Gen, i2Gen) match {
        case (true, true) => true
        case (false, false) => true
        case (true, false) => false
        case (false, true) => i1 == i2 - 1
      }
    }


    val doubleItems =
      Timer.measure("genMoves.doubleItems") {
        for (i1 <- tmpItems; i2 <- tmpItems; if i1 < i2; if compatible(i1, i2)) yield List(i1, i2)
      }
    val items = Timer.measure("genMoves.items") { (singleItems ++ doubleItems).toList }

    Timer.measure("genMoves.forloop") {
      for (i <- items; tf <- targetFloors) yield {
        val newF = doMove(f, i, tf)
        newF
      }
    }
  }

  def checkFloors(fs : Floors) : Boolean =  Timer.measure("checkFloors") {
    for (i <- 0 until 3) {
      val isGenerator = (2 to fs.length by 2).find(x => fs(x) == i).isDefined
      if (isGenerator)
        if ((1 to (fs.length - 1) by 2).find(x => fs(x) == i && fs(x+1) != i).isDefined)
          return false
    }
    true
  }

  def part1() = {
    // HARDCODED input

    def simple = {
      val floors = Array.ofDim[Int](5)
      floors(0) = 0
      floors(1) = 0
      floors(2) = 1
      floors(3) = 0
      floors(4) = 2

      val finalFloors = Array.ofDim[Int](5)
      finalFloors(0) = 3
      finalFloors(1) = 3
      finalFloors(2) = 3
      finalFloors(3) = 3
      finalFloors(4) = 3

      (floors, finalFloors)
    }

    val input1 = {
      val floors =
        Array(0,
          0, 0,
          1, 0,
          1, 0,
          2, 2,
          2, 2)
      val finalFloors =
        Array(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

      (floors, finalFloors)
    }

    val input2 = {
      val floors =
        Array(0,
          0, 0,
          1, 0,
          1, 0,
          2, 2,
          2, 2,
          0, 0,
          0, 0)
      val finalFloors =
        Array(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

      (floors, finalFloors)
    }    

    val (floors, finalFloors) = input2

    type State = List[Int]
    val states = Set() : Set[State]
    states +=  finalFloors.toList

    val toCheck = ListBuffer(finalFloors) : ListBuffer[Floors]
    var iteration = 0

    Timer.measure("while") { 
      while (iteration < 15 && !toCheck.isEmpty && !(states contains floors.toList)) {
        println(iteration)
        println("\ttoCheck:" + toCheck.length)
        println("\tstates: " + states.size)
        val checkStates = toCheck.toList
        toCheck.clear
        for (s <- checkStates) {
          println("ADDING:" + s.mkString(", "))
          for (nf <- genMoves(s).toList; if (!(states contains nf.toList) && checkFloors(nf))) {
            states += nf.toList
            toCheck += nf
          }
        }
        iteration += 1
      }
    }
    println("--" + iteration + "--")
    println(Timer)
    println(states.size)
  }

  def main (args : Array[String]) = {
    part1()
  }
}  
