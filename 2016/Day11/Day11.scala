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
    // println("genMoves(" + f.mkString(", ") + ")")
    val elevatorFloor = f(0)

    val floorItems =  Timer.measure("genMoves.tmpItems") { for (i <- 1 until f.length; if f(i) == elevatorFloor) yield i }
    // println("\tfloorItems:"  +floorItems.mkString(", "))
    val targetFloors = Timer.measure("genMoves.targetFloors") {
      elevatorFloor match {
        case 0 => List(1)
        case 1 => List(0, 2)
        case 2 => List(1, 3)
        case 3 => List(2)
      }
    }

    val chipGenCouples = (1 to (f.length-1) by 2).filter(x => f(x) == elevatorFloor && f(x+1) == elevatorFloor)
    val singleChips = (1 to (f.length-1) by 2).filter(x => f(x) == elevatorFloor && f(x+1) != elevatorFloor)
    val singleGens = (2 to f.length by 2).filter(x => f(x) == elevatorFloor && f(x-1) != elevatorFloor)

    // println("\tchipGenCouples: " + chipGenCouples.mkString(", "))
    // println("\tsingleChips: " + singleChips.mkString(", "))
    // println("\tsingleGens: " + singleGens.mkString(", "))    

    val singleItems =
      (if (!chipGenCouples.isEmpty)
        List(chipGenCouples.head) :: List(chipGenCouples.head+1) :: List()
      else
        List()) ++ (singleChips.map(List(_)) ++ singleGens.map(List(_))).toList

    val doubleItems = ListBuffer() : ListBuffer[List[Int]]
    if (!chipGenCouples.isEmpty) {
      if (!singleChips.isEmpty)
          throw new Exception("Both couples single chips?" + f.mkString(", "))        
      // We have a generator, thus there are no single chips
      // This means we cannot move any generator without its chip
      // We could move two chips 
      doubleItems += List(chipGenCouples.head, chipGenCouples.head + 1)
      if (chipGenCouples.length == 2 && singleGens.isEmpty)
        doubleItems += List(chipGenCouples(0) + 1, chipGenCouples(1) + 1)

      if (chipGenCouples.length >= 2)
        doubleItems += List(chipGenCouples(0), chipGenCouples(1))

      for (g1 <- ((chipGenCouples.head + 1) :: singleGens.toList); g2 <- singleGens; if g1 < g2)
        doubleItems += List(g1, g2)
    } else {
      // No couples, that means we only have single chips or only single generators
      if (!singleChips.isEmpty) {
        if (!singleGens.isEmpty)
          throw new Exception("Both single chips and single generators?" + f.mkString(", "))
        for (c1 <- singleChips; c2 <- singleChips; if c1 < c2) 
          doubleItems += List(c1, c2)
      }
      if (!singleGens.isEmpty) {
        if (!singleChips.isEmpty)
          throw new Exception("Both single chips and single generators?" + f.mkString(", "))
        for (g1 <- singleGens; g2 <- singleGens; if g1 < g2) 
          doubleItems += List(g1, g2)
      }
    }

    val items = Timer.measure("genMoves.items") { (singleItems ++ doubleItems.toList) : List[List[Int]] }

    // println("-----")
    Timer.measure("genMoves.forloop") {
      for (i <- items; tf <- targetFloors) yield {
        val newF = doMove(f, i, tf)
        // println(newF.mkString(", "))
        newF
      }
    }
  }

  def checkFloorsAux(fs : Floors) : Boolean =  Timer.measure("checkFloors") {
    for (i <- 0 until 4) {
      val isGenerator = (2 to fs.length by 2).find(x => fs(x) == i).isDefined
      if (isGenerator)
        if ((1 until fs.length by 2).find(x => fs(x) == i && fs(x+1) != i).isDefined) {
          // println("fail floors: " + i)
          return false
        }
    }
    true
  }
  def checkFloors(fs : Floors) : Boolean = {
    val ret = checkFloorsAux(fs)
    // println("checkFloors(" + fs.mkString(", "))
    // println("\t" + ret)
    ret
  }

  def score(fs : Floors) : Int = Timer.measure("score") {
    fs.sum
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
    states +=  floors.toList

    val toCheck = ListBuffer(floors) : ListBuffer[Floors]
    var iteration = 0

    Timer.measure("while") { 
      while (!toCheck.isEmpty && !(states contains finalFloors.toList)) {
        println(iteration)
        println("toCheck: " + toCheck.length)
        println("states: " + states.size)
        val checkStates = Timer.measure("checkStates") { toCheck.toList } 
        toCheck.clear
        for (s <- checkStates) Timer.measure("while.for") {
          for (nf <- genMoves(s).toList; if (score(nf) > iteration - 1) ; if (!(Timer.measure("contains") { states contains nf.toList }) && checkFloors(nf))) {
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
