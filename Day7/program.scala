import scala.collection.mutable.Map
import scala.collection.mutable.MutableList

println("Starting...")

val lines = io.Source.fromFile("input.txt").getLines

object CommandType extends Enumeration {
  type CommandType = Value
  val Not, And, Or, LShift, RShift, Assignment = Value
}

abstract class Command
case class Not(source : String, dest : String) extends Command
case class And(source1 : String, source2 : String, dest : String) extends Command
case class Or(source1 : String, source2 : String, dest : String) extends Command
case class LShift(source : String, steps : Int, dest : String) extends Command
case class RShift(source : String, steps : Int, dest : String) extends Command
case class AssInt(value : Int, dest : String) extends Command
case class AssSymbol(source : String, dest : String) extends Command

def getDest(cmd : Command) : String = {
  cmd match {
    case Not(source : String, dest : String) => dest
    case And(source1 : String, source2 : String, dest : String) => dest
    case Or(source1 : String, source2 : String, dest : String) => dest
    case LShift(source : String, steps : Int, dest : String) => dest
    case RShift(source : String, steps : Int, dest : String) => dest
    case AssInt(value : Int, dest : String) => dest
    case AssSymbol(source : String, dest : String) => dest
  }
}

def getSources(cmd : Command) : List[String] = {
  cmd match {
    case Not(source : String, dest : String) => List(source)
    case And(source1 : String, source2 : String, dest : String) => List(source1, source2)
    case Or(source1 : String, source2 : String, dest : String) => List(source1, source2)
    case LShift(source : String, steps : Int, dest : String) => List(source)
    case RShift(source : String, steps : Int, dest : String) => List(source)
    case AssInt(value : Int, dest : String) => List()
    case AssSymbol(source : String, dest : String) => List(source)
  }
}

def numeric(x: String) = x forall Character.isDigit

def parseString(s : String) : Command = {
  val splitted = s.split(" ")
  if (splitted(0) == "NOT") {
    Not(splitted(1), splitted(3))
  } else if (splitted(1) == "AND") {
    And(splitted(0), splitted(2), splitted(4))
  } else if (splitted(1) == "OR") {
    Or(splitted(0), splitted(2), splitted(4))
  } else if (splitted(1) == "LSHIFT") {
    LShift(splitted(0), splitted(2).toInt, splitted(4))
  } else if (splitted(1) == "RSHIFT") {
    RShift(splitted(0), splitted(2).toInt, splitted(4))
  } else if (splitted(1) == "->" && numeric(splitted(0))){
    AssInt(splitted(0).toInt, splitted(2))
  } else if (splitted(1) == "->") {
    AssSymbol(splitted(0), splitted(2))
  } else {
    throw new Exception("Mismatch command parse")
  }
}

val commands : Map[String, (Option[Int], Command)] = scala.collection.mutable.Map()
val calculated : Map[String, Int] = scala.collection.mutable.Map()
val cmds : scala.collection.mutable.MutableList[Command] = scala.collection.mutable.MutableList()

for (l <- lines) {
  val cmd = parseString(l)
  val dest = getDest(cmd)
  commands += (dest -> (None, cmd))
  cmds += cmd
}

def getValue(symbol : String) : Int = {
  if (numeric(symbol)) {
    symbol.toInt
  } else {
    val tuple = commands.get(symbol)
    val (calc, cmd) = tuple.get
    if (calc.isDefined) {
      calc.get
    } else {
      val retVal =
        cmd match {
          case Not(source, _) =>
            val lhs = getValue(source)
            val rhs = 65535.toInt
            val res = (lhs ^ rhs).toInt
            res

          case And(source1, source2, _) =>
            val lhs = getValue(source1)
            val rhs = getValue(source2)
            val res = (lhs & rhs).toInt
            res

          case Or(source1, source2, _) =>
            val lhs = getValue(source1)
            val rhs = getValue(source2)
            val res = (lhs | rhs).toInt
            res

          case LShift(source, steps, _) =>
            val lhs = getValue(source)
            val res = (lhs << steps).toInt
            res

          case RShift(source, steps, _) =>
            val lhs = getValue(source)
            val res = (lhs >>> steps).toInt
            res

          case AssInt(value, _) =>
            value

          case AssSymbol(source, _) =>
            val lhs = getValue(source)
            val res = lhs
            res

          case _ =>
            println("Unsupported")
            val a = 10 / 0
            0.toInt
        }
      commands += (symbol -> (Some(retVal), cmd))
      retVal
    }
  }
}



def getVal(s : String) = {
  if (numeric(s))
    s.toInt
  else
    calculated(s)
}

def calculate(cmd : Command) : Int = {
  cmd match {
    case Not(source, _) =>
      val lhs = getVal(source)
      val rhs = 65535.toInt
      val res = (lhs ^ rhs).toInt
      res

    case And(source1, source2, _) =>
      val lhs = getVal(source1)
      val rhs = getVal(source2)
      val res = (lhs & rhs).toInt
      res

    case Or(source1, source2, _) =>
      val lhs = getVal(source1)
      val rhs = getVal(source2)
      val res = (lhs | rhs).toInt
      res

    case LShift(source, steps, _) =>
      val lhs = getVal(source)
      val res = (lhs << steps).toInt
      res

    case RShift(source, steps, _) =>
      val lhs = getVal(source)
      val res = (lhs >>> steps).toInt
      res

    case AssInt(value, _) =>
      value

    case AssSymbol(source, _) =>
      val lhs = getVal(source)
      val res = lhs
      res

    case _ =>
      println("Unsupported")
      val a = 10 / 0
      0.toInt
  }
}

val sources = List("a")
// val sources = List("d", "e", "f", "g", "h", "i", "x", "y")

def convert(s : Int) = 
  if (s >= 0)
    s
  else
    65535 + s + 1

def allDefined(cmd : Command) : Boolean = {
  val sources = getSources(cmd)
  for (s <- sources) {
    if (!numeric(s) && calculated.get(s).isEmpty)
      return false
  }
  return true
}

while (calculated.get("a").isEmpty) {
  for (cmd <- cmds) {
    if (allDefined(cmd)) {
      val result = calculate(cmd)
      calculated += (getDest(cmd) -> result)
    }
  }
}


for (source <- sources) {
  val value = getValue(source)
  println(source + ": " + convert(value) + " (" + value + ")")
}

println(calculated)

println("Done!")


