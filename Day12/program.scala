import scala.util.parsing.json._

val input = io.Source.fromFile("input.txt").mkString


val result = JSON.parseFull(input).get

def parseList(list : List[Any]) : List[Double] = {
  val res : List[List[Double]] = 
    for (l <- list) yield {
      l match {
        case str : String => List() : List[Double]
        case d : Double => List(d) : List[Double]
        case l : List[Any] => parseList(l) : List[Double]
        case m : Map[Any, Any] => parseMap(m) : List[Double]
        case asd =>  {
          println(asd.getClass)
          println(10/0)
          List(0.0)
        }
      }
    }
  res.flatten
}

def parseMap(m : Map[Any, Any]) : List[Double] = {
  val res = 
    for (k <- m.keys) yield {
      m(k) match {
        case asd : List[Any] => parseList(asd)
        case d : Double => List(d)
        case s : String => List()
        // For second step: case s : String => if (s == "red") return List() else List()
        case m : Map[Any, Any] => parseMap(m) : List[Double]
        case dsa => {
          println(dsa.getClass)
          println(10/0)
          List(0.0)
        }
      }
    }
  res.toList.flatten
}

result match {
  case m : Map[Any, Any] => println(parseMap(m).sum)
  case asd => println(asd.getClass)
}
