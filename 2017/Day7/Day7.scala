object Day7 {

  def part1(input : List[String]) = {

    val p = "([a-z]*) \\((\\d+)\\)".r
    val p2 = "([a-z]*) \\((\\d+)\\) -> (.*)".r
    import scala.collection.mutable.Set
    var all = Set() : Set[String]
    for (str <- input) {
      val (s, l) = 
        str.trim() match {
          case p(s, d) => (s, List())
          case p2(s, d, l) => (s, l.split(",").map(_.trim()).toList)
        }
      all += s
    }

    for (str <- input) {
      val (s, l) = 
        str.trim() match {
          case p(s, d) => (s, List())
          case p2(s, d, l) => (s, l.split(",").map(_.trim()).toList)
        }
      for (i <- l)
        all -= i
    }

    all.head
  }

  def part2(input : List[String]) = {

    val p = "([a-z]*) \\((\\d+)\\)".r
    val p2 = "([a-z]*) \\((\\d+)\\) -> (.*)".r
    import scala.collection.mutable.Map
    var map = Map() : Map[String, List[String]]
    var weights = Map() : Map[String, Int]
    var weightTable = Map() : Map[String, Int]
    for (str <- input) {
      str.trim() match {
        case p(s, d) => {
          map += s -> List()
          weights += s -> d.toInt
        }
        case p2(s, d, l) => {
          map += s -> (l.split(",").map(_.trim()).toList)
          weights += s -> d.toInt
        }
      }
    }

    def weight(str : String) : Int = {
      if (!(weightTable contains str)) {
        val result =  weights(str) + map(str).map(weight).sum
        weightTable += str -> result
      }
      weightTable(str)
    }

    def isBalanced(str : String) = {
      val stacks = map(str).map(weight).toSet
      stacks.size < 2
    }

    def keyNode(str : String) : String = {
      map(str).find(!isBalanced(_)) match {
        case None => str
        case Some(s) => keyNode(s)
      }

    }

    def fixNode(str : String) = {
      val children = map(str).map(weight)
      val unique = children.find(x => children.count(_ == x) == 1).get
      val unqIdx = children indexOf unique
      val w = weights(map(str)(unqIdx))
      val other = children.find(_ != unique).get
      w - (unique-other)
    }
    fixNode(keyNode(map.keys.find(!isBalanced(_)).get))
  }
  def main(args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    println(part1(input))
    println(part2(input))    
  }
}
