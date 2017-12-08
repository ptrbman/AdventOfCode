object Day8 {

  def part12(input : List[String]) = {
    import scala.collection.mutable.Map
    val regs = Map() : Map[String, Int]
    def gr(str : String) =
      regs getOrElse (str, 0)

    def sr(str : String, i : Int) =
      regs += str -> i

    val maxReg = 
      (for (l <- input) yield {
        val pattern = "([a-z]+) (inc|dec) ([-]?\\d+) if (.*)".r
        val p = "([a-z]+) (inc|dec).*".r
        l match {
          case pattern(r, incdec, offset, cond) => {
            val gtP = "([a-z]+) > ([-]?\\d+)".r
            val geqP = "([a-z]+) >= ([-]?\\d+)".r
            val ltP = "([a-z]+) < ([-]?\\d+)".r
            val leqP = "([a-z]+) <= ([-]?\\d+)".r
            val eqP = "([a-z]+) == ([-]?\\d+)".r
            val neqP = "([a-z]+) != ([-]?\\d+)".r
            val doIt =
              cond match {
                case gtP(r2, v) => gr(r2) > v.toInt
                case geqP(r2, v) => gr(r2) >= v.toInt
                case ltP(r2, v) => gr(r2) < v.toInt
                case leqP(r2, v) => gr(r2) <= v.toInt
                case eqP(r2, v) => gr(r2) == v.toInt
                case neqP(r2, v) => gr(r2) != v.toInt
              }
            if (doIt) {
              incdec match {
                case "inc" => sr(r, gr(r) + offset.toInt)
                case "dec" => sr(r, gr(r) - offset.toInt)
              }
            }
          }
        }
        regs.maxBy(_._2)._2
      }).max

    val finalMaxReg = regs.maxBy(_._2)._2
    (finalMaxReg, maxReg)
  }

  def main(args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    println(part12(input))
  }
}
