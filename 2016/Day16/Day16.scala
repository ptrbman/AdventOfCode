object Day16 {
  // export JAVA_OPTS="-Xmx512M -Xms32M -Xprof"

  def part1(input : String, size : Int) = {
    def genData(str : String) : String = {
      if (str.length >= size)
        str.take(size)
      else
        genData(str + "0" + str.reverse.map(_ match { case '1' => '0'; case '0' => '1' }))
    }

    def checksum(str : String) : String = {
      if (str.length % 2 == 1) {
        str
      } else {
        val newStr = 
          for (i <- 0 until str.length by 2) yield {
            if (str(i) == str(i+1)) '1'
            else '0'
          }
        checksum(newStr.mkString(""))
      }
    }

    println(checksum(genData(input)))
  }


  def main (args : Array[String]) = {
    val input = "ihaygndm"
    part1("11100010111110100", 272)
    part1("11100010111110100", 35651584)
  }
}  
