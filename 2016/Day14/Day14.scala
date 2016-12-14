object Day14 {

  import java.security.MessageDigest
  import scala.collection.mutable.Map
    import scala.collection.mutable.ListBuffer  

  def md5(s: String) : String= {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }

  def md5rep(s: String) : String= {
    var answer = s
    for (i <- 0 until 2017) {
      answer = MessageDigest.getInstance("MD5").digest(answer.getBytes).map("%02X".format(_)).mkString.toLowerCase
    }
    answer
  }  

  def part12(input : String, hash : String => String) = {
    val str = input
    var idx = 0
    var nextKey = 1

    val candidates = Map() : Map[Char, List[Int]] // Number -> Index

    def findKeys(idx : Int, c : Char) = {
      if (candidates contains c) {
        val newKeys = candidates(c).filter(i => i != idx && i >= idx - 1000)
        candidates += (c -> candidates(c).filter(_ == idx))
        newKeys.reverse
      } else {
        List()
      }
    }

    def addCandidate(idx : Int, c : Char) = {
      val prevCand = candidates.getOrElse(c, List() : List[Int])
      if (prevCand.isEmpty || prevCand.head != idx)
        candidates += (c -> (idx :: prevCand))
    }



    var extraCheck = 1000
    var keys = ListBuffer() : ListBuffer[Int]
    while (extraCheck >= 0) {
      val trystr = str + idx.toString
      var md5str = hash(trystr)
      var curChar = md5str.head
      var ccount = 1
      var keyAdded = false
      md5str = md5str.tail

      while (md5str != "") {
        val nextC = md5str.head
        md5str = md5str.tail
        if (nextC == curChar) {
          ccount += 1
        } else {
          ccount = 1
          curChar = nextC
        }

        if (ccount == 3) {
          if (!keyAdded) {
            addCandidate(idx, curChar)
            keyAdded = true
          }
        }

        if (ccount == 5) {
          for (k <- findKeys(idx, curChar)) {
            keys += k
            nextKey += 1
          }
        }
      }
      idx = idx + 1
      if (nextKey >= 65)
        extraCheck -= 1
    }
    val sortedKeys = keys.sorted.toList
    println(sortedKeys(63))
  }

  def main (args : Array[String]) = {
    val input = "ihaygndm"
    // val simple = "abc" 
    part12(input, md5)
    part12(input, md5rep)    
  }
}  
