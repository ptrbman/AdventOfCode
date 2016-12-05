object Day5 {

  import java.security.MessageDigest

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  def part1(input : List[String]) = {
    val str = input.head
    var idx = 0
    var remain = 8
    var finalStr = ""
    while (remain > 0) {
      val trystr = str + idx.toString
      val md5str = md5(trystr).map("%02X".format(_)).mkString
      if (md5str.prefixLength(_ == '0') >= 5) {
        finalStr = finalStr + md5str(5)
        remain = remain - 1
      } else {
      }
      idx = idx + 1
    }
    println(finalStr.toLowerCase)
  }


  def part2(input : List[String]) = {
    val str = input.head
    var idx = 0
    var remain = 8
    var finalStr = "-0--1--2--3--4--5--6--7-"
    while (remain > 0) {
      val trystr = str + idx.toString
      val md5str = md5(trystr).map("%02X".format(_)).mkString
      if (md5str.prefixLength(_ == '0') >= 5) {
        val i = md5str(5).toString
        if (finalStr contains ("-" + i + "-")) {
          finalStr = finalStr.replace("-" + i + "-", md5str(6).toString)
          remain = remain - 1
          println(finalStr)
        }
      }
      idx = idx + 1
    }
    println(finalStr.toLowerCase)
  }


  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part1(input)
    part2(input)    
  }
}  
