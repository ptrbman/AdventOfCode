import java.security.MessageDigest
 
val digest = MessageDigest.getInstance("MD5")
 
//Quick MD5 of text
var i = 0
while (true) {
  i = i + 1
  val text = "bgvyzdsv" + i
  val beg = digest.digest(text.getBytes).map("%02x".format(_)).mkString.substring(0, 6)
  if (beg == "000000")
    println(i + " -> " + beg)
}

