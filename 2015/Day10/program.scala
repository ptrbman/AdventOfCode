import scala.collection.mutable.ListBuffer

def iteration(str : List[Int]) = {
  val result : ListBuffer[Int] = ListBuffer()

  var lastInt = str.head
  var length = 1
  for (i <- str.tail) {
    if (i == lastInt) {
      length = length + 1
    } else {
      result += length
      result += lastInt
      lastInt = i
      length = 1
    }
  }

  result += length
  result += lastInt
  result.toList
}

var list = List(1, 1, 1, 3, 1, 2, 2, 1, 1, 3)
println("Start: " + list)

for (i <- 0 until 50) {
  list = iteration(list)
  println(i + ": " + list.length)
}


