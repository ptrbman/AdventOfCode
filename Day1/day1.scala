val lines = io.Source.fromFile("input.txt").mkString
var curFloor = 0
var count = 0

for (c <- lines) {
    count = count + 1
    if (c == '(') {
       curFloor = curFloor + 1
    } else if (c == ')') {
       curFloor = curFloor - 1
    } else {
      println("ERROR")
    }
    if (curFloor < 0)
        println(count)
}

println(curFloor)
