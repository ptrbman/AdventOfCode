import scala.collection.mutable.Set

val line = io.Source.fromFile("input.txt")

var santax = 0
var santay = 0

var robox = 0
var roboy = 0

var santaMove = true

val visited = Set((0,0))

for (c <- line) {
    if (santaMove) {
      if (c == '<')
        santax = santax - 1
      else if (c == '>')
        santax = santax + 1
      else if (c == '^')
        santay = santay - 1
      else if (c == 'v')
        santay = santay + 1
      else
	println("Visited: " + visited.size)
      visited.add((santax,santay))
    } else {
      if (c == '<')
        robox = robox - 1
      else if (c == '>')
        robox = robox + 1
      else if (c == '^')
        roboy = roboy - 1
      else if (c == 'v')
        roboy = roboy + 1
      else
	println("Visited: " + visited.size)
      visited.add((robox,roboy))
    }
  santaMove = !santaMove
}



