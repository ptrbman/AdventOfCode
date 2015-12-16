// Store string in reverse

val chars = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
val MAX = chars.length - 1

val str = List(7, 6, 5, 4, 3, 2, 1, 0)

def inc(l : List[Int]) : List[Int] =
  if (l.head == MAX)
    0 :: (inc(l.tail))
  else
    (l.head + 1) :: l.tail

def cond1(l : List[Int]) : Boolean = {
  var streak = 1
  var lastChar = l.head
  for (i <- l) {
    if (i + 1 == lastChar) {
      streak = streak + 1
      if (streak == 3)
        return true
    } else {
      streak = 1
    }
    lastChar = i
  }
  return false
}

def cond2(l : List[Int]) : Boolean = {
  for (i <- l) {
    if (i == 8 || i == 14 || i == 11)
      return false
  }
  return true
}

def cond3(l : List[Int]) : Boolean = {
  var lastPair = -1
  var lastChar = l.head
  var pairs = 0
  for (i <- l.tail) {
    if (i == lastChar && i != lastPair) {
      pairs = pairs + 1
      if (pairs == 2)
        return true
      lastPair = i
    } else {
      lastPair = -1
    }
    lastChar = i
  }
  return false
}


val tests = List("hijklmmn", "abbceffg", "abbcegjk", "abb")

// for (t <- tests) {
//   val test = t.map(c => chars.findIndexOf(_ == c)).toList.reverse
//   println(test)
//   println("\tCond1: " + cond1(test))
//   println("\tCond2: " + cond2(test))
//   println("\tCond3: " + cond3(test))
// }

var input = inc("vzbxxyzz".map(c => chars.findIndexOf(_ == c)).toList.reverse)

while (!cond1(input) || !cond2(input) || !cond3(input)) {
  input = inc(input)
}

println(input.reverse.map(chars(_)).mkString(""))




