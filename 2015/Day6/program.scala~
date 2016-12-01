val lines = io.Source.fromFile("input.txt").getLines

var totalPaper = 0
var totalRibbon = 0

def isNice(s : String) = {
  var vowelCount = 0
  var double = false
  var nice = true
  for (i <- 0 until s.length) {
    if (s(i) == 'a' || s(i) == 'e' || s(i) == 'i' || s(i) == 'o' || s(i) == 'u')
      vowelCount = vowelCount + 1
    if (i > 0 && s(i) == s(i-1))
      double = true
    if (i > 0) {
      if (s(i-1) == 'a' && s(i) == 'b')
        nice = false
      if (s(i-1) == 'c' && s(i) == 'd')
        nice = false
      if (s(i-1) == 'p' && s(i) == 'q')
        nice = false
      if (s(i-1) == 'x' && s(i) == 'y')
        nice = false
    }
  }

  if (vowelCount < 3 || double == false)
    nice = false
  nice
}


def isNicer(s : String) = {
  var doublePair = false
  var repeat = false

  for (i <- 0 until s.length) {
    for (j <- i+2 until (s.length-1)) {
      if (s(i) == s(j) && s(i+1) == s(j+1))
        doublePair = true
    }

    if (i+2 < s.length && s(i) == s(i+2))
      repeat = true
  }
  doublePair && repeat
}


var nice = 0
for (l <- lines) {
  if (isNicer(l))
    nice = nice + 1
}

println("Nice: " + nice)

