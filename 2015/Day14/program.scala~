import scala.collection.mutable.Map
import scala.collection.mutable.Set

val input = io.Source.fromFile("input.txt").getLines

val gainpattern = "([A-Za-z]+) would gain ([0-9]+) happiness units by sitting next to ([A-Za-z]+).".r
val losepattern = "([A-Za-z]+) would lose ([0-9]+) happiness units by sitting next to ([A-Za-z]+).".r

val seatings = Map() : Map[(String, String), Int]
val names = Set() : Set[String]

for (l <- input) {
  if (l.contains("gain")) {
    val gainpattern(name1, happiness, name2) = l
    seatings += ((name1, name2) -> happiness.toInt)
    names += name1
    names += name2
  }

  if (l.contains("lose")) {
    val losepattern(name1, happiness, name2) = l
    seatings += ((name1, name2) -> -happiness.toInt)
    names += name1
    names += name2
  }
}

def permutations(l : List[String]) : List[List[String]] = {
  if (l.length == 1) {
    List(l)
  } else {
    val rest = permutations(l.tail)
    (for (r <- rest) yield {
      (for (i <- 0 to r.length) yield (r.take(i) ++ List(l.head) ++ r.drop(i)))
    }).flatten
  }
}

def score(list : List[String]) = {
  var score = 0
  for (i <- 0 until list.length) {
    val ii = (i + 1) % list.length
    val str1 = list(i)
    val str2 = list(ii)
    score += seatings((str1, str2))
    score += seatings((str2, str1))
  }
  score
}

val scores = 
  for (p <- permutations(names.toList)) yield {
    score(p)
  }

println(scores.max)

for (n <- names) {
  seatings += (("me", n) -> 0)
  seatings += ((n, "me") -> 0)
  names += "me"
}


val newScores = 
  for (p <- permutations(names.toList)) yield {
    score(p)
  }

println(newScores.max)




