val lines = io.Source.fromFile("input.txt").getLines

import scala.collection.mutable.Map
import scala.collection.mutable.Set

val distances : Map[(String, String), Int] = Map()
var towns : Set[String] = Set()

for (l <- lines) {
  val splitted = l.split(" ")
  val town1 = splitted(0)
  val town2 = splitted(2)
  val dist = splitted(4).toInt
  distances += ((town1, town2) -> dist)
  distances += ((town2, town1) -> dist)
  towns += town1
  towns += town2
}

println(distances.mkString("\n"))


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


val perms = permutations(towns.toList)
def pathLength(path : List[String]) : Int = {
  if (path.length == 1)
    0
  else
    distances((path(0), path(1))) + pathLength(path.tail)
}

for (p <- perms; if (pathLength(p) == 141)) 
  println(p.mkString("->"))


// var town1 = towns.head
// var town2 = town1
// var totalPath = 0
// towns -= town1

// while (towns.size > 0) {
//   // Find closest town to *either* town1 or town2
//   var closestDist = Int.MaxValue
//   var town1Closest = true
//   var closestTown = ""
//   for (t <- towns) {
//     if (distances((t, town1)) < closestDist) {
//       closestDist = distances((t, town1))
//       town1Closest = true
//       closestTown = t
//     } else if (distances((t, town2)) < closestDist) {
//       closestDist = distances((t, town2))
//       town1Closest = false
//       closestTown = t
//     }
//   }
//   towns -= closestTown
//   totalPath += closestDist
//   if (town1Closest) {
//     println(town1 + " -> " + closestTown)
//     town1 = closestTown
//   } else {
//     println(town2 + " -> " + closestTown)
//     town2 = closestTown
//   }
// }
// println(totalPath)

