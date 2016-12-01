import scala.collection.mutable.Map
import scala.collection.mutable.Set

val input = io.Source.fromFile("input.txt").getLines

val pattern = "([A-Za-z]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds.".r

val dists = Map() : Map[(String, Int), Int]

def calcDist(totalTime : Int, speed : Int, duration : Int, rest : Int) : Int = {
  var time = totalTime
  var dist = 0
  while (time > 0) {
    if (duration > time) {
      dist += speed*time
      time = 0
    } else {
      dist += duration*speed
      time -= (duration + rest)
    }
  }
  dist
}

// println((for (l <- input) yield {
//   val pattern(name, speed, duration, rest) = l
//   println(name + ", " + speed + ", " + duration + ", " + rest)
//   calcDist(2503, speed.toInt, duration.toInt, rest.toInt)
// }).max)

val names = Set() : Set[String]

for (l <- input) {
  val pattern(name, speed, duration, rest) = l
  for (i <- 0 to 2503) {
    dists += ((name, i) -> calcDist(i, speed.toInt, duration.toInt, rest.toInt))
  }
  names += name
}

val nameList = names.toList

val scores = nameList.map(x => 0).toArray


for (i <- 1 to 2503) {
  val maxDist = (for (j <- 0 until nameList.length) yield (dists(nameList(j), i))).max
  for (j <- 0 until nameList.length; if (dists(nameList(j), i) == maxDist)) scores(j) += 1
}

println(scores.mkString(", "))








