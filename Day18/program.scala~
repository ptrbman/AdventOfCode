import scala.collection.mutable.Map

val containers = io.Source.fromFile("input.txt").getLines.toList.map(_.toInt)

println(containers)

val desiredVolume = 150

def pickAndChoose(remaining : List[Int], picked : Int) : Int = {
  if (picked == desiredVolume)
    1
  else if (picked > desiredVolume)
    0
  else if (remaining.isEmpty)
    0
  else
    pickAndChoose(remaining.tail, remaining.head + picked) + 
    pickAndChoose(remaining.tail, picked)
}

var combs = Map() : Map[Int, Int]

def pickAndChooseMin(remaining : List[Int], picked : List[Int]) : Unit = {
  if (picked.sum == desiredVolume)
    combs += (picked.length -> (1 + (combs getOrElse (picked.length, 0))))
  else if (picked.sum > desiredVolume)
    ()
  else if (remaining.isEmpty)
    ()
  else {
    pickAndChooseMin(remaining.tail, remaining.head :: picked)
    pickAndChooseMin(remaining.tail, picked)
  }
}

println("Part 1: " + pickAndChoose(containers, 0))

pickAndChooseMin(containers, List(0))

println("Part 2: "+ combs(combs.keys.min))
