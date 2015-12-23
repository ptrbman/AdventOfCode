import scala.collection.mutable.Map

val lines = io.Source.fromFile("input.txt").getLines.toList

val lights = Array.ofDim[Boolean](100,100)
val lights2 = Array.ofDim[Boolean](100,100)

var useLights = true

for (i <- 0 until 100) {
  for (j <- 0 until 100) {
    if (lines(i)(j) == '#')
      lights(i)(j) = true
    else if (lines(i)(j) == '.')
      lights(i)(j) = false
    else
      println("ERROR: " + lines(i)(j))
  }
}


def printLights() = {
  if (useLights)
    println(lights.map(_.map(if (_) '#' else '.').mkString("")).mkString("\n"))
  else
    println(lights2.map(_.mkString("")).mkString("\n"))
}

def getXY(x : Int, y : Int) = {
  if (x == 0 && y == 0)
    true
  else if (x == 0 && y == 99)
    true
  else if (x == 99 && y == 99)
    true
  else if (x == 99 && y == 0)
    true
  else if (x < 0 || x >= 100 || y < 0 || y >= 100)
    false
  else if (useLights)
    lights(x)(y)
  else
    lights2(x)(y)
}

def countLights(x : Int, y : Int) = {
  List(getXY(x-1, y-1),
  getXY(x-1, y),
  getXY(x-1, y+1),
  getXY(x, y-1),
  getXY(x, y+1),
  getXY(x+1, y-1),
  getXY(x+1, y),
  getXY(x+1, y+1)).map(if (_) 1 else 0).sum
}

def setXY(x : Int, y : Int, v : Boolean) = {
  if (useLights)
    lights2(x)(y) = v
  else
    lights(x)(y) = v
}

def step() : Unit = {
  for (i <- 0 until 100) {
    for (j <- 0 until 100) {
      val cl = countLights(i, j)
      if (getXY(i, j)) {
        if (cl == 2 || cl == 3)
          setXY(i, j, true)
        else
          setXY(i, j, false)
      } else {
        if (cl == 3)
          setXY(i, j, true)
        else
          setXY(i, j, false)
      }
    }
  }
  if (useLights) 
    useLights = false
  else
    useLights = true
}

printLights()

for (i <- 0 until 100)
  step()


var count = 0

for (i <- 0 until 100) {
  for (j <- 0 until 100) {
    if (i == 0 && j == 0)
      count += 1
    else if (i == 0 && j == 99)
      count += 1
    else if (i == 99 && j == 99)
      count += 1
    else if (i == 99 && j == 0)
      count += 1
    else if (useLights && lights(i)(j))
      count += 1
    else if (!useLights && lights2(i)(j))
      count += 1
  }
}

println("Light count: " + count)

 
