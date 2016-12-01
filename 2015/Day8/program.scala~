val lines = io.Source.fromFile("input.txt").getLines

var lights = Array.ofDim[Int](1000,1000)

def toggle(x1 : Int, y1 : Int, x2 : Int, y2 : Int) = {
  for (x <- x1 to x2; y <- y1 to y2) {
    lights(x)(y) = lights(x)(y) + 2
  }
}

def turnOn(x1 : Int, y1 : Int, x2 : Int, y2 : Int) = {
  for (x <- x1 to x2; y <- y1 to y2) {
    lights(x)(y) = lights(x)(y) + 1
  }
}

def turnOff(x1 : Int, y1 : Int, x2 : Int, y2 : Int) = {
  for (x <- x1 to x2; y <- y1 to y2) {
    if (lights(x)(y) > 0)
      lights(x)(y) = lights(x)(y) - 1
  }
}


for (l <- lines) {
  val splitted = l.split(" ")
  if (splitted(0) == "toggle")
    toggle(splitted(1).split(",")(0).toInt, splitted(1).split(",")(1).toInt, 
           splitted(3).split(",")(0).toInt, splitted(3).split(",")(1).toInt)
  else if (splitted(1) == "on")
    turnOn(splitted(2).split(",")(0).toInt, splitted(2).split(",")(1).toInt, 
           splitted(4).split(",")(0).toInt, splitted(4).split(",")(1).toInt)
  else if (splitted(1) == "off")
    turnOff(splitted(2).split(",")(0).toInt, splitted(2).split(",")(1).toInt, 
            splitted(4).split(",")(0).toInt, splitted(4).split(",")(1).toInt)

}


val count = lights.flatten.foldRight(0)(_ + _)

println(count)



