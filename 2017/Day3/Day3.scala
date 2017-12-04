object Day3 {

  def ringIdx(n_ : Int) = {
    n_ match {
      case 1 => 1
      case _ => {
        var n = n_ - 1
        var i = 3
        while (n > 4*i-4) {
          n -= 4*i - 4
          i += 2
        }
        (i-1)/2
      }
    }
  }

  def ringStart(idx : Int) = {
    idx match {
      case 0 => 1
      case _ => {
        var start = 2
        for (i <- 1 until idx) {
          start += 4*(1+2*i)-4
        }
        start
      }
    }
  }

  def distToMid(i : Int, n : Int) : (Int, Int) = {
    val rIdx = n
    val start = ringStart(rIdx)
    val ringSideLen = (4*(2*rIdx+1)-4)/4
    // tl -- tr
    // |     |
    // bl -- br
    val tr = start + ringSideLen - 1
    val tl = tr + ringSideLen
    val bl = tl + ringSideLen
    val br = bl + ringSideLen

    val (beg, end) = 
      if (start <= i && i <= tr) {
        (start-1, tr)
      } else if (tr <= i && i <= tl) {
        (tr, tl)        
      } else if (tl <= i && i <= bl) {
        (tl, bl)
      } else if (bl <= i && i <= br) {
        (bl, br)
      } else {
        distToMid(i, n+1)
      }
    (beg, end)
  }


  def distToMid(i : Int) : Int = {
    val (beg, end) = distToMid(i, 1)
    val mid = (end - beg)/2
    val dist = (i - beg - mid).abs
    val rIdx = ringIdx(i) 
    dist + rIdx
  }

  def part1(n : Int) = {
    distToMid(n)
  }

  def part2(i : Int) = {
    import scala.collection.mutable.Map
    val map :Map[(Int, Int), Int] = Map()
    map += (0, 0) -> 1
    map += (1, 0) -> 1
    map += (1, 1) -> 2
    map += (0, 1) -> 4
    map += (-1, 1) -> 5
    map += (-1, 0) -> 10
    map += (-1, -1) ->11
    map += (0, -1) ->23
    map += (1, -1)->25

    var curX = 1
    var curY = -1
    // Dir
    // Up = 0
    // Left = 1
    // Down = 2
    // Right = 3

    var dir = 3
    var lastVal = 25

    def move(x : Int, y : Int, d : Int) = {
      d match {
        case 0 => (x, y+1)
        case 1 => (x-1, y)
        case 2 => (x, y-1)
        case 3 => (x+1, y)
      }
    }

    def assign(x : Int, y : Int) = {
      val sum =
        map.getOrElse((x-1, y-1), 0) +
      map.getOrElse((x, y-1), 0) +
      map.getOrElse((x+1, y-1), 0) +
        map.getOrElse((x-1, y), 0) +
      map.getOrElse((x, y), 0) +
      map.getOrElse((x+1, y), 0) +
        map.getOrElse((x-1, y+1), 0) +
      map.getOrElse((x, y+1), 0) +
      map.getOrElse((x+1, y+1), 0)
      map += (x, y) -> sum
      sum
    }

    while (lastVal < i) {
      val (nx, ny) = move(curX, curY, dir)
      lastVal = assign(nx, ny)
      val (cx, cy) = move(nx, ny, (dir + 1) % 4)
      if (!(map contains (cx, cy))) {
        dir = (dir + 1) % 4
      }
      curX = nx
      curY = ny
    }

    lastVal
  }

  def main(args : Array[String]) = {
    // val n = 300
    val n = 265149
    println(part1(n))
    println(part2(n))
  }
}
