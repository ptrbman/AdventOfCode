import scala.collection.mutable.Set

val limit = 29000000

// Find the number with sum of its divisors to be greater than limit


def divisors(n : Int) : List[Int] = {
  (for (i <- 1 to n; if (n % i == 0)) yield i).toList
}

def magic(n : Int) : Int = {
  var sum = 0
  for (i <- 1 to Math.sqrt(n).toInt) {
    if (n % i == 0) {
      sum += i*10
      if (i*i != n) {
        sum += (n/i)*10
      }
    }
  }
  sum
}

def magic2(n : Int) : Int = {
  var sum = 0
  for (i <- 1 to Math.sqrt(n).toInt) {
    if (n % i == 0) {
      if (n / i <= 50)
        sum += i*11

      if (i*i != n) {
        if (n / (n/i) <= 50)
          sum += (n/i)*11
      }
    }
  }
  sum
}


// var i = 687960
var i = 1
var max = 1
while (max <= limit) {
  val m = magic2(i)
  if (m > max) {
    max = m
    println("MAX: " + max + " (" + i + ")")
  }
  i += 1
}

println(i)
