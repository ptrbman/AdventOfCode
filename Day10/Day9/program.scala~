val lines = io.Source.fromFile("input.txt").getLines

// val sum = 
//   for (l <- lines) yield {
//     var skip = 0
//     val str =
//       for (i <- 1 until l.length - 1) yield {
//         if (skip > 0) {
//           skip = skip - 1
//           ""
//         } else if (l(i) == '\\' && l(i+1) == 'x') {
//           skip = 3
//           "X"
//         } else if (l(i) == '\\' && l(i+1) == '\\') {
//           skip = 1
//           "\\"
//         } else if (l(i) == '\\' && l(i+1) == '\"') {
//           skip = 1
//           "\""
//         } else {
//           l(i)
//         }
//       }
//     val extra = l.length - (str.foldLeft("")((a, b) => a + b)).length
//     extra
//   }

// val total = sum.foldLeft(0)(_ + _)
// println(total)

// Just count the number of extra characters

val extra = 
  for (l <- lines) yield {
    val ex =
      for (c <- l) yield {
        if (c == '\"')
          1
        else if (c == '\\')
          1
        else
          0
      }
    ex.foldLeft(0)(_ + _) + 2
  }

println(extra.foldLeft(0)(_ + _))




