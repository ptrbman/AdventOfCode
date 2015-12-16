import scala.collection.mutable.Map
import scala.collection.mutable.Set

val input = io.Source.fromFile("input.txt").getLines


val childrenp = ".*children: ([0-9]+).*".r
val catsp = ".*cats: ([0-9]+).*".r
val samoyedsp = ".*samoyeds: ([0-9]+).*".r
val pomeraniansp = ".*pomeranians: ([0-9]+).*".r
val akitasp = ".*akitas: ([0-9]+).*".r
val vizslasp = ".*vizslas: ([0-9]+).*".r
val goldfishp = ".*goldfish: ([0-9]+).*".r
val treesp = ".*trees: ([0-9]+).*".r
val carsp = ".*cars: ([0-9]+).*".r
val perfumesp = ".*perfumes: ([0-9]+).*".r



// for (l <- input) {
//   // println(l)
//   var works = true
//   if (l contains "children") {
//     val childrenp(count) = l
//     if (count.toInt != 3) {
//       works = false
//       // println("\tCHILDREN(" + count + ")")
//     }
//   }
//   if (l contains "cats") {
//     val catsp(count) = l
//     if (count.toInt != 7) {
//       works = false
//       // println("\tCATS")
//     }
//   }
//   if (l contains "samoyeds") {
//     val samoyedsp(count) = l
//     if (count.toInt != 2) {
//       works = false
//       // println("\tSAMOYEDS")
//     }
//   }
//   if (l contains "pomeranians") {
//     val pomeraniansp(count) = l
//     if (count.toInt != 3) {
//       works = false
//       // println("\tPOMERIANS")
//     }
//   }
//   if (l contains "akitas") {
//     val akitasp(count) = l
//     if (count.toInt != 0) {
//       works = false
//       // println("\tAKITAS")
//     }
//   }
//   if (l contains "vizslas") {
//     val vizslasp(count) = l
//     if (count.toInt != 0) {
//       works = false
//       // println("\tVIZLAS")
//     }
//   }
//   if (l contains "goldfish") {
//     val goldfishp(count) = l
//     if (count.toInt != 5) {
//       works = false
//       // println("\tGOLDFISH")
//     }
//   }
//   if (l contains "trees") {
//     val treesp(count) = l
//     if (count.toInt != 3) {
//       works = false
//       // println("\tTREES")
//     }
//   }
//   if (l contains "cars") {
//     val carsp(count) = l
//     if (count.toInt != 2) {
//       works = false
//       // println("\tCARS")
//     }
//   }
//   if (l contains "perfumes") {
//     val perfumesp(count) = l
//     if (count.toInt != 1) {
//       works = false
//       // println("\tPERFUMES")
//     }
//   }
//   if (works)
//     println(l)
//     // println("\tWORKS!")
// }


for (l <- input) {
  // println(l)
  var works = true
  if (l contains "children") {
    val childrenp(count) = l
    if (count.toInt != 3) {
      works = false
    }
  }
  if (l contains "cats") {
    val catsp(count) = l
    if (count.toInt <= 7) {
      works = false
    }
  }
  if (l contains "samoyeds") {
    val samoyedsp(count) = l
    if (count.toInt != 2) {
      works = false
    }
  }
  if (l contains "pomeranians") {
    val pomeraniansp(count) = l
    if (count.toInt >= 3) {
      works = false
    }
  }
  if (l contains "akitas") {
    val akitasp(count) = l
    if (count.toInt != 0) {
      works = false
    }
  }
  if (l contains "vizslas") {
    val vizslasp(count) = l
    if (count.toInt != 0) {
      works = false
    }
  }
  if (l contains "goldfish") {
    val goldfishp(count) = l
    if (count.toInt >= 5) {
      works = false
    }
  }
  if (l contains "trees") {
    val treesp(count) = l
    if (count.toInt <= 3) {
      works = false
    }
  }
  if (l contains "cars") {
    val carsp(count) = l
    if (count.toInt != 2) {
      works = false
    }
  }
  if (l contains "perfumes") {
    val perfumesp(count) = l
    if (count.toInt != 1) {
      works = false
    }
  }
  if (works)
    println(l)
}
