import scala.collection.mutable.Set

val lines = io.Source.fromFile("input.txt").getLines.toList

var rules = List() : List[(String, String)]
val words = Set() : Set[String]
 val word = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"

for (l <- lines) {
  val split = l.split(" ")
  rules = (split(0), split(2)) :: rules
}

// for ((w, r) <- rules) {
//   var idx = word.indexOf(w, 0)
//   while (idx >= 0) {
//     val newString = word.substring(0, idx) ++ r ++ word.substring(idx+w.length, word.length)
//     words += newString
//     idx = word.indexOf(w, idx+1)
//   }
// }

var steps = 0
var oldWords = Set(word)
var newWords = Set() : Set[String]

val sortRules = rules.sortBy(-_._2.length)
println(sortRules.mkString("\n"))

var str = word
var ruleIdx = 0

while (str != "e") {
  println("Str: " + str)
  println("IDX: " + ruleIdx)
  val idx = str.indexOf(sortRules(ruleIdx)._2, 0)
  if (idx >= 0) {
    str = str.substring(0, idx) ++ sortRules(ruleIdx)._1 ++ str.substring(idx+sortRules(ruleIdx)._2.length, str.length)
    ruleIdx = 0
    steps += 1
  } else {
    ruleIdx += 1
  }
}

println("Steps: " + steps)

// while (!(oldWords contains "e")) {
//   newWords = Set() : Set[String]
//   println(steps + ": " + oldWords.size)
//   steps += 1
//   // Add all words
//   for (curWord <- oldWords) {
//     for ((r, w) <- rules) {
//       var idx = curWord.indexOf(w, 0)
//       while (idx >= 0) {
//         val newString = curWord.substring(0, idx) ++ r ++ curWord.substring(idx+w.length, curWord.length)
//         newWords += newString
//         idx = curWord.indexOf(w, idx+1)
//       }
//     }
//   }
//   val minSize = newWords.map(_.length).min
//   println("minSize: " + minSize)
//   oldWords = for (w <- newWords; if (w.length == minSize)) yield w

// }

// println(steps)
