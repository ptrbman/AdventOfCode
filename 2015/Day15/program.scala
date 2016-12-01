import scala.collection.mutable.Map
import scala.collection.mutable.Set

val input = io.Source.fromFile("input.txt").getLines

val pattern = "([A-Za-z]+): capacity (-?[0-9]+), durability (-?[0-9]+), flavor (-?[0-9]+), texture (-?[0-9]+), calories (-?[0-9]+)".r

class Ingredient(val name : String, val capacity : Int, val durability : Int, val flavor : Int, val texture : Int, val calories : Int) {}

val ingredients = 
(for (l <- input) yield {
  val pattern(name, capacity, durability, flavor, texture, calories) = l
  new Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
}).toList

val divs = ingredients.length

def evaluateDiv(aux : List[Int]) = {
  var cur = 0
  var cap = 0
  var dur = 0
  var fla = 0
  var tex = 0
  var cal = 0
  for ((i, div) <- (ingredients zip aux)) {
    val qty = div - cur
    cap += i.capacity*qty
    dur += i.durability*qty
    fla += i.flavor*qty
    tex += i.texture*qty
    cal += i.calories*qty
    cur = div
  }

  // println(aux)
  // println("\tCapacity: " + cap)
  // println("\tDurability: " + dur)
  // println("\tFlavor: " + fla)
  // println("\tTexture: " + tex)
  if (cal != 500 || cap <= 0 || dur <= 0 || fla <= 0 || tex <= 0)
    0
  else
    cap*dur*fla*tex
}


var max = 0
def makeDiv(count : Int, aux : List[Int]) : Unit = {
  if (count == 0) {
    val res = evaluateDiv((100 :: aux).reverse)
    if (res > max)
      max = res
  } else {
    val start = if (aux.isEmpty) 0 else aux.head
    for (i <- start until 100) {
      makeDiv(count-1, i :: aux)
    }
  }
}


makeDiv(3, List())
println("Max: " + max)
