object Day10 {
  import scala.collection.mutable.Map

  def part12(input : List[String]) = {
    val valuePattern = "value (\\d+) goes to bot (\\d+)".r
    val givesPattern = "bot (\\d+) gives low to bot (\\d+) and high to bot (\\d+)".r
    val firstOutputPattern = "bot (\\d+) gives low to output (\\d+) and high to bot (\\d+)".r
    val lastOutputPattern = "bot (\\d+) gives low to bot (\\d+) and high to output (\\d+)".r
    val bothOutputPattern = "bot (\\d+) gives low to output (\\d+) and high to output (\\d+)".r            
    val giveMap = Map() : Map[Int, ((Boolean, Int), (Boolean, Int))]
    val valueMap = Map() : Map[Int, List[Int]]
    val outputMap = Map() : Map[Int, List[Int]]
    for (str <- input) {
      str match {
        case valuePattern(value, bot) => {
          valueMap += bot.toInt -> (value.toInt :: (valueMap.getOrElse(bot.toInt, List())))
        }
        case givesPattern(bot, low, high) => {
          giveMap += bot.toInt -> ((false, low.toInt), (false, high.toInt))
        }
        case firstOutputPattern(bot, low, high) => {
          giveMap += bot.toInt -> ((true, low.toInt), (false, high.toInt))
        }
        case lastOutputPattern(bot, low, high) => {
          giveMap += bot.toInt -> ((false, low.toInt), (true, high.toInt))
        }
        case bothOutputPattern(bot, low, high) => {
          giveMap += bot.toInt -> ((true, low.toInt), (true, high.toInt))
        }                              
        case str => {
          throw new Exception("Couldn't match: |" + str + "|")
        }
      }
    }

    def giveBot(b : Int, v : Int) = {
      valueMap += b.toInt -> (v.toInt :: (valueMap.getOrElse(b.toInt, List())))
    }

    def giveOutput(o : Int, v : Int) = {
      outputMap += o.toInt -> (v.toInt :: (valueMap.getOrElse(o.toInt, List())))
    }


    def take(b : Int) = {
      valueMap += b -> List()
    }

    def give(d : (Boolean, Int), v : Int) = {
      val (output, i) = d
      if (output) {
        giveOutput(i, v)
      } else {
        giveBot(i, v)
      }
    }

    def doubleBot() = {
      valueMap.find{
        x => {
          val (k, v) = x
          v.length == 2
        }
      }
    }

    while (!doubleBot().isEmpty) {
      val bot = doubleBot().get
      val b = bot._1
      val val1 = bot._2(0) min bot._2(1)
      val val2 = bot._2(0) max bot._2(1)
      if ((val1, val2) == (17, 61))
        println(b)
      val dest1 = giveMap(b)._1
      val dest2 = giveMap(b)._2
      give(dest1, val1)
      give(dest2, val2)
      take(b)
    }

    println(outputMap(0).head * outputMap(1).head * outputMap(2).head)

  }

  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    part12(input)
  }
}  
