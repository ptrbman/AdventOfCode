object Day12 {
  import scala.collection.mutable.Map

  type State = (Int, Int, Int, Int)
  type Program = List[String]

  def run(program : Program, initialState : State) = {
    val idxMap = Map('a' -> 0, 'b' -> 1, 'c' -> 2, 'd' -> 3)
    val copyInt = "cpy (\\d+) ([a-d])".r
    val copyReg = "cpy ([a-d]) ([a-d])".r
    val inc = "inc ([a-d])".r
    val dec = "dec ([a-d])".r
    val jnzInt = "jnz (\\d+) (\\-?)(\\d+)".r    
    val jnzReg = "jnz ([a-d]) (\\-?)(\\d+)".r

    def gs(c : String, s : State) = {
      c match {
        case "a" => s._1
        case "b" => s._2
        case "c" => s._3
        case "d" => s._4
      }
    }

    def ss(c : String, i : Int, s : State) = {
      c match {
        case "a" => (i, s._2, s._3, s._4)
        case "b" => (s._1, i, s._3, s._4)
        case "c" => (s._1, s._2, i, s._4)
        case "d" => (s._1, s._2, s._3, i)          
      }
    }

    def runProgram(program : Program, state : State, pc : Int) : State = {
      if (pc >= program.length) {
        state
      } else {
        program(pc) match {
          case copyInt(i, r) => runProgram(program, ss(r, i.toInt, state), pc+1)
          case copyReg(rs, rt) => runProgram(program, ss(rt, gs(rs, state), state), pc+1)
          case inc(r) => runProgram(program, ss(r, gs(r, state) + 1, state), pc+1)
          case dec(r) => runProgram(program, ss(r, gs(r, state) - 1, state), pc+1)
          case jnzReg(r, sign, t) => {
            val target = 
              if (sign == "-")
                -(t.toInt)
              else
                t.toInt
            if (gs(r, state) != 0)
              runProgram(program, state, pc + target)
            else
              runProgram(program, state, pc+1)
          }
          case jnzInt(i, sign, t) => {
            val target = 
              if (sign == "-")
                -(t.toInt)
              else
                t.toInt
            if (i.toInt != 0)
              runProgram(program, state, pc + target)
            else
              runProgram(program, state, pc+1)            
          }
          case _ => throw new Exception("Unknown instruction: " + program(pc))
        }
      }
    }

    val finalState = runProgram(program, initialState, 0)
    println(gs("a", finalState))
  }


  def part2(input : List[String]) = {
  }


  def main (args : Array[String]) = {
    val input = io.Source.fromFile("input.txt").getLines.toList
    val program = input
    val state1 = (0, 0, 0, 0)
    val state2 = (0, 0, 1, 0)        
    run(input, state1)
    run(input, state2)    
  }
}  
