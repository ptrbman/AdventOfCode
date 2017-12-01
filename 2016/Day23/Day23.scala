object Day23 {
  import scala.collection.mutable.Map

  type State = (Int, Int, Int, Int)
  type Program = List[String]

  def run(program : Program, initialState : State) = {
    val idxMap = Map('a' -> 0, 'b' -> 1, 'c' -> 2, 'd' -> 3)
    val copyInt = "cpy (\\-?\\d+) ([a-d])".r
    val copyReg = "cpy ([a-d]) ([a-d])".r
    val inc = "inc ([a-d])".r
    val dec = "dec ([a-d])".r
    val jnzInt = "jnz (\\d+) (\\-?)(\\d+)".r
    val jnzReg = "jnz ([a-d]) (\\-?)(\\d+)".r
    val jnzIntReg = "jnz (\\d+) ([a-d])".r
    val tgl = "tgl ([a-d])".r

    val mul = "mul ([a-d]) ([a-d]) ([a-d])".r
    val add = "add ([a-d]) ([a-d])".r

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

    def toggle(program : Program, idx : Int) = {
      if (idx < 0 || idx >= program.length) {
        program
      } else {
        val newInstruction =
          program(idx) match {
            case copyInt(i, r) => "jnz " + i + " " + r
            case copyReg(rs, rt) => "jnz " + rs + " " + rt
            case inc(r) => "dec " + r
            case dec(r) => "inc " + r
            case jnzReg(r, sign, t) => "cpy " + r  + " " + sign + t
            case jnzInt(i, sign, t) => "cpy " + i  + " " + sign + t
            case jnzIntReg(int, reg) => "cpy " + int + " " + reg
            case tgl(r) => "inc " + r
          }
        program.take(idx) ++ (newInstruction :: program.drop(idx+1))
      }
    }

    def runProgram(program : Program, state : State, pc : Int) : State = {
      if (pc >= program.length) {
        state
      } else {
        println(pc + " || " + program(pc) + " || " + state)
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
          case jnzIntReg(i, reg) =>
            if (i.toInt == 0)
              runProgram(program, state, pc+1)
            else
              runProgram(program, state, pc+gs(reg, state))

          case tgl(r) => runProgram(toggle(program, pc+gs(r, state)), state, pc+1)

          case mul(r1, r2, t) => runProgram(program, ss(t, gs(r1, state)*gs(r2, state), state), pc+1)
          case add(r, t) => runProgram(program, ss(t, gs(r, state)+gs(t, state), state), pc+1)

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
    val input = io.Source.fromFile("myinput.txt").getLines.toList
    val program = input
    for (i <- 6 until 7) {
      val state = (i, 0, 0, 0)
      println(state)
      run(input, state)
    }
  }
}  
