import scala.collection.mutable.Set

val program = io.Source.fromFile("input.txt").getLines.toArray

println(program.mkString("\n"))

var rega = 1 : BigInt
var regb = 0 : BigInt
var pc = 0

def hlf(r : String) = {
  pc += 1
  // println("HLF")
  if (r == "a")
    rega /= 2
  else if (r == "b")
    regb /= 2
  // else
  //   println("\thlf ERROR")
}

def inc(r : String) {
  pc += 1
  // println("INC")
  if (r == "a")
    rega += 1
  else if (r == "b")
    regb += 1
}

def tpl(r : String) {
  pc += 1
  // println("TPL")
  if (r == "a")
    rega *= 3
  else if (r == "b")
    regb *= 3
}

def jmp(t : String) {
  pc += t.toInt
  // println("JMP")
}

def jie(r : String, t : String) {
  // println("JIE")
  if (r == "a" && rega % 2 == 0)
    pc += t.toInt
  else if (r == "b" && regb % 2 == 0)
    pc += t.toInt
  else 
    pc += 1
}

def jio(r : String, t : String) {
  // println("JIO")
  if (r == "a" && rega == 1)
    pc += t.toInt
  else if (r == "b" && regb == 1)
    pc += t.toInt
  else 
    pc += 1
}


def parsePC(str : String) = {
  // print(str)
  val splitted = str.split(" ")
  val cmd = splitted(0)
  if (cmd == "hlf")
    hlf(splitted(1))
  else  if (cmd == "inc")
    inc(splitted(1))
  else if (cmd == "tpl")
    tpl(splitted(1))
  else if (cmd == "jmp")
    jmp(splitted(1))
  else if (cmd == "jie") 
    jie(splitted(1).take(splitted(1).length-1), splitted(2))
  else if (cmd == "jio")
    jio(splitted(1).take(splitted(1).length-1), splitted(2))
  else
    println("Unsupported command: " + cmd)
}

while (pc < program.length) {
  print(pc + " (" + rega  +", " + regb + "): ")
  println("\t" + program(pc))
  parsePC(program(pc))
}

println("A: " + rega)
println("B: " + regb)
