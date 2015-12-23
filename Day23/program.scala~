import scala.collection.mutable.Set

val MM = 0
val DRAIN = 1
val SHIELD = 2
val POISON = 3
val RECHARGE = 4



// Guess less than a fifty steps

val moves = Array.ofDim[Int](20)


def nextStrat() = {
  def inc(idx : Int) : Unit = {
    if (moves(idx) == RECHARGE) {
      moves(idx) = 0
      inc(idx+1)
    } else {
      moves(idx) = moves(idx) + 1
    }
  }
  inc(0)
}


def playStrat(maxMana : Int) : Option[Int] = {
  var turn = 0
  var poison = 0
  var shield = 0
  var recharge = 0

  var myHP = 50
  var myMP = 500

  var enHP = 51
  var enDMG = 9

  var manaSpent = 0


  def checkEffects() = {
    if (poison > 0) {
      enHP -= 3
      poison -= 1
    }
    if (shield > 0) {
      shield -= 1
    }
    if (recharge > 0) {
      myMP += 101
      recharge -= 1
    }
  }

  def decMP(mp : Int) = {
    manaSpent += mp
    myMP -= mp
  }

  while (true) {
    // PLY TURN
    checkEffects()
    myHP -= 1
    if (myHP <= 0)
      return None

    moves(turn) match {
      case 0 => {
        decMP(53)
        enHP -= 4
      }
      case 1 => {
        decMP(73)
        enHP -= 2
        myHP += 2
      }
      case 2 => {
        if (shield > 0)
          return None
        decMP(113)
        shield = 6
      }
      case 3 => {
        if (poison > 0)
          return None
        decMP(173)
        poison = 6
      }
      case 4 => {
        if (recharge > 0)
          return None
        decMP(229)
        recharge = 5
      }
    }

    if (manaSpent >= maxMana)
      return None

    if (myMP < 0)
      return None
    if (enHP <= 0)
      return Some(manaSpent)

    // EN TURN
    checkEffects()
    if (enHP <= 0)
      return Some(manaSpent)
    if (shield > 0)
      myHP -= enDMG - 7
    else
      myHP -= enDMG

    if (myHP <= 0)
      return None

    turn += 1
  }
  None
}

var best = 10000

for (i <- 0 until 1000000000) {
  nextStrat()
  val res = playStrat(best)
  if(!res.isEmpty) {
    best = res.get
    println(best)
  }
}

