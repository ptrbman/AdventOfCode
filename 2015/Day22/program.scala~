import scala.collection.mutable.Set

val wps = List(
  (8, 4, 0), 
    (10, 5, 0),
    (25, 6, 0),
    (40, 7, 0),
    (74, 8, 0))

val arm = List(
  (0, 0, 0),
  (13, 0, 1),
    (31, 0, 2),
    (53, 0, 3),
    (75, 0, 4),
    (102, 0, 5))

val rings = List(
  (0, 0, 0, "A"),
  (0, 0, 0, "B"),
  (0, 0, 0, "C"),
  (25, 1, 0, "D"),
    (50, 2, 0, "E"),
    (100, 3, 0, "F"),
    (20, 0, 1, "G"),
    (40, 0, 2, "H"),
    (80, 0, 3, "I"))

def combat(_hp : Int, dmg : Int, arm : Int) : Boolean = {
  var hp = _hp
  var bossHp = 104
  val bossDmg = 8
  val bossArm = 1

  val plyAtk = if (dmg - bossArm > 0) dmg - bossArm else 1
  val bossAtk = if (bossDmg - arm > 0) bossDmg - arm else 1

  while (true) {
    bossHp -= plyAtk
    if (bossHp <= 0)
      return true

    hp -= bossAtk
    if (hp <= 0)
      return false
  }
  true
}

var best = 0

for (w <- wps; a <- arm; r1 <- rings; r2 <- rings; r3 <- rings;
  if (r1 != r2 && r1 != r3 && r2 != r3)) {
  val (wpnCost, wpnDmg, wpnArm) = w
  val (armCost, armDmg, armArm) = a
  val (r1Cost, r1Dmg, r1Arm, _) = r1
  val (r2Cost, r2Dmg, r2Arm, _) = r2
  val (r3Cost, r3Dmg, r3Arm, _) = r3

  print(".")
  val dmg = wpnDmg + armDmg + r1Dmg + r2Dmg + r3Dmg
  val arm = wpnArm + armArm + r1Arm + r2Arm + r3Arm
  val hp = 100
  if (!combat(hp, dmg, arm)) {
    val cost = wpnCost + armCost + r1Cost + r2Cost + r3Cost
    if (cost > best)
      best = cost
  }
}

println(best)
