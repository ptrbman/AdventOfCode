    trait Operation
{
    val wireName: String

    def resolve(resolved: Map[String, Int], unresolved: List[Operation]): (Map[String, Int])

    def getAndResolve(wire: String, resolved: Map[String, Int], unresolved: List[Operation]) =
    {
        val value = if (wire.forall(_.isDigit)) Some(wire.toInt) else resolved.find(_._1 == wire).map(_._2)
        val resolvedValue = value.map(_ => Map.empty[String, Int]).getOrElse(unresolved.find(_.wireName == wire).get.resolve(resolved, unresolved))

        value.getOrElse(resolvedValue.find(_._1 == wire).map(_._2).get) -> resolvedValue
    }
}

case class AndOperation(wireName: String, left: String, right: String) extends Operation
{
    override def resolve(resolved: Map[String, Int], unresolved: List[Operation]): (Map[String, Int]) =
    {
        val resolvedLeft = getAndResolve(left, resolved, unresolved)
        val resolvedRight = getAndResolve(right, resolved ++ resolvedLeft._2, unresolved)

        Map(wireName -> (resolvedLeft._1 & resolvedRight._1)) ++ resolvedLeft._2 ++ resolvedRight._2
    }
}

case class OrOperation(wireName: String, left: String, right: String) extends Operation
{
    override def resolve(resolved: Map[String, Int], unresolved: List[Operation]): Map[String, Int] =
    {
        val resolvedLeft = getAndResolve(left, resolved, unresolved)
        val resolvedRight = getAndResolve(right, resolved ++ resolvedLeft._2, unresolved)

        Map(wireName -> (resolvedLeft._1 | resolvedRight._1)) ++ resolvedLeft._2 ++ resolvedRight._2
    }
}

case class LShiftOperation(wireName: String, left: String, right: String) extends Operation
{
    override def resolve(resolved: Map[String, Int], unresolved: List[Operation]): Map[String, Int] =
    {
        val resolvedLeft = getAndResolve(left, resolved, unresolved)
        val resolvedRight = getAndResolve(right, resolved ++ resolvedLeft._2, unresolved)

        Map(wireName -> (resolvedLeft._1 << resolvedRight._1)) ++ resolvedLeft._2 ++ resolvedRight._2
    }
}

case class RShiftOperation(wireName: String, left: String, right: String) extends Operation
{
    override def resolve(resolved: Map[String, Int], unresolved: List[Operation]): Map[String, Int] =
    {
        val resolvedLeft = getAndResolve(left, resolved, unresolved)
        val resolvedRight = getAndResolve(right, resolved ++ resolvedLeft._2, unresolved)

        Map(wireName -> (resolvedLeft._1 >> resolvedRight._1)) ++ resolvedLeft._2 ++ resolvedRight._2
    }
}

case class NotOperation(wireName: String, left: String) extends Operation
{
    override def resolve(resolved: Map[String, Int], unresolved: List[Operation]): Map[String, Int] =
    {
        val resolvedLeft = getAndResolve(left, resolved, unresolved)

        Map(wireName -> (~resolvedLeft._1)) ++ resolvedLeft._2
    }
}

case class PipeOperation(inputWire: String, wireName: String) extends Operation
{
    override def resolve(resolved: Map[String, Int], unresolved: List[Operation]): Map[String, Int] =
    {
        val value = resolved.find(_._1 == inputWire).map(_._2)
        val resolvedVal = value.map(Map()).getOrElse(unresolved.find(_.wireName == inputWire).get.resolve(resolved, unresolved))

        Map(wireName -> value.getOrElse(resolvedVal.find(_._1 == inputWire).get._2)) ++ resolvedVal
    }
}

class Resolver()
{
    val circuit = scala.io.Source.fromFile("input.txt").getLines.toList

    val (resolved, unresolved) = circuit.foldLeft((Map[String, Int](), List[Operation]()))((a, b) => b.split(" ") match
    {
        case Array(left, "AND", right, _, wireName) => a.copy(_2 = a._2 :+ AndOperation(wireName, left, right))
        case Array(left, "OR", right, _, wireName) => a.copy(_2 = a._2 :+ OrOperation(wireName, left, right))
        case Array(left, "RSHIFT", right, _, wireName) => a.copy(_2 = a._2 :+ RShiftOperation(wireName, left, right))
        case Array(left, "LSHIFT", right, _, wireName) => a.copy(_2 = a._2 :+ LShiftOperation(wireName, left, right))
        case Array("NOT", left, _, wireName) => a.copy(_2 = a._2 :+ NotOperation(wireName, left))
        case Array(input, _, wireName) if input.forall(_.isDigit) => a.copy(_1 = a._1 + (wireName -> input.toInt))
        case Array(input, _, wireName) => a.copy(_2 = a._2 :+ PipeOperation(input, wireName))
    })

    val results = unresolved.scanLeft(resolved, unresolved)((a, b) =>
    {
        if (resolved.exists(_._1 == b.wireName))
        {
            a
        } else
        {
            val newResolved = b.resolve(a._1, a._2)
            val resolvedWires = newResolved.keys.toList

            a._1 ++ newResolved -> a._2.filterNot(o => resolvedWires.contains(o.wireName))
        }
    })

    results.last._1.map(r => println(r + " = " + r._2.toString))

    println(results.last._1.find(_._1 == "a").map(_._2))
}


new Resolver()
