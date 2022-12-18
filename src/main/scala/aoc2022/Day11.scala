package aoc2022

import scala.io.Source
import scala.annotation.tailrec

object Day11 extends aoc.Problem[Long] {
  override val year = 2022
  override val day = 11

  override lazy val results1 = 10605 :: Nil
  override lazy val results2 = 2713310158L :: Nil

  case class Monkey(
      id: Int,
      operation: Long => Long,
      divisor: Long,
      trueTo: Int,
      falseTo: Int
  )
  case class State(items: Map[Int, Vector[Long]], numInspected: Map[Int, Long])

  override def solve1(s: Source): Long =
    val (monkeys, state) = parseInput(s)
    runRounds(
      state,
      monkeys,
      w => w / 3,
      20
    ).numInspected.values.toList.sorted.reverse
      .slice(0, 2)
      .product

  override def solve2(s: Source): Long =
    val (monkeys, state) = parseInput(s)
    val gcd = monkeys.map(_.divisor).product
    runRounds(
      state,
      monkeys,
      w => w % gcd,
      10_000
    ).numInspected.values.toList.sorted.reverse
      .slice(0, 2)
      .product

  private def runItem(
      s: State,
      m: Monkey,
      w0: Long,
      manageWorry: Long => Long
  ): State =
    val w1 = m.operation(w0)
    val w2 = manageWorry(w1)
    val throwTo = if (w2 % m.divisor == 0) then m.trueTo else m.falseTo
    State(
      items = s.items.updatedWith(throwTo) {
        case None     => Some(Vector(w2))
        case Some(is) => Some(is :+ w2)
      },
      numInspected = s.numInspected.updatedWith(m.id) {
        case None    => Some(1)
        case Some(v) => Some(v + 1)
      }
    )

  @tailrec
  private def runTurn(
      s: State,
      m: Monkey,
      manageWorry: Long => Long,
      items: Vector[Long]
  ): State =
    items match
      case Vector() => s.copy(items = s.items.updated(m.id, Vector.empty))
      case items @ Vector(item, _*) =>
        val s1 = runItem(s, m, item, manageWorry)
        runTurn(s1, m, manageWorry, items.tail)

  @tailrec
  private def runRound(
      s: State,
      ms: List[Monkey],
      manageWorry: Long => Long
  ): State =
    ms match
      case Nil => s
      case m :: tail =>
        val s1 = runTurn(s, m, manageWorry, s.items(m.id))
        runRound(s1, tail, manageWorry)

  @tailrec
  private def runRounds(
      s: State,
      ms: List[Monkey],
      manageWorry: Long => Long,
      runUntil: Int,
      round: Int = 0
  ): State =
    if round == runUntil then s
    else
      val s1 = runRound(s, ms, manageWorry)
      runRounds(s1, ms, manageWorry, runUntil, round + 1)

  private def parseInput(s: Source) =
    val input = s
      .getLines()
      .filterNot(_.isBlank())
      .grouped(6)
      .map {
        case Seq(
              s"Monkey $id:",
              s"  Starting items: $items",
              s"  Operation: new = old $operator $opRhs",
              s"  Test: divisible by $divisor",
              s"    If true: throw to monkey $trueTo",
              s"    If false: throw to monkey $falseTo"
            ) =>
          val op = (operator, opRhs) match
            case ("*", "old") => (old: Long) => old * old
            case ("+", "old") => (old: Long) => old + old
            case ("*", rhs)   => (old: Long) => old * rhs.toLong
            case ("+", rhs)   => (old: Long) => old + rhs.toLong
          val m =
            Monkey(id.toInt, op, divisor.toLong, trueTo.toInt, falseTo.toInt)
          m.id -> (m -> items.split(",").map(_.strip.toLong).toVector)
      }
      .toMap

    val monkeys = input.map(_._2._1).toList.sortBy(_.id)
    val state =
      State(input.view.mapValues(_._2).toMap, numInspected = Map.empty)
    (monkeys, state)
}
