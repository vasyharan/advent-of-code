package aoc2022

import scala.io.Source
import scala.collection.immutable.Vector

object Day05 extends aoc.Problem[String] {
  override val year = 2022
  override val day = 5
  override lazy val results1 = "CMZ" :: Nil
  override lazy val results2 = "MCD" :: Nil

  type Crates = Vector[Char]
  type CrateStacks = Map[Int, Crates]
  case class Move(from: Int, to: Int, num: Int)

  override def solve1(s: Source): String =
    def rec(from: Crates, to: Crates, num: Int): (Crates, Crates) =
      if (num == 0) then (from, to)
      else rec(from.tail, from.head +: to, num - 1)

    val (stacks, moves) = parseInput(s)
    val nextStacks = moves.foldLeft(stacks) { (stacks, move) =>
      val from = stacks(move.from)
      val to = stacks(move.to)
      val (nextFrom, nextTo) = rec(from, to, move.num)
      stacks.updated(move.from, nextFrom).updated(move.to, nextTo)
    }
    val topCrates =
      for { i <- 0 until nextStacks.size } yield nextStacks(i).headOption
        .getOrElse("")
    topCrates.mkString

  override def solve2(s: Source): String =
    val (stacks, moves) = parseInput(s)
    val nextStacks = moves.foldLeft(stacks) { (stacks, move) =>
      val from = stacks(move.from)
      val (toMove, nextFrom) = from.splitAt(move.num)
      val nextTo = toMove ++ stacks(move.to)
      stacks.updated(move.from, nextFrom).updated(move.to, nextTo)
    }
    val topCrates =
      for { i <- 0 until nextStacks.size } yield nextStacks(i).headOption
        .getOrElse("")
    topCrates.mkString

  private def parseInput(s: Source): (CrateStacks, Seq[Move]) =
    val (ss, actions, _) = s
      .getLines()
      .foldLeft((Vector.empty[(Int, Char)], Vector.empty[Move], false)) {
        case ((stack, actions, false), line) if line.isBlank =>
          (stack, actions, true)
        case ((stack, actions, false), line) =>
          val s = line.toCharArray().zipWithIndex.collect {
            case (c, i) if c >= 'A' && c <= 'Z' => (i / 4, c)
          }
          (stack ++ s, actions, false)
        case ((stack, actions, true), line) =>
          val move = line.split(" ", 6) match {
            case Array("move", num, "from", from, "to", to) =>
              Move(from.toInt - 1, to.toInt - 1, num.toInt)
          }
          (stack, actions :+ move, true)
      }
    val stacks = ss.groupMap((key, c) => key)((_, c) => c)
    stacks -> actions
}
