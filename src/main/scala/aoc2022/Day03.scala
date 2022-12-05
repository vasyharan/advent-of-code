package aoc2022

import scala.io.Source

object Day03 extends aoc.Problem[Long] {
  override val year = 2022
  override val day = 3
  override lazy val results1 = 157 :: Nil
  override lazy val results2 = 70 :: Nil

  override def solve1(s: Source): Long = s
    .getLines()
    .map(line => line.splitAt(line.length / 2))
    .map { case (s1, s2) => s1.toSet.intersect(s2.toSet) }
    .tapEach(s => assert(s.size == 1))
    .map(a => toPriority(a.head))
    .sum

  override def solve2(s: Source): Long = s
    .getLines()
    .map(_.toSet)
    .grouped(3)
    .map(_.reduce(_.intersect(_)))
    .tapEach(s => assert(s.size == 1))
    .map(a => toPriority(a.head))
    .sum

  private def toPriority(c: Char): Int = {
    if c >= 'a' && c <= 'z' then (c - 'a') + 1
    else if c >= 'A' && c <= 'Z' then (c - 'A') + 27
    else sys.error("unexpected char " + c)
  }
}
