package aoc2021

import scala.io.Source

object Day08 extends aoc.Problem {
  override val year: Int = 2021
  override val day: Int = 8
  override lazy val result1: Long = 26
  override lazy val result2: Long = 61229

  def solve1(in: Source) = in
    .getLines()
    .map(_.split(" \\| ", 2))
    .map { case Array(_, out) => out }
    .flatMap { s => s.strip().split(" ") }
    .map(_.length)
    .filter { len => len == 2 || len == 4 || len == 3 || len == 7 }
    .length

  def solve2(in: Source) = in
    .getLines()
    .map(_.split(" \\| ", 2))
    .map { case Array(in, out) =>
      val lookup: Map[Set[Char], Int] = mkLookup(in)
      out
        .strip()
        .split(" ")
        .map(_.toCharArray.toSet)
        .map(lookup.get(_).get)
        .mkString
        .toInt
    }
    .sum

  private def mkLookup(s: String): Map[Set[Char], Int] = {
    def d(digits: Array[Set[Char]]): Set[Char] =
      assert(digits.length == 1)
      digits(0)

    val digits: Array[Set[Char]] = s.strip().split(" ").map(_.toCharArray.toSet)
    val digitsByLen = digits.groupBy(_.size)
    val d1 = d(digitsByLen(2))
    val d7 = d(digitsByLen(3))
    val d4 = d(digitsByLen(4))
    val d8 = d(digitsByLen(7))

    val c41 = d4 -- d1
    val d235 = digitsByLen(5)
    val d069 = digitsByLen(6)

    val (d3x, d25) = d235.partition(_.intersect(d1) == d1)
    val (d5x, d2x) = d25.partition(_.intersect(c41) == c41)
    val d2 = d(d2x)
    val d3 = d(d3x)
    val d5 = d(d5x)

    val (d0x, d69) = d069.partition(_.intersect(c41) != c41)
    val (d9x, d6x) = d69.partition(_.intersect(d1) == d1)
    val d0 = d(d0x)
    val d6 = d(d6x)
    val d9 = d(d9x)

    Map(
      d0 -> 0,
      d1 -> 1,
      d2 -> 2,
      d3 -> 3,
      d4 -> 4,
      d5 -> 5,
      d6 -> 6,
      d7 -> 7,
      d8 -> 8,
      d9 -> 9
    )
  }
}
