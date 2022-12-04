package aoc2022

import aoc.Problem

import scala.io.Source

object Day04 extends Problem {
  override val year: Int = 2022
  override val day: Int = 4
  override lazy val result1: Long = 2
  override lazy val result2: Long = 4

  case class Range(start: Int, end: Int) {
    assert(end >= start)
  }

  case class Pair(r1: Range, r2: Range)

  override def solve1(s: Source): Long =
    parseInput(s)
      .filter(p => overlapsEntirely(p.r1, p.r2) || overlapsEntirely(p.r2, p.r1))
      .length

  override def solve2(s: Source): Long =
    parseInput(s)
      .filter(p => overlaps(p.r1, p.r2) || overlaps(p.r2, p.r1))
      .length

  private def parseInput(s: Source): Iterator[Pair] =
    s.getLines()
      .map(_.split(",", 2))
      .map { case Array(s1, s2) => Pair(parseRange(s1), parseRange(s2)) }

  private def parseRange(s: String): Range =
    s.split("-", 2) match
      case Array(s, e) => Range(s.toInt, e.toInt)

  private def overlapsEntirely(r1: Range, r2: Range) =
    (r2.start >= r1.start) && (r2.start <= r1.end) &&
      (r2.end >= r1.start) && (r2.end <= r1.end)

  private def overlaps(r1: Range, r2: Range) =
    (r2.start >= r1.start) && (r2.start <= r1.end) ||
      (r2.end >= r1.start) && (r2.end <= r1.end)
}
