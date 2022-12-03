package aoc2022

import scala.io.Source

object Day03 {
  def solve01(s: Source): Int = s.getLines()
    .map(line => line.splitAt(line.length / 2))
    .map { case (s1, s2) => s1.toSet.intersect(s2.toSet) }
    .tapEach(s => assert(s.size == 1))
    .map(a => toPriority(a.head))
    .sum

  def solve02(s: Source): Int = s.getLines()
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

@main
def run202203(): Unit =
  assert(Day03.solve01(Source.fromResource("aoc2022/sample03.txt")) == 157)
  println(Day03.solve01(Source.fromResource("aoc2022/input03.txt")))
  assert(Day03.solve02(Source.fromResource("aoc2022/sample03.txt")) == 70)
  println(Day03.solve02(Source.fromResource("aoc2022/input03.txt")))
