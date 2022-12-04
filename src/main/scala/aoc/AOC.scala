package aoc

import scala.io.Source

trait Problem:
  val year: Int
  val day: Int
  lazy val results1: List[Long]
  lazy val results2: List[Long]

  def solve1(s: Source): Long
  def solve2(s: Source): Long

@main
def main() =
  val problem: Problem = aoc2022.Day04
  val input = f"aoc${problem.year}%s/input${problem.day}%02d.txt"

  def sampleResource(idx: Int) =
    f"aoc${problem.year}%s/sample${problem.day}%02d_${idx + 1}%02d.txt"

  for { (expected, idx) <- problem.results1.zipWithIndex } {
    val actual = problem.solve1(Source.fromResource(sampleResource(idx)))
    assert(expected == actual, s"expected=${expected} actual=${actual}")
  }
  println(problem.solve1(Source.fromResource(input)))

  for { (expected, idx) <- problem.results2.zipWithIndex } {
    val actual = problem.solve2(Source.fromResource(sampleResource(idx)))
    assert(expected == actual, s"expected=${expected} actual=${actual}")
  }
  println(problem.solve2(Source.fromResource(input)))
