package aoc

import scala.io.Source

trait Problem:
  val year: Int
  val day: Int
  lazy val result1: Long
  lazy val result2: Long

  def solve1(s: Source): Long

  def solve2(s: Source): Long


@main
def main() =
  val problem: Problem = aoc2022.Day04
  val sample = f"aoc${problem.year}%s/sample${problem.day}%02d.txt"
  val input = f"aoc${problem.year}%s/input${problem.day}%02d.txt"

  val r1 = problem.solve1(Source.fromResource(sample))
  assert(r1 == problem.result1, s"expected=${problem.result1} actual=${r1}")
  println(problem.solve1(Source.fromResource(input)))

  val r2 = problem.solve2(Source.fromResource(sample))
  assert(r2 == problem.result2, s"expected=${problem.result2} actual=${r2}")
  println(problem.solve2(Source.fromResource(input)))
