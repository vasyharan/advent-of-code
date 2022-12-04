package aoc

import aoc2021.Day11

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
  val problem: Problem = Day11
  val sample = s"aoc${problem.year}/sample${problem.day}.txt"
  val input = s"aoc${problem.year}/input${problem.day}.txt"

  val r1 = problem.solve1(Source.fromResource(sample))
  assert(r1 == problem.result1, s"expected=${problem.result1} actual=${r1}")
  println(problem.solve1(Source.fromResource(input)))

  val r2 = problem.solve2(Source.fromResource(sample))
  assert(r2 == problem.result2, s"expected=${problem.result2} actual=${r2}")
  println(problem.solve2(Source.fromResource(input)))
