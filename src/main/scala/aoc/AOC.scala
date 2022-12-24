package aoc

import scala.io.Source
import scala.concurrent.duration.Duration

trait Problem[A]:
  val year: Int
  val day: Int
  lazy val results1: List[A]
  lazy val results2: List[A]

  def solve1(s: Source): A
  def solve2(s: Source): A

@main
def main() =
  val problem: Problem[?] = aoc2022.Day14
  val input = f"aoc${problem.year}%s/input${problem.day}%02d.txt"

  def sampleResource(idx: Int) =
    f"aoc${problem.year}%s/sample${problem.day}%02d_${idx + 1}%02d.txt"

  def solve[A](fn: => A): Unit =
    val t0 = System.nanoTime()
    val res = fn
    val dur = Duration.fromNanos(System.nanoTime() - t0)
    println(s"${res} (${dur.toMillis}ms)")

  for { (ex, idx) <- problem.results1.zipWithIndex } {
    val ac = problem.solve1(Source.fromResource(sampleResource(idx)))
    assert(ex == ac, s"test=${idx + 1} expected=${ex} actual=${ac}")
  }
  solve(problem.solve1(Source.fromResource(input)))

  for { (ex, idx) <- problem.results2.zipWithIndex } {
    val ac = problem.solve2(Source.fromResource(sampleResource(idx)))
    assert(ex == ac, s"test=${idx + 1} expected=${ex} actual=${ac}")
  }
  solve(problem.solve2(Source.fromResource(input)))
