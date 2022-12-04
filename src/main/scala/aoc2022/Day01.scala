package aoc2022

import scala.io.Source

object Day01 {
  def solve01(input: Source): Int = {
    val (max, _) = input.getLines().map(_.toIntOption).foldLeft((0, 0)) {
      case ((max, acc), None)           => (Math.max(max, acc), 0)
      case ((max, acc), Some(calories)) => (max, acc + calories)
    }
    max
  }

  def solve02(input: Source): Int = {
    val ((t1, t2, t3), _) =
      input.getLines().map(_.toIntOption).foldLeft(((0, 0, 0), 0)) {
        case ((top3 @ (t1, t2, t3), acc), None) =>
          if acc > t1 then ((acc, t1, t2), 0)
          else if acc > t2 then ((t1, acc, t2), 0)
          else if acc > t3 then ((t1, t2, acc), 0)
          else (top3, 0)
        case ((top3, acc), Some(calories)) => (top3, acc + calories)
      }
    t1 + t2 + t3
  }
}

@main
def run202201() =
  assert(Day01.solve01(Source.fromResource("aoc2022/input01.txt")) == 72240)
  assert(Day01.solve02(Source.fromResource("aoc2022/input01.txt")) == 210957)
