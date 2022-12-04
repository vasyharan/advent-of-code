package aoc2022

import scala.io.Source

object Day01 extends aoc.Problem {
  override val year = 2022
  override val day = 1
  override lazy val results1 = 24000 :: Nil
  override lazy val results2 = 45000 :: Nil

  override def solve1(s: Source): Long =
    val (max, _) = calories(s).foldLeft((0, 0)) {
      case ((max, acc), None)           => (Math.max(max, acc), 0)
      case ((max, acc), Some(calories)) => (max, acc + calories)
    }
    max

  override def solve2(s: Source): Long =
    val ((t1, t2, t3), _) =
      calories(s)
        .foldLeft(((0, 0, 0), 0)) {
          case ((top3 @ (t1, t2, t3), acc), None) =>
            if acc > t1 then ((acc, t1, t2), 0)
            else if acc > t2 then ((t1, acc, t2), 0)
            else if acc > t3 then ((t1, t2, acc), 0)
            else (top3, 0)
          case ((top3, acc), Some(calories)) => (top3, acc + calories)
        }
    t1 + t2 + t3

  private def calories(s: Source): Iterator[Option[Int]] =
    (s.getLines().map(_.toIntOption) ++ Iterator(None))
}
