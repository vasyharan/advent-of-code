package aoc2022

import scala.io.Source

object Day15 extends aoc.Problem[Int] {
  override val year: Int = 2022
  override val day: Int = 15
  override lazy val results1: List[Int] = 26 :: Nil
  override lazy val results2: List[Int] = ???

  case class Point(x: Int, y: Int) {
    def manhattanDistance(o: Point): Int = Math.abs(x - o.x) + Math.abs(y - o.y)
  }
  case class XRange(start: Int, end: Int)

  override def solve1(s: Source): Int =
    val (row, sensorBeacons) = parseInput(s)
    sensorBeacons
      .foldLeft(List.empty[XRange]) { case (acc, (s, b)) =>
        val d = s.manhattanDistance(b)
        val dx = d - Math.abs(s.y - row)
        if dx < 0 then acc
        else XRange((s.x - dx), (s.x + dx)) :: acc
      }
      .sortBy(_.start)
      // .tapEach(s => println(s"unmerged: ${s}"))
      .foldLeft((List.empty[XRange])) {
        case (Nil, r) => r :: Nil
        case (p :: tail, r) =>
          if r.start >= p.start && r.start <= (p.end + 1) then
            XRange(Math.min(r.start, p.start), Math.max(r.end, p.end)) :: tail
          else r :: p :: tail
      }
      .reverse
      // .tapEach(s => println(s"merged: ${s}"))
      .foldLeft(0) { case (acc, XRange(start, end)) =>
        acc + (end - start)
      }

  override def solve2(s: Source): Int =
    ???

  private def parseInput(s: Source): (Int, List[(Point, Point)]) =
    val (s1, s2) = s.getLines().splitAt(1)
    (
      s1.toList.map(_.toInt).head,
      s2.map {
        case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
          (Point(sx.toInt, sy.toInt), Point(bx.toInt, by.toInt))
      }.toList
    )

}
