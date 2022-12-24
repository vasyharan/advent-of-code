package aoc2022

import scala.io.Source

object Day15 extends aoc.Problem[Long] {
  override val year: Int = 2022
  override val day: Int = 15
  override lazy val results1: List[Long] = 26 :: Nil
  override lazy val results2: List[Long] = 56000011 :: Nil

  case class Point(x: Int, y: Int) {
    def manhattanDistance(o: Point): Int = Math.abs(x - o.x) + Math.abs(y - o.y)
  }
  case class XRange(start: Int, end: Int)

  override def solve1(s: Source): Long =
    val (row, sensorBeacons) = parseInput(s)
    eliminateBeacon(row, sensorBeacons)
      .foldLeft(0) { case (acc, XRange(start, end)) =>
        acc + (end - start)
      }

  override def solve2(s: Source): Long =
    val (row, sensorBeacons) = parseInput(s)
    val max = row * 2
    (0 to max)
      .map { row =>
        val ranges = eliminateBeacon(row, sensorBeacons).map(xr =>
          XRange(Math.max(0, xr.start), Math.min(max, xr.end))
        )
        (row, ranges)
      }
      .filter((row, ranges) => ranges.size > 1)
      // .tapEach(println)
      .map((y, ranges) => Point(ranges.head.end + 1, y))
      // .tapEach(println)
      .map(p => p.x * 4000000L + p.y)
      .head

  private def eliminateBeacon(row: Int, sensorBeacons: List[(Point, Point)]) =
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
