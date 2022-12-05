package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends aoc.Problem[Long] {
  val year = 2021
  val day = 11
  lazy val results1 = 1656 :: Nil
  lazy val results2 = 195 :: Nil

  case class Point(x: Int, y: Int)

  case class Grid(es: Map[Point, Int]) {

    import Grid.*

    override def toString: String = {
      val v = es.view.mapValues {
        case x if x == 10 => "x"
        case x if x > 10  => "."
        case x            => s"${x}"
      }
      0.until(size)
        .map { y => 0.until(size).map { x => v(Point(x, y)) }.mkString }
        .mkString("\n")
    }
  }

  object Grid {
    val size = 10

    def neighbours(p: Point): List[Point] = {
      List(
        Point(p.x - 1, p.y),
        Point(p.x - 1, p.y - 1),
        Point(p.x, p.y - 1),
        Point(p.x + 1, p.y - 1),
        Point(p.x + 1, p.y),
        Point(p.x + 1, p.y + 1),
        Point(p.x, p.y + 1),
        Point(p.x - 1, p.y + 1)
      ).filter { p => p.y >= 0 && p.y < size && p.x >= 0 && p.x < size }
    }
  }

  def solve1(s: Source): Long =
    @tailrec
    def rec(g: Grid, totalFlashes: Long, remaining: Int): Long =
      if remaining == 0 then totalFlashes
      else
        val (gNext, numFlashes) = runStep(g)
        rec(gNext, totalFlashes + numFlashes, remaining - 1)

    rec(fromSource(s), 0, 100)

  def solve2(s: Source): Long =
    @tailrec
    def rec(g: Grid, gen: Int): Long =
      val (gNext, numFlashes) = runStep(g)
      if numFlashes == 100 then gen
      else rec(gNext, gen + 1)

    rec(fromSource(s), 1)

  private def fromSource(s: Source): Grid =
    Grid(
      s.getLines()
        .zipWithIndex
        .flatMap { case s -> y =>
          s.split("").zipWithIndex.map { case c -> x => Point(x, y) -> c.toInt }
        }
        .toMap
    )

  private def runStep(g: Grid): (Grid, Int) =
    val g1 = Grid(g.es.view.mapValues(_ + 1).toMap)
    val g2 = runFlashes(g1)
    countFlashes(g2)

  private def runFlashes(g: Grid): Grid =
    @tailrec
    def rec(g: Grid, es: List[Point]): Grid =
      es match
        case Nil => g
        case e :: tail =>
          val g0 = (e :: Grid.neighbours(e)).foldLeft(g) { case acc -> p =>
            Grid(acc.es.updatedWith(p)(_.map(_ + 1)))
          }
          val flashes = Grid.neighbours(e).filter(p => g0.es(p) == 10)
          rec(g0, flashes :++ tail)

    rec(g, g.es.filter { case (p, c) => c == 10 }.keys.toList)

  private def countFlashes(g: Grid): (Grid, Int) =
    @tailrec
    def rec(
        es: List[(Point, Int)],
        fs: List[(Point, Int)],
        c: Int
    ): (Grid, Int) =
      es match
        case Nil => (Grid(fs.toMap), c)
        case (p, en) :: tail =>
          if en > 9 then rec(tail, p -> 0 :: fs, c + 1)
          else rec(tail, (p, en) :: fs, c)

    rec(g.es.toList, Nil, 0)
}
