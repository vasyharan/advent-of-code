package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day09 extends aoc.Problem[Long] {
  override val year = 2021
  override val day = 9
  override lazy val results1 = 15 :: Nil
  override lazy val results2 = 1134 :: Nil

  case class Point(x: Int, y: Int)

  case class Grid(g: Array[Array[Int]]) {
    val size = Point(g.head.length, g.length)

    def neighbours(p: Point): List[Point] = {
      List(
        Point(p.x + 1, p.y),
        Point(p.x - 1, p.y),
        Point(p.x, p.y + 1),
        Point(p.x, p.y - 1)
      )
        .filter { p => p.y >= 0 && p.y < size.y && p.x >= 0 && p.x < size.x }
    }

    def apply(p: Point): Int = apply(p.x, p.y)

    def apply(x: Int, y: Int): Int = g(y)(x)
  }

  override def solve1(s: Source) = {
    val grid = Grid(s.getLines().map(_.split("").map(_.toInt)).toArray)
    findLowPoints(grid).map(grid(_)).map(_ + 1).sum
  }

  override def solve2(s: Source) = {
    val grid = Grid(s.getLines().map(_.split("").map(_.toInt)).toArray)
    findLowPoints(grid)
      .map(findBasin(grid, _))
      .map(_.size)
      .sorted
      .reverse
      .take(3)
      .product
  }

  private def findBasin(grid: Grid, point: Point): Set[Point] = {
    @tailrec
    def rec(search: List[Point], basins: Set[Point]): Set[Point] =
      search match
        case Nil => basins
        case p :: tail =>
          val ps = grid
            .neighbours(p)
            .filter(n => grid(n) != 9 && grid(n) > grid(p))
          rec(tail :++ ps, basins + p)

    rec(point :: Nil, Set.empty)
  }

  private def findLowPoints(grid: Grid): Seq[Point] =
    for {
      y <- 0.until(grid.size.y)
      x <- 0.until(grid.size.x)
      point = Point(x, y)
      if isLowPoint(grid, point)
    } yield point

  private def isLowPoint(grid: Grid, point: Point) =
    grid.neighbours(point).forall(n => grid(point) < grid(n))
}
