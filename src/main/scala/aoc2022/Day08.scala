package aoc2022

import scala.io.Source

object Day08 extends aoc.Problem[Int] {
  override val year = 2022
  override val day = 8
  override lazy val results1 = 21 :: Nil
  override lazy val results2: List[Int] = 8 :: Nil

  case class Point(x: Int, y: Int)

  case class Grid[A](g: Vector[Vector[A]]) {
    val size = Point(g.head.length, g.length)

    def apply(p: Point): A = apply(p.x, p.y)

    def apply(x: Int, y: Int): A = g(y)(x)

    def map[B](fn: (Point, A) => B): Grid[B] =
      Grid(g.zipWithIndex.map { (row, y) =>
        row.zipWithIndex.map { (v, x) =>
          fn(Point(x, y), v)
        }
      })

    override def toString(): String = g.map(_.mkString).mkString("\n")
  }

  override def solve1(s: Source): Int =
    def scanLeftMax(as: Vector[Int]) = as.scanLeft(0)(Math.max).tail
    def scanRightMax(as: Vector[Int]) = as.scanRight(0)(Math.max).init

    val grid = parseInput(s)
    val maxL = Grid(grid.g.map(scanLeftMax))
    val maxR = Grid(grid.g.map(scanRightMax))
    val maxD = Grid(grid.g.transpose.map(scanLeftMax).transpose)
    val maxU = Grid(grid.g.transpose.map(scanRightMax).transpose)
    val visible = grid.map { (p, v) =>
      if p.x == 0 || p.x == grid.size.x - 1 then true
      else if p.y == 0 || p.y == grid.size.y - 1 then true
      else
        v > maxL(p.copy(x = p.x - 1)) ||
        v > maxR(p.copy(x = p.x + 1)) ||
        v > maxD(p.copy(y = p.y - 1)) ||
        v > maxU(p.copy(y = p.y + 1))
    }
    visible.g.map { row => row.count(identity) }.sum

  override def solve2(s: Source): Int =
    def scanLeftMax(as: Vector[Int]) =
      as.zipWithIndex.map { case (h, i) =>
        as.slice(0, i)
          .zipWithIndex
          .reverse
          .find((hh, ii) => hh >= h)
          .map(i - _._2)
          .getOrElse(i)
      }

    def scanRightMax(as: Vector[Int]) =
      as.zipWithIndex.map { case (h, i) =>
        as.zipWithIndex
          .slice(i + 1, as.size)
          .find((hh, ii) => hh >= h)
          .map(_._2 - i)
          .getOrElse(as.size - i - 1)
      }

    val grid = parseInput(s)
    val maxL = Grid(grid.g.map(scanLeftMax))
    val maxR = Grid(grid.g.map(scanRightMax))
    val maxU = Grid(grid.g.transpose.map(scanLeftMax).transpose)
    val maxD = Grid(grid.g.transpose.map(scanRightMax).transpose)
    val score = grid.map { (p, _) => maxL(p) * maxR(p) * maxU(p) * maxD(p) }
    score.g.map { _.max }.max

  private def parseInput(s: Source): Grid[Int] = Grid(
    s.getLines().map(_.split("").map(_.toInt).toVector).toVector
  )
}
