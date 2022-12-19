package aoc2022

import scala.collection.mutable.PriorityQueue
import scala.io.Source

object Day12 extends aoc.Problem[Int] {
  override val year = 2022
  override val day = 12

  override lazy val results1: List[Int] = 31 :: Nil
  override lazy val results2: List[Int] = 29 :: Nil

  case class Pos(x: Int, y: Int)
  case class Path(d: Int, pos: Pos)
  object Path {
    implicit def ordering: Ordering[Path] = Ordering.by[Path, Int](_.d).reverse
  }

  case class Grid[A](g: Vector[Vector[A]]) {
    val size = Pos(g.head.length, g.length)

    def neighbours(p: Pos): Iterator[Pos] = {
      Iterator(
        Pos(p.x + 1, p.y),
        Pos(p.x - 1, p.y),
        Pos(p.x, p.y + 1),
        Pos(p.x, p.y - 1)
      )
        .filter { p => p.y >= 0 && p.y < size.y && p.x >= 0 && p.x < size.x }
    }

    def apply(p: Pos): A = apply(p.x, p.y)
    def apply(x: Int, y: Int): A = g(y)(x)

    def foldLeft[B](z: B)(fn: (B, (Pos, A)) => B): B =
      g.zipWithIndex.foldLeft(z) { case (z, (row, y)) =>
        row.zipWithIndex.foldLeft(z) { case (z, (v, x)) =>
          fn(z, (Pos(x, y), v))
        }
      }

    override def toString(): String = g.map(_.mkString).mkString("\n")
  }

  override def solve1(s: Source): Int =
    val (start, end, grid) = parseInput(s)
    var shortestPaths = dijkstra(
      start,
      p => {
        val maxHeight = calcHeight(grid(p)) + 1
        grid.neighbours(p).filter(p => calcHeight(grid(p)) <= maxHeight)
      }
    )
    val path = constructPath(shortestPaths, end)
    path.size - 1

  override def solve2(s: Source): Int =
    val (start, end, grid) = parseInput(s)

    var shortestPaths = dijkstra(
      end,
      p => {
        val minHeight = calcHeight(grid(p)) - 1
        grid.neighbours(p).filter(p => calcHeight(grid(p)) >= minHeight)
      }
    )

    grid.g.zipWithIndex
      .flatMap { (row, y) =>
        row.zipWithIndex
          .filter { (v, x) => calcHeight(v) == 0 }
          .map { (_, x) => Pos(x, y) }
      }
      .map(constructPath(shortestPaths, _))
      .filter(_.head == end)
      .map(_.size - 1)
      .min

  private def dijkstra(start: Pos, edges: Pos => Iterator[Pos]): Map[Pos, Pos] =
    var visited = Set.empty[Pos]
    var dist = Map(start -> 0).withDefaultValue(Int.MaxValue)
    var prev = Map.empty[Pos, Pos]
    val q = PriorityQueue[Path](Path(0, start))
    while (!q.isEmpty) {
      val Path(_, u) = q.dequeue()
      if !visited.contains(u) then
        visited = visited + u
        edges(u)
          .foreach { v =>
            val alt = dist(u) + 1
            val cur = dist(v)
            if alt < cur then
              dist = dist.updated(v, alt)
              prev = prev.updated(v, u)
              q.enqueue(Path(alt, v))
          }
    }
    prev

  private def constructPath(
      prev: Map[Pos, Pos],
      pos: Pos,
      path: List[Pos] = List.empty[Pos]
  ): List[Pos] =
    prev.get(pos) match {
      case Some(p) => constructPath(prev, p, pos :: path)
      case None    => pos :: path
    }

  private def calcHeight(c: Char): Int = c match
    case 'S' => 0
    case 'E' => 25
    case c   => (c - 'a')

  private def parseInput(s: Source) =
    val g = Grid(s.getLines().map(_.toCharArray().toVector).toVector)
    val (start, end) = g.foldLeft((Option.empty[Pos], Option.empty[Pos])) {
      case ((s, e), (p, v)) if v == 'S' => ((Some(p), e))
      case ((s, e), (p, v)) if v == 'E' => ((s, Some(p)))
      case (acc, _)                     => acc
    }
    (start.get, end.get, g)
}
