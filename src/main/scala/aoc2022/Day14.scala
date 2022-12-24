package aoc2022

import aoc2022.Day14.StepResult.AtRest

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends aoc.Problem[Int] {
  override val year: Int = 2022
  override val day: Int = 14
  override lazy val results1: List[Int] = 24 :: Nil
  override lazy val results2: List[Int] = ???

  enum Cell:
    case Air, Rock, Sand

  case class Point(x: Int, y: Int) {
    def -(o: Point): Point = Point(x - o.x, y - o.y)

    def +(o: Point): Point = Point(x + o.x, y + o.y)
  }

  object Point {
    def parse(s: String): Point =
      s.split(",", 2) match
        case Array(x, y) => Point(x.toInt, y.toInt)
  }

  case class Polyline(points: Vector[Point]) {
    def path: Iterator[Point] =
      points
        .sliding(2)
        .flatMap {
          case Vector(a, b) =>
            if a.x == b.x then
              val step = if a.y < b.y then 1 else -1
              Range(a.y, b.y, step).map(y => Point(a.x, y))
            else if a.y == b.y then
              val step = if a.x < b.x then 1 else -1
              Range(a.x, b.x, step).map(x => Point(x, a.y))
            else sys.error("diagonal line!")
          case _ => sys.error("unexpected sliding window")
        } ++ Iterator(points.last)
  }

  object Polyline {
    def parse(s: String): Polyline =
      Polyline(s.split(" -> ").toVector.map(Point.parse))
  }

  case class Grid(origin: Point, width: Int, height: Int, cells: Vector[Cell]) {
    private val end: Point = origin + Point(width, height)

    def cell(p: Point): Option[Cell] = index(p).map(cells(_))

    def update(p: Point, c: Cell): Option[Grid] =
      index(p).map(i => copy(cells = cells.updated(i, c)))

    private def index(p: Point): Option[Int] =
      if p.x < origin.x || p.y < origin.y then None
      else if p.x >= end.x || p.y >= end.y then None
      else
        val p1 = p - origin
        Some(p1.y * width + p1.x)

    override def toString(): String =
      cells
        .grouped(width)
        .map { row =>
          row.map {
            case Cell.Air  => '.'
            case Cell.Rock => '#'
            case Cell.Sand => 'o'
          }.mkString
        }
        .mkString("\n")
  }

  object Grid {
    def apply(tl: Point, br: Point, rocks: Vector[Polyline]): Grid =
      val dims = (br - tl) + Point(1, 1)
      val grid =
        Grid(tl, dims.x, dims.y, Vector.fill(dims.x * dims.y)(Cell.Air))
      rocks.foldLeft(grid) { case (g, rock) =>
        rock.path.foldLeft(g) { case (g, p) => g.update(p, Cell.Rock).get }
      }
  }

  sealed trait StepResult {}

  object StepResult:
    case class AtRest(p: Point) extends StepResult

    case class Falling(p: Point) extends StepResult

    case object OutOfBounds extends StepResult

  override def solve1(s: Source): Int =
    import Cell._
    import StepResult._

    val lines = parseInput(s)
    val spawn = Point(500, 0)
    val (min, max) =
      lines.flatMap(_.points).foldLeft((spawn, spawn)) { case ((min, max), p) =>
        Point(Math.min(min.x, p.x), Math.min(min.y, p.y)) ->
          Point(Math.max(max.x, p.x), Math.max(max.y, p.y))
      }

    @tailrec
    def searchSteps(ps: List[Point], grid: Grid, g: Point): StepResult =
      ps match
        case Nil => AtRest(g)
        case p :: tail =>
          grid.cell(p) match
            case None                => OutOfBounds
            case Some(c) if c == Air => Falling(p)
            case Some(_)             => searchSteps(tail, grid, g)

    def runStep(grid: Grid, g: Point): StepResult =
      val down = g + Point(0, 1)
      val downLeft = g + Point(-1, 1)
      val downRight = g + Point(1, 1)
      val possibleSteps = down :: downLeft :: downRight :: Nil
      searchSteps(possibleSteps, grid, g)

    @tailrec
    def runSimulation(grid: Grid, grain: Point, atRest: Int): Int =
      runStep(grid, grain) match {
        case AtRest(p) =>
          runSimulation(grid.update(grain, Sand).get, spawn, atRest + 1)
        case Falling(p)  => runSimulation(grid, p, atRest)
        case OutOfBounds => atRest
      }

    val grid = Grid(min, max, lines)
    runSimulation(grid, spawn, 0)

  override def solve2(s: Source): Int = ???

  private def parseInput(s: Source) =
    s.getLines().map(Polyline.parse).toVector
}
