package aoc2022

import aoc2022.Day14.StepResult.AtRest

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends aoc.Problem[Int] {
  override val year: Int = 2022
  override val day: Int = 14
  override lazy val results1: List[Int] = 24 :: Nil
  override lazy val results2: List[Int] = 93 :: Nil

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

  trait Grid:
    val origin: Point
    val width, height: Int

    def update(p: Point, c: Cell): Option[Grid]

    def cell(p: Point): Option[Cell]

    override def toString(): String =
      (0 until height)
        .map { y =>
          (0 until width).map { x =>
            cell(origin + Point(x, y)) match {
              case None => sys.error("appease the exhaustiveness checker")
              case Some(Cell.Air)  => '.'
              case Some(Cell.Rock) => '#'
              case Some(Cell.Sand) => 'o'
            }
          }.mkString
        }
        .mkString("\n")

  case class InfiniteGrid(
      origin: Point,
      width: Int,
      height: Int,
      cells: Vector[Cell]
  ) extends Grid {
    private val end: Point = origin + Point(width, height)

    override def update(p: Point, c: Cell): Option[InfiniteGrid] =
      index(p).map(i => copy(cells = cells.updated(i, c)))

    override def cell(p: Point): Option[Cell] =
      index(p).map(cells(_))

    def index(p: Point): Option[Int] =
      if p.x < origin.x || p.y < origin.y then None
      else if p.x >= end.x || p.y >= end.y then None
      else
        val p1 = p - origin
        Some(p1.y * width + p1.x)
  }

  object InfiniteGrid {
    def apply(tl: Point, br: Point, rocks: Vector[Polyline]): InfiniteGrid =
      val dims = (br - tl) + Point(1, 1)
      val grid =
        InfiniteGrid(tl, dims.x, dims.y, Vector.fill(dims.x * dims.y)(Cell.Air))
      rocks.foldLeft(grid) { case (g, rock) =>
        rock.path.foldLeft(g) { case (g, p) => g.update(p, Cell.Rock).get }
      }
  }

  case class FiniteGrid(
      origin: Point,
      end: Point,
      cells: Map[Point, Cell]
  ) extends Grid {
    private val dims: Point = end - origin
    val width = dims.x
    val height = dims.y

    override def cell(p: Point): Option[Cell] =
      if p.y < origin.y then None
      else if p.y >= end.y then None
      else if p.y == end.y - 1 then Some(Cell.Rock)
      else Some(cells(p))

    override def update(p: Point, c: Cell): Option[FiniteGrid] =
      if p.y < origin.y then None
      else if p.y >= end.y then None
      else
        val o = Point(Math.min(origin.x, p.x), Math.min(origin.y, p.y))
        val e = Point(Math.max(end.x, p.x), Math.max(end.y, p.y))

        Some(copy(origin = o, end = e, cells = cells.updated(p, c)))
  }

  object FiniteGrid {
    def apply(tl: Point, br: Point, rocks: Vector[Polyline]): FiniteGrid =
      val grid =
        FiniteGrid(tl, br + Point(1, 3), Map.empty.withDefaultValue(Cell.Air))
      rocks.foldLeft(grid) { case (g, rock) =>
        rock.path.foldLeft(g) { case (g, p) => g.update(p, Cell.Rock).get }
      }
  }

  sealed trait StepResult {}

  object StepResult:
    case class AtRest(p: Point) extends StepResult

    case class Falling(p: Point) extends StepResult

    case object OutOfBounds extends StepResult

  import Cell.*
  import StepResult.*

  override def solve1(s: Source): Int =
    val lines = parseInput(s)
    val spawn = Point(500, 0)
    val (min, max) =
      lines.flatMap(_.points).foldLeft((spawn, spawn)) { case ((min, max), p) =>
        Point(Math.min(min.x, p.x), Math.min(min.y, p.y)) ->
          Point(Math.max(max.x, p.x), Math.max(max.y, p.y))
      }

    val grid = InfiniteGrid(min, max, lines)
    runSimulation1(grid, spawn :: Nil, 0)

  override def solve2(s: Source): Int =
    val lines = parseInput(s)
    val spawn = Point(500, 0)
    val (min, max) =
      lines.flatMap(_.points).foldLeft((spawn, spawn)) { case ((min, max), p) =>
        Point(Math.min(min.x, p.x), Math.min(min.y, p.y)) ->
          Point(Math.max(max.x, p.x), Math.max(max.y, p.y))
      }

    val grid = FiniteGrid(min, max, lines)
    runSimulation2(grid, spawn :: Nil, 0)

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
  def runSimulation1(grid: Grid, stack: List[Point], atRest: Int): Int =
    stack match
      case Nil => sys.error("appease the exhaustiveness checker")
      case stack @ (head :: tail) =>
        runStep(grid, head) match {
          case OutOfBounds => atRest
          case AtRest(p) =>
            runSimulation1(grid.update(head, Sand).get, tail, atRest + 1)
          case Falling(p) =>
            runSimulation1(grid, p :: stack, atRest)
        }

  @tailrec
  def runSimulation2(grid: Grid, stack: List[Point], atRest: Int): Int =
    stack match
      case Nil => atRest
      case stack @ (head :: tail) =>
        runStep(grid, head) match {
          case OutOfBounds => sys.error("appease the exhaustiveness checker")
          case AtRest(p) =>
            runSimulation2(grid.update(head, Sand).get, tail, atRest + 1)
          case Falling(p) =>
            runSimulation2(grid, p :: stack, atRest)
        }

  private def parseInput(s: Source) =
    s.getLines().map(Polyline.parse).toVector
}
