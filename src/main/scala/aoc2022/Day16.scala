package aoc2022

import scala.io.Source

object Day16 extends aoc.Problem[Int] {
  override val year: Int = 2022
  override val day: Int = 16
  override lazy val results1: List[Int] = 1651 :: Nil
  override lazy val results2: List[Int] = 1707 :: Nil

  case class Room(valve: String, flowRate: Int, edges: Set[String])

  case class Cave(rooms: Map[String, Room])

  case class Matrix(ds: Vector[Vector[Double]], imap: Map[String, Int]) {
    private val rmap = imap.map(_.swap)

    def apply(i: String, j: String) = ds(imap(i))(imap(j))

    def updated(i: String, j: String, d: Double): Matrix =
      val ii = imap(i)
      val jj = imap(j)
      copy(ds = ds.updated(ii, ds(ii).updated(jj, d)))

    override def toString(): String =
      val size = imap.size
      val rows = (0 until size)
        .map { i =>
          val row = (0 until size)
            .map { j =>
              ds(i)(j) match {
                case Double.PositiveInfinity => " Inf"
                case d                       => f"${d}%4.1f"
              }
            }
          (f"${rmap(i)}%4s" +: row).mkString(" ")
        }
      val header =
        (f"${""}%4s" +: (0 until size).map(i => f"${rmap(i)}%4s")).mkString(" ")
      (header +: rows).mkString("\n")
  }

  object Matrix {
    def apply(c: Cave, initial: Double): Matrix =
      val imap = c.rooms.keySet.toList.sorted.zipWithIndex.toMap
      val ds = Vector.fill(imap.size, imap.size)(initial)
      new Matrix(ds, imap)
  }

  case class SearchState(
      remaining: Int,
      room: String,
      open: Set[String],
      pressure: Int
  )

  def solve1(s: Source): Int =
    val cave = parseInput(s)
    val flowRooms = cave.rooms.filter((k, v) => v.flowRate > 0)
    val dm = calcDistances(cave)

    def edges(s: SearchState): Vector[SearchState] = {
      val r1 = cave.rooms(s.room)
      flowRooms
        .filterNot { (k, _) => s.open.contains(k) }
        .map((k, r) => {
          val remaining = s.remaining - dm(s.room, k).toInt - 1
          SearchState(
            remaining = remaining,
            room = k,
            open = s.open + k,
            pressure = s.pressure + r.flowRate * remaining
          )
        })
        .filter { _.remaining > 0 }
        .toVector
    }

    def search(s: Seq[SearchState], b: SearchState): SearchState =
      s match {
        case Seq() => b
        case head +: tail =>
          val nextBest = if head.pressure > b.pressure then head else b
          search(edges(head) ++ tail, nextBest)
      }

    val initial = SearchState(30, "AA", Set.empty, 0)
    val best = search(Vector(initial), initial)
    best.pressure

  def solve2(s: Source): Int = ???

  private def parseInput(s: Source) =
    def parseLine(valve: String, flowRate: String, leadsTo: String) =
      val edgeLabels = leadsTo.split(",").map(_.strip).toSet
      (valve, Room(valve, flowRate.toInt, edges = edgeLabels))

    val rooms = s
      .getLines()
      .map {
        case s"Valve $valve has flow rate=$flowRate; tunnels lead to valves $leadsTo" =>
          parseLine(valve, flowRate, leadsTo)
        case s"Valve $valve has flow rate=$flowRate; tunnel leads to valve $leadsTo" =>
          parseLine(valve, flowRate, leadsTo)
      }
    Cave(rooms.toMap)

  def calcDistances(c: Cave): Matrix =
    // Floyd-Warshall
    val rooms = c.rooms.keySet
    val imap = rooms.toList.sorted.zipWithIndex.toMap
    val m1 = Matrix(c, Double.PositiveInfinity)
    val m2 = rooms.foldLeft(m1) { case (m, r1) =>
      rooms.foldLeft(m) { case (m, r2) =>
        if r1 == r2 then m.updated(r1, r2, 0)
        else if c.rooms(r1).edges.contains(r2) then m.updated(r1, r2, 1)
        else m
      }
    }
    rooms.foldLeft(m2) { case (m, k) =>
      rooms.foldLeft(m) { case (m, i) =>
        rooms.foldLeft(m) { case (m, j) =>
          val d = m(i, k) + m(k, j)
          if m(i, j) > d then {
            m.updated(i, j, d)
          } else {
            m
          }
        }
      }
    }
}
