package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import cats.data.NonEmptyList

object Day09 extends aoc.Problem[Long] {
  override val year = 2022
  override val day = 9

  override lazy val results1: List[Long] = 13 :: Nil
  override lazy val results2: List[Long] = 1L :: 36L :: Nil

  enum Direction:
    case Up, Right, Down, Left

    def delta(): Pos = this match
      case Up    => Pos(0, 1)
      case Right => Pos(1, 0)
      case Left  => Pos(-1, 0)
      case Down  => Pos(0, -1)

  object Direction:
    def fromString(s: String): Direction = s match
      case "U" => Up
      case "R" => Right
      case "D" => Down
      case "L" => Left

  case class Pos(x: Int, y: Int) {
    def delta(p: Pos): Pos = Pos(x - p.x, y - p.y)
    def plus(p: Pos): Pos = Pos(x + p.x, y + p.y)
    def abs: Pos = Pos(Math.abs(x), Math.abs(y))
  }

  case class Move(dir: Direction, count: Int) {
    def decr(): Move = copy(count = count - 1)
  }

  override def solve1(s: Source): Long =
    val moves = parseMoves(s)
    val ps = (0 until 2).map(_ => Pos(0, 0)).toList
    updateMoves(moves, ps, Set(Pos(0, 0))).size

  override def solve2(s: Source): Long =
    val moves = parseMoves(s)
    val ps = (0 until 10).map(_ => Pos(0, 0)).toList
    updateMoves(moves, ps, Set(Pos(0, 0))).size

  private def parseMoves(s: Source): List[Move] = s
    .getLines()
    .map(_.split(" ", 2))
    .map { case Array(dir, count) =>
      Move(Direction.fromString(dir), count.toInt)
    }
    .toList

  @tailrec
  def updateMoves(moves: List[Move], ps: List[Pos], v: Set[Pos]): Set[Pos] =
    moves match
      case Nil => v
      case move :: tail if move.count == 0 =>
        updateMoves(tail, ps, v)
      case move :: tail =>
        val (nextPs, tp) =
          updateTail(Nil, NonEmptyList(ps.head.plus(move.dir.delta()), ps.tail))
        updateMoves(move.decr() :: tail, nextPs, v + tp)

  @tailrec
  private def updateTail(
      hps: List[Pos],
      tps: NonEmptyList[Pos]
  ): (List[Pos], Pos) =
    (hps, tps) match
      case Nil -> NonEmptyList(tp, tail) =>
        updateTail(tp :: Nil, NonEmptyList.fromListUnsafe(tail))
      case _ -> NonEmptyList(tp, tail) =>
        val nextTp = updateTailPos(hps.head, tp)
        if tail == Nil then (nextTp :: hps).reverse -> nextTp
        else updateTail(nextTp :: hps, NonEmptyList.fromListUnsafe(tail))

  private def updateTailPos(hp: Pos, tp: Pos): Pos =
    val delta = hp.delta(tp)
    val absDelta = delta.abs
    if absDelta == Pos(0, 0) || absDelta == Pos(1, 1) ||
      absDelta == Pos(1, 0) || absDelta == Pos(0, 1)
    then tp
    else if absDelta.y == 0 && absDelta.x == 2 then
      Pos(tp.x + (if delta.x < 0 then -1 else 1), tp.y)
    else if absDelta.x == 0 && absDelta.y == 2 then
      Pos(tp.x, tp.y + (if delta.y < 0 then -1 else 1))
    else if (absDelta.y < 3 && absDelta.x < 3)
    then
      Pos(
        tp.x + (if delta.x < 0 then -1 else 1),
        tp.y + (if delta.y < 0 then -1 else 1)
      )
    else sys.error("tail is too far behind!")

  // horizontal
  assert(updateTailPos(Pos(0, 0), Pos(0, 0)) == Pos(0, 0))
  assert(updateTailPos(Pos(1, 0), Pos(0, 0)) == Pos(0, 0))
  assert(updateTailPos(Pos(2, 0), Pos(0, 0)) == Pos(1, 0))
  assert(updateTailPos(Pos(0, 0), Pos(2, 0)) == Pos(1, 0))

  // vertical
  assert(updateTailPos(Pos(0, 0), Pos(0, 0)) == Pos(0, 0))
  assert(updateTailPos(Pos(0, 1), Pos(0, 0)) == Pos(0, 0))
  assert(updateTailPos(Pos(0, 2), Pos(0, 0)) == Pos(0, 1))
  assert(updateTailPos(Pos(0, 0), Pos(0, 2)) == Pos(0, 1))

  // diagonal
  assert(updateTailPos(Pos(1, 1), Pos(0, 0)) == Pos(0, 0))
  assert(updateTailPos(Pos(2, 1), Pos(0, 0)) == Pos(1, 1))
  assert(updateTailPos(Pos(2, 2), Pos(0, 0)) == Pos(1, 1))
}
