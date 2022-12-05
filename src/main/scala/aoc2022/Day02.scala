package aoc2022

import scala.io.Source

object Day02 extends aoc.Problem[Long] {
  override val day = 2
  override val year = 2022
  override lazy val results1 = 15 :: Nil
  override lazy val results2 = 12 :: Nil

  enum RPS(val score: Int):
    case Rock extends RPS(1)
    case Paper extends RPS(2)
    case Scissors extends RPS(3)

    def beats: RPS = this match
      case Rock     => Scissors
      case Paper    => Rock
      case Scissors => Paper

  enum Result(val score: Int):
    case Loss extends Result(0)
    case Draw extends Result(3)
    case Win extends Result(6)

  case class Input(opponent: RPS, player: RPS)

  override def solve1(s: Source): Long = s
    .getLines()
    .map(_.split(" ", 2))
    .map { case Array(c1, c2) => Input(col1RPS(c1), col2RPS(c2)) }
    .map { rs => rs.player.score + calcResult(rs).score }
    .sum

  override def solve2(s: Source): Long = s
    .getLines()
    .map(_.split(" ", 2))
    .map { case Array(c1, c2) => (col1RPS(c1), col2RoundResult(c2)) }
    .map {
      case (rps, result @ Result.Draw) => result.score + rps.score
      case (rps, result @ Result.Loss) => result.score + rps.beats.score
      case (rps, result @ Result.Win)  => result.score + rps.beats.beats.score
    }
    .sum

  private def col1RPS(c: String): RPS = c match
    case "A" => RPS.Rock
    case "B" => RPS.Paper
    case "C" => RPS.Scissors

  private def col2RPS(c: String): RPS = c match
    case "X" => RPS.Rock
    case "Y" => RPS.Paper
    case "Z" => RPS.Scissors

  private def col2RoundResult(c: String): Result = c match
    case "X" => Result.Loss
    case "Y" => Result.Draw
    case "Z" => Result.Win

  private def calcResult(input: Input): Result =
    if input.player == input.opponent then Result.Draw
    else if input.opponent == input.player.beats then Result.Win
    else Result.Loss
}
