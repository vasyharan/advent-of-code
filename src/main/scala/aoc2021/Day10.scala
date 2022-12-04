package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends aoc.Problem {
  override val year = 2021
  override val day = 10
  override lazy val results1 = 26397 :: Nil
  override lazy val results2 = 288957 :: Nil

  override def solve1(s: Source) =
    s.getLines()
      .flatMap(firstIllegalChar)
      .map {
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
      }
      .sum

  override def solve2(s: Source) =
    val scores = s
      .getLines()
      .map(parseLine)
      .collect { case (mc, stack) if mc.isEmpty => stack }
      .map(closeOpens)
      .map { cs =>
        cs.foldLeft(0L) {
          case (acc, ')') => acc * 5 + 1
          case (acc, ']') => acc * 5 + 2
          case (acc, '}') => acc * 5 + 3
          case (acc, '>') => acc * 5 + 4
        }
      }
      .toVector
      .sorted
    assert(scores.length % 2 == 1)
    scores(scores.length / 2)

  private def firstIllegalChar(s: String): Option[Char] = {
    parseLine(s)._1
  }

  private def parseLine(s: String): (Option[Char], List[Char]) = {
    @tailrec
    def rec(cs: List[Char], stack: List[Char]): (Option[Char], List[Char]) =
      (cs, stack) match
        case (Nil, _)                    => (None, stack)
        case (c :: tail, _) if isOpen(c) => rec(tail, c :: stack)
        case (c :: tail, s :: nextStack) if isClose(c) =>
          if isMatching(s, c) then rec(tail, nextStack)
          else (Some(c), Nil)

    rec(s.toCharArray.toList, Nil)
  }

  private def closeOpens(opens: List[Char]): List[Char] = {
    @tailrec
    def rec(opens: List[Char], closes: List[Char]): List[Char] =
      opens match
        case Nil       => closes
        case o :: tail => rec(tail, closeOpen(o) :: closes)

    rec(opens, Nil).reverse
  }

  private def isOpen(c: Char): Boolean =
    c == '(' || c == '[' || c == '{' || c == '<'

  private def isClose(c: Char): Boolean =
    c == ')' || c == ']' || c == '}' || c == '>'

  private def closeOpen(open: Char): Char = open match
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'

  private def isMatching(open: Char, close: Char): Boolean =
    close == closeOpen(open)
}
