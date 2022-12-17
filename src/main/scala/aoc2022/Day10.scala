package aoc2022

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends aoc.Problem[String] {
  override val year = 2022
  override val day = 10

  override lazy val results1 = "13140" :: Nil
  override lazy val results2 =
    """##..##..##..##..##..##..##..##..##..##..
      |###...###...###...###...###...###...###.
      |####....####....####....####....####....
      |#####.....#####.....#####.....#####.....
      |######......######......######......####
      |#######.......#######.......#######.....""".stripMargin :: Nil

  sealed trait Instruction {}

  object Instruction:
    case object Noop extends Instruction
    case class Addx(v: Int) extends Instruction

  case class CPU(clock: Int = 1, registerX: Int = 1) {
    import Instruction.*
    def execute(i: Instruction): CPU = i match
      case Noop =>
        copy(clock = clock + 1)
      case Addx(v) =>
        copy(clock = clock + 2, registerX = registerX + v)
  }

  import Instruction.*
  override def solve1(s: Source): String =
    val instructions = parseInstructions(s)
    val cpu = CPU()

    @tailrec
    def runAndRecord(
        cpu: CPU,
        is: List[Instruction],
        ats: List[Int],
        ss: List[CPU]
    ): List[CPU] =
      (ats, is) match
        case _ -> Nil => ss.reverse
        case Nil -> _ => ss.reverse
        case (at :: atTail) -> (i :: iTail) =>
          val nextCpu = cpu.execute(i)
          val (nextAts, nextSs) =
            if at >= cpu.clock && at < nextCpu.clock then (atTail, cpu :: ss)
            else (ats, ss)
          runAndRecord(cpu.execute(i), iTail, nextAts, nextSs)

    val at = List(20, 60, 100, 140, 180, 220)
    val ss = runAndRecord(cpu, instructions, at, List.empty)
    at.zip(ss).map((i, cpu) => i * cpu.registerX).sum.toString

  override def solve2(s: Source): String =
    val instructions = parseInstructions(s)
    val cpu = CPU()

    @tailrec
    def computeDisplay(
        cpu: CPU,
        is: List[Instruction],
        pixels: List[Boolean]
    ): List[Boolean] =
      if cpu.clock > 240 then pixels.reverse
      else
        val nextCpu = is.headOption.map(cpu.execute(_)).getOrElse(cpu)
        val sprite = Range.inclusive(cpu.registerX - 1, cpu.registerX + 1)
        val clock = Range((cpu.clock - 1), (nextCpu.clock - 1))
        val pps = clock
          .map(c => sprite.contains(c % 40))
          .toList
          .reverse
        computeDisplay(nextCpu, is.tail, pps ++ pixels)

    computeDisplay(cpu, instructions, List.empty)
      .map(on => if on then "#" else ".")
      .grouped(40)
      .map(_.mkString)
      .mkString("\n")

  private def parseInstructions(s: Source): List[Instruction] =
    s.getLines()
      .map(_.split(" "))
      .map {
        case Array("noop")    => Noop
        case Array("addx", v) => Addx(v.toInt)
      }
      .toList
}
