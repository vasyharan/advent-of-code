package aoc2022

import java.util.Comparator
import scala.annotation.tailrec
import scala.io.Source

object Day13 extends aoc.Problem[Int] {
  override val year = 2022
  override val day = 13
  override lazy val results1 = 13 :: Nil
  override lazy val results2 = 140 :: Nil

  sealed trait PacketData {}

  object PacketData:
    case class IntData(v: Int) extends PacketData {
      override def toString(): String = v.toString()
    }
    case class ListData(vs: List[PacketData]) extends PacketData {
      override def toString(): String = vs.toString
    }

    object IntData {
      implicit def ordering: Ordering[IntData] = Ordering.by(_.v)
    }

    object ListData {
      implicit def ordering: Ordering[ListData] = (left, right) =>
        @tailrec
        def compare(ls: List[PacketData], rs: List[PacketData]): Int =
          (ls, rs) match
            case (Nil, Nil)    => 0
            case (Nil, _ :: _) => -1
            case (_ :: _, Nil) => 1
            case (l :: ltail, r :: rtail) =>
              val cmp = PacketData.ordering.compare(l, r)
              if cmp == 0 then compare(ltail, rtail)
              else cmp

        compare(left.vs, right.vs)
    }

    implicit def ordering: Ordering[PacketData] = (left, right) =>
      (left, right) match
        case (left: IntData, right: IntData) =>
          IntData.ordering.compare(left, right)
        case (left: ListData, right: ListData) =>
          ListData.ordering.compare(left, right)
        case (left: IntData, right: ListData) =>
          ListData.ordering.compare(ListData(left :: Nil), right)
        case (left: ListData, right: IntData) =>
          ListData.ordering.compare(left, ListData(right :: Nil))

  import PacketData.*

  case class Packet(data: PacketData.ListData) {
    override def toString(): String = data.toString()
  }
  object Packet {
    implicit def ordering: Ordering[Packet] = (left, right) =>
      PacketData.ordering.compare(left.data, right.data)
  }

  override def solve1(s: Source) =
    parseInput(s)
      .grouped(2)
      .map { case Seq(a, b) => (a, b) }
      .zipWithIndex
      .filter { case ((p1, p2), _) =>
        ListData.ordering.compare(p1.data, p2.data) < 0
      }
      .map(_._2 + 1)
      .sum

  override def solve2(s: Source) =
    val divider1 = Packet(ListData(ListData(IntData(2) :: Nil) :: Nil))
    val divider2 = Packet(ListData(ListData(IntData(6) :: Nil) :: Nil))

    (divider1 :: divider2 :: parseInput(s).toList).sorted.zipWithIndex
      .filter { case ((p), _) => p == divider1 || p == divider2 }
      .map(_._2 + 1)
      .product

  private def parseInput(s: Source): Iterator[Packet] =
    def parseIntData(cs: List[Char], ss: List[Char]): (IntData, List[Char]) =
      cs match
        case Nil => (IntData(ss.reverse.mkString.toInt), cs.tail)
        case c :: tail =>
          if c >= '0' && c <= '9' then parseIntData(cs.tail, c :: ss)
          else (IntData(ss.reverse.mkString.toInt), cs)

    @tailrec
    def parseListData(
        cs: List[Char],
        vs: List[PacketData],
        stack: List[List[PacketData]]
    ): (ListData, List[Char]) =
      cs match
        case '[' :: tail => parseListData(tail, List.empty, vs :: stack)
        case ',' :: tail => parseListData(tail, vs, stack)
        case ']' :: tail =>
          val v: ListData = ListData(vs.reverse)
          if stack.isEmpty then v -> tail
          else parseListData(tail, v :: stack.head, stack.tail)
        case cs =>
          val (v, tail) = parseIntData(cs, List.empty)
          parseListData(tail, v :: vs, stack)

    def parsePacket(s: String): Packet =
      val cs = s.toList
      assert(cs.head == '[')
      val (vs, tail) = parseListData(cs.tail, List.empty, List.empty)
      assert(tail == Nil)
      Packet(vs)

    assert(
      parsePacket("[[1]]") ==
        Packet(ListData(ListData(IntData(1) :: Nil) :: Nil))
    )
    s.getLines()
      .filterNot(_.isBlank())
      .map(parsePacket)
}
