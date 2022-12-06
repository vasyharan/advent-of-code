package aoc2022

import scala.io.Source
import scala.collection.immutable.Queue

object Day06 extends aoc.Problem[Int] {
  override val year = 2022
  override val day = 6

  override lazy val results1 = 7 :: 5 :: 6 :: 10 :: 11 :: Nil
  override lazy val results2 = 19 :: 23 :: 23 :: 29 :: 26 :: Nil

  class RingBuffer[+A] private (val capacity: Int, val size: Int, q: Queue[A]) {
    assert(capacity > 0, "capacity must be > 0")

    def this(capacity: Int) = this(capacity, 0, Queue.empty)

    def push[B >: A](v: B): (RingBuffer[B], Option[A]) =
      if size == capacity then
        (RingBuffer[B](capacity, size, q.tail.enqueue(v)), Some(q.head))
      else (RingBuffer[B](capacity, size + 1, q.enqueue(v)), None)

    override def toString(): String = q.toString()
  }

  object RingBuffer {
    def empty[A](capacity: Int) = new RingBuffer[A](capacity)
  }

  override def solve1(s: Source): Int =
    findMarker(s.zipWithIndex, Map.empty, RingBuffer.empty(4))

  override def solve2(s: Source): Int =
    findMarker(s.zipWithIndex, Map.empty, RingBuffer.empty(14))

  private def findMarker(
      it: Iterator[(Char, Int)],
      s: Map[Char, Int],
      buf: RingBuffer[Char]
  ): Int =
    it.nextOption match
      case None => -1
      case Some(c -> i) =>
        val (buf1, c0) = buf.push(c)
        val s1 = c0.map(s.updatedWith(_)(_.map(_ - 1))).getOrElse(s)
        val s2 = s1.updatedWith(c)(_.map(_ + 1).orElse(Some(1)))
        val s3 = s2.filter((_, count) => count > 0)
        if s3.size < buf.capacity then findMarker(it, s3, buf1)
        else i + 1
}
