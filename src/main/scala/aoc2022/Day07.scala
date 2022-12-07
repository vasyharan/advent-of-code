package aoc2022

import scala.collection.immutable.{TreeMap, Vector}
import scala.io.Source
import scala.annotation.tailrec

object Day07 extends aoc.Problem[Long] {
  override val year = 2022
  override val day = 7
  override lazy val results1 = 95437 :: Nil
  override lazy val results2 = 24933642 :: Nil

  sealed trait FS:
    val name: String

  object FS {
    case class File(name: String, size: Int) extends FS

    case class Dir(
        name: String,
        entries: Map[String, FS] = Map.empty,
        size: Int = 0
    ) extends FS {
      def add(parent: List[String], entry: FS): Dir = parent match {
        case Nil => {
          val (nextEntries, entrySize) = entry match {
            case dir: Dir =>
              val updated = entries.updatedWith(entry.name) {
                case None                               => Some(entry)
                case Some(dir: Dir) if name == dir.name => Some(dir)
                case Some(e) =>
                  sys.error(s"conflicting entry=${entry} existing=${e}")
              }
              updated -> 0
            case file: File =>
              val updated = entries.updatedWith(entry.name) {
                case None                              => Some(entry)
                case Some(file: File) if file == entry => Some(file)
                case Some(e) =>
                  sys.error(s"conflicting entry=${entry} existing=${e}")
              }
              updated -> file.size
          }
          copy(entries = nextEntries, size = size + entrySize)
        }
        case p :: tail => {
          val nextEntries = entries.updatedWith(p) {
            case None           => Some(Dir(p).add(tail, entry))
            case Some(dir: Dir) => Some(dir.add(tail, entry))
            case Some(e) =>
              sys.error(s"conflicting entry=${entry} existing=${e}")
          }
          val entrySize = entry match
            case file: File => file.size
            case _: Dir     => 0
          copy(entries = nextEntries, size = size + entrySize)
        }
      }
    }

    def empty = Dir("", Map.empty)
  }

  override def solve1(s: Source): Long =
    val rootDir = parseInput(s)
    collectDirs(rootDir :: Nil, List.empty)
      .map(_.size)
      .filter(_ <= 100_000)
      .sum

  override def solve2(s: Source): Long =
    val rootDir = parseInput(s)
    val used = rootDir.size
    val unused = 70_000_000 - used
    val needed = 30_000_000 - unused
    collectDirs(rootDir :: Nil, List.empty).map(_.size).filter(_ >= needed).min

  @tailrec
  private def collectDirs(dirs: Seq[FS.Dir], acc: List[FS.Dir]): List[FS.Dir] =
    dirs match
      case Nil => acc
      case d :: tail =>
        val nextDirs = d.entries.values.collect { case dir: FS.Dir => dir }
        collectDirs(nextDirs.toSeq ++ tail, d :: acc)

  private def parseInput(s: Source): FS.Dir =
    val (entries, _) = s
      .getLines()
      .foldLeft(FS.empty -> List.empty[String]) { case ((fs, path), line) =>
        line.strip().split(" ") match
          case Array("$", "cd", "/")  => fs -> Nil
          case Array("$", "cd", "..") => fs -> path.tail
          case Array("$", "cd", arg)  => fs -> (arg :: path)
          case Array("$", "ls")       => fs -> path
          case Array("dir", name) =>
            fs.add(path.reverse, FS.Dir(name)) -> path
          case Array(size, name) =>
            fs.add(path.reverse, FS.File(name, size.toInt)) -> path
      }
    entries
}
