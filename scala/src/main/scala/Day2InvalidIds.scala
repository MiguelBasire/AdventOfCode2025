import java.nio.file.Paths

import scala.util.Try

import cats.effect._
import cats.effect.std.Queue
import fs2._
import fs2.io.file._

object InvalidIdsPart2 extends IOApp.Simple {

  case class Range(a: Long, b: Long)

  private def parseRange(line: String): Option[Range] = {
    Try(
      line.split("-") match {
        case parts => Range(parts(0).toLong, parts(1).toLong)
      }
    ).toOption
  }

  private def repeatedPattern(size: Int)(stringId: String) = {
    val groups = stringId.grouped(size).toList
    groups.size >= 2 && groups.toSet.size == 1
  }

  private def isInvalid(id: Long) = {
    val stringId = id.toString
    repeatedPattern(1)(stringId) ||
    repeatedPattern(2)(stringId) ||
    repeatedPattern(3)(stringId) ||
    repeatedPattern(4)(stringId) ||
    repeatedPattern(5)(stringId)

  }

  def ranges(
    filename: String
  ): fs2.Stream[IO, Range] =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.string2char)
      .split(c => c == ',' || c == '\n')
      .evalMap(id => IO(parseRange(id.toList.mkString)))
      .unNone

  override def run: IO[Unit] =
    ranges("day2.txt")
      .evalTap(r => IO.println(r))
      .parEvalMapUnorderedUnbounded(r =>
        fs2.Stream.range[IO, Long](r.a, r.b + 1).filter(isInvalid).foldMonoid.compile.foldMonoid
      )
      .compile
      .foldMonoid
      .flatMap(sum => IO.println(s"invalid ids sum: $sum"))

}
