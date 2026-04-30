import java.nio.file.Paths

import scala.util.Try

import cats.effect._
import cats.effect.std.Queue
import fs2._
import fs2.io.file._

object Day2Part2InvalidIds extends IOApp.Simple {

  case class Range(a: Long, b: Long)

  private def parseRange(line: String): Option[Range] = {
    Try(
      line.split("-") match {
        case parts => Range(parts(0).toLong, parts(1).toLong)
      }
    ).toOption
  }

  private def isInvalid(id: Long) = {
    val stringId = id.toString
    val (a, b)   = stringId.splitAt(stringId.size / 2)
    a == b
  }

  def ranges(
    filename: String
  ): fs2.Stream[IO, Range] =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.string2char)
      .split(c => c == ',' || c == '\n')
      .evalTap(IO.println)
      .evalMap(id => IO(parseRange(id.toList.mkString)))
      .unNone

  override def run: IO[Unit] =
    ranges("day2.txt")
      .evalTap(r => IO.println(r))
      .flatMap(r => fs2.Stream.range(r.a, r.b + 1).filter(isInvalid).foldMonoid)
      .evalTap(s => IO.println(s"invalid sum: $s"))
      .compile
      .foldMonoid
      .flatMap(sum => IO.println(s"invalid ids sum: $sum"))

}
