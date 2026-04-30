import java.nio.file.Paths

import scala.util.Try

import cats.effect._
import cats.effect.std.Queue
import fs2._
import fs2.io.file._

object Day1InputRotationsProcessing extends IOApp.Simple {

  sealed trait Rotation

  case class LeftRotation(distance: Int)  extends Rotation
  case class RightRotation(distance: Int) extends Rotation

  case class Position private (value: Int) extends AnyVal

  object Position {

    def p(value: Int): Position = value match {
      case v if v < 0    => Position(100 + value)
      case v if v >= 100 => Position(value % 100)
      case v             => Position(v)
    }

  }

  // 0 - 99
  // position %

  def rotate(position: Position, rotation: Rotation): Position =
    rotation match {
      case LeftRotation(distance)  => Position.p(position.value - (distance % 100))
      case RightRotation(distance) => Position.p(position.value + (distance % 100))
    }

  private def parseRotation(line: String): Option[Rotation] = {
    Try(
      line.splitAt(1) match {
        case ("R", distance) => RightRotation(distance.toInt)
        case ("L", distance) => LeftRotation(distance.toInt)
      }
    ).toOption
  }

  def positions(
    startingPosition: Position,
    filename: String
  ): fs2.Stream[IO, Position] =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .evalMap(s => IO(parseRotation(s)))
      .unNone
      .scan(startingPosition)(rotate)

  override def run: IO[Unit] =
    positions(Position(50), "day1.txt")
      .evalTap(p => IO.println(s"Position : ${p.value}"))
      .filter(p => p.value == 0)
      .compile
      .count
      .flatMap(count => IO.println(s"number of 0 position: $count"))

}
