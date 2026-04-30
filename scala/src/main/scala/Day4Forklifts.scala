import java.nio.file.Paths

import scala.util.Try

import cats._
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Day4Forklifts extends IOApp.Simple {

  val RollOfPaper = 1
  val NoRollPaper = 0

  case class Line(places: Array[Int]) extends AnyVal

  implicit val rollOfPaper: Show[(Int, Int)] = Show {
    case (1, n) if n < 4 => "x"
    case (0, n)          => "."
    case (1, _)          => "@"
  }

  implicit val show: Show[Line] = Show(l => l.places.map(_.show).mkString)

  private def parseLine(line: String): Option[Line] = {
    Try(
      line
        .toCharArray
        .map {
          case '.' => NoRollPaper
          case '@' => RollOfPaper
          case _   => NoRollPaper // should not happen
        }
    ).toOption.filterNot(_.isEmpty).map(Line)
  }

  def place(index: Int, line: Line) =
    if (index < 0 || index >= line.places.size) NoRollPaper else line.places(index)

  def occupiedNeighours(
    aboveNeighbours: Line,
    current: Line,
    belowNeighbours: Line
  ) =
    current
      .places
      .zipWithIndex
      .map { case (currentRoll, index) =>
        currentRoll -> (
          place(index - 1, aboveNeighbours) + place(index, aboveNeighbours) + place(
            index + 1,
            aboveNeighbours
          ) +
            place(index - 1, current) + place(index + 1, current) +
            place(index - 1, belowNeighbours) + place(index, belowNeighbours) + place(
              index + 1,
              belowNeighbours
            )
        )

      }

  def grid(
    filename: String
  ): fs2.Stream[IO, Line] =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      // .evalTap(IO.println)
      .evalMap(line => IO(parseLine(line))).unNone

  val emptyLine = grid("day4.txt.sample")
    .take(1)
    .map(l => Line(Array.fill(l.places.size)(NoRollPaper)))

  override def run: IO[Unit] =
    (emptyLine ++ grid("day4.txt") ++ emptyLine)
      .sliding(3)
      .map(chunk => occupiedNeighours(chunk(0), chunk(1), chunk(2)))
      .evalTap(ns => IO.println(ns.toList))
      .flatMap(ns => fs2.Stream.emits(ns.filter(n => n._1 > 0).filter(n => n._2 < 4).toSeq))
      .compile
      .count
      .flatMap(count => IO.println(s"availble rolls count: $count"))

}

/*
 *
..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x.
 *
 * */
