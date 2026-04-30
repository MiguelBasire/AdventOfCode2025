import java.nio.file.Paths

import scala.util.Try

import cats._
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Day4Part2Forklifts extends IOApp.Simple {

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
      .evalMap(line => IO(parseLine(line)))
      .unNone

  def emptyLine(grid: fs2.Stream[IO, Line]) =
    grid.take(1).map(l => Line(Array.fill(l.places.size)(NoRollPaper)))

  def takePapers(remainingPapers: fs2.Stream[IO, Line]) =
    (emptyLine(remainingPapers) ++ remainingPapers ++ emptyLine(remainingPapers))
      .sliding(3)
      .map(chunk => occupiedNeighours(chunk(0), chunk(1), chunk(2)))
      .map(neighbours =>
        (
          neighbours.count(n => n._1 > 0 && n._2 < 4),
          Line(neighbours.map { case (r, ns) => if (ns < 4 && r > 0) 0 else r })
        )
      )
  // .evalTap(n => IO.println(n))

  def takeAllPapers(lastCount: Int, g: fs2.Stream[IO, Line]): IO[Int] = {

    val (count, nextGrid) = takePapers(g).unzip
    (count.foldMonoid.compile.lastOrError, nextGrid.compile.toList).flatMapN {
      case (0, _)    => IO.pure(lastCount)
      case (n, list) => takeAllPapers(n + lastCount, fs2.Stream.emits(list))
    }

  }

  override def run: IO[Unit] =
    takeAllPapers(0, grid("day4.txt"))
      .flatMap(count => IO.println(show"availble rolls count: $count"))

}
