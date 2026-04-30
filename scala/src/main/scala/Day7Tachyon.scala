import java.nio.file.Paths

import scala.collection.mutable.ArraySeq
import scala.util.Try

import cats._
import cats.effect._
import cats.effect.std.Mutex
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Day7Tachyon extends IOApp.Simple {

  sealed trait ManifoldArea
  case object Beam     extends ManifoldArea
  case object Empty    extends ManifoldArea
  case object Splitter extends ManifoldArea

  implicit val show: Show[ManifoldArea] = Show[String].contramap {
    case Beam     => "|"
    case Empty    => "."
    case Splitter => "^"
  }

  def tachyonManifold(
    filename: String
  ) =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_ != "")
      .map[Array[ManifoldArea]](line =>
        line
          .toCharArray
          .map {
            case 'S' => Beam
            case '^' => Splitter
            case _   => Empty
          }
      )

  def propagateBeam: fs2.Pipe[IO, Array[ManifoldArea], List[ManifoldArea]] = areas =>
    areas
      .take(1)
      .flatMap(firstLine =>
        areas
          .drop(1)
          .mapAccumulate(firstLine) { case (previous, second) =>
            val updated = previous
              .zipWithIndex
              .zip(second)
              .foldLeft(ArraySeq.from(second)) {
                case (as, ((Beam, i), Splitter)) =>
                  as.update(i - 1, Beam)
                  as.update(i + 1, Beam)
                  as
                case (as, ((Beam, i), Empty)) =>
                  as.update(i, Beam)
                  as
                case (as, _) => as
              }
            (updated.toArray, updated.toList)
          }
          .map(_._2)
      )

  val file = "day7.txt.sample"

  override def run: IO[Unit] =
    tachyonManifold(file)
      .through(propagateBeam)
      .evalTap(areas => IO.println(show"${areas.map(_.show).mkString}"))
      .zipWithPrevious
      .collect { case (Some(prev), second) =>
        prev.zipWithIndex.count { case (a, index) => a == Beam && second(index) == Splitter }
      }
      // .evalTap(IO.println)
      .foldMonoid.compile.lastOrError.flatMap(IO.println)

}
