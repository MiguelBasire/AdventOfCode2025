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

object Day7Part2Tachyon extends IOApp.Simple {

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

  def counters(line: Array[ManifoldArea]) = line.map {
    case Beam     => 1L
    case Splitter => 0L
    case Empty    => 0L
  }

  def propagateBeam: fs2.Pipe[IO, Array[ManifoldArea], Array[Long]] = areas =>
    areas
      .take(1)
      .flatMap(firstLine =>
        areas
          .drop(1)
          .fold(ArraySeq.from(counters(firstLine))) { case (beams, second) =>
            second
              .zipWithIndex
              .foldLeft(beams) {
                case (bs, (Splitter, 0)) =>
                  bs.update(1, bs(0) + bs(1))
                  bs.update(0, 0)
                  bs

                case (bs, (Splitter, i)) if i == beams.size - 1 =>
                  bs.update(i - 1, bs(i - 1) + bs(i))
                  bs.update(i, 0)
                  bs

                case (bs, (Splitter, i)) =>
                  bs.update(i - 1, bs(i - 1) + bs(i))
                  bs.update(i + 1, bs(i + 1) + bs(i))
                  bs.update(i, 0)
                  bs

                case (bs, _) => bs
              }
          }
      )
      .map(_.toArray)

  val file = "day7.txt"

  override def run: IO[Unit] =
    tachyonManifold(file)
      .through(propagateBeam)
      .evalTap(IO.println)
      .flatMap(fs2.Stream.emits(_))
      .foldMonoid
      .compile
      .lastOrError
      .flatMap(IO.println)

}
