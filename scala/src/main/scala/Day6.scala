import java.nio.file.Paths

import scala.collection.immutable.NumericRange
import scala.util.Try

import cats._
import cats.effect._
import cats.effect.std.Mutex
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Day6 extends IOApp.Simple {

  sealed trait Operation {
    val monoid: Monoid[Long]
  }

  case object Sum extends Operation {

    val monoid: Monoid[Long] = new Monoid[Long] {

      def combine(x: Long, y: Long): Long = x + y
      val empty                           = 0L

    }

  }
  case object Multiply extends Operation {

    val monoid: Monoid[Long] = new Monoid[Long] {

      def combine(x: Long, y: Long): Long = x * y
      val empty                           = 1L

    }

  }

  implicit val show: Show[Operation] = Show[String].contramap {
    case Sum      => "+"
    case Multiply => "x"
  }

  private def parseOperations(line: String): Option[Seq[Operation]] =
    line
      .toCharArray
      .filter(_ != ' ')
      .toSeq
      .traverse {
        case '+' => Some(Sum)
        case '*' => Some(Multiply)
        case _   => None
      }

  def operations(
    filename: String
  ) =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .evalTap(IO.println)
      .takeWhile(_ != "")
      .last
      .evalTap(last => IO.println(show"Last line : $last"))
      .evalMap(line => IO(line.flatMap(parseOperations)))
      .unNone

  private def parseNumbers(line: String): Option[Seq[Long]] =
    line
      .split(" +")
      .toSeq
      .traverse { n =>
        Either.catchNonFatal(n.toLong)
      }
      .toOption

  def numbers(
    filename: String
  ) =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .evalTap(IO.println)
      .takeWhile(_ != "")
      .evalMap(line => IO(parseNumbers(line.trim())))
      .unNone

  val file = "day6.txt"

  override def run: IO[Unit] =
    operations(file)
      .compile
      .lastOrError
      .flatMap { ops =>
        numbers(file)
          .fold(ops.map(_.monoid.empty))((acc, ns) =>
            acc
              .zipWithIndex
              .zip(ns)
              .map { case ((n1, index), n2) => ops(index).monoid.combine(n1, n2) }
          )
          .map(_.sum)
          .evalMap(IO.println)
          .compile
          .drain

      }

}
