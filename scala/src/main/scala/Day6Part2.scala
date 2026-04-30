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

object Day6Part2 extends IOApp.Simple {

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

  private val parseOperation: PartialFunction[Char, Operation] = {
    case '+' => Sum
    case '*' => Multiply
  }

  def numbers(
    filename: String
  ) =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_ != "")
      .map(_.toCharArray())
      .chunkAll
      .map(lines => lines.toArraySeq)
      .flatMap { lines =>
        val lineIndices   = 0.until(lines.head.size)
        val columnIndices = 0.until(lines.size - 1) // because of operations

        Stream
          .emits(lineIndices.toSeq.reverse)
          .map { i =>
            val line = columnIndices.map(j => lines(j)(i)).mkString.trim
            Either
              .cond(line == "", parseOperation(lines(columnIndices.last + 1)(i + 1)), line.toLong)
          } ++ Stream.emit(parseOperation(lines(columnIndices.last + 1)(0)).asRight[Long])

      }
      .fold((0L, List.empty[Long])) {
        case ((total, current), Left(long)) => (total, long :: current)
        case ((total, current), Right(op)) =>
          (total + op.monoid.combineAll(current), List.empty[Long])
      }

  val file = "day6.txt"

  override def run: IO[Unit] =
    numbers(file).compile.lastOrError.flatMap(IO.println)

}
