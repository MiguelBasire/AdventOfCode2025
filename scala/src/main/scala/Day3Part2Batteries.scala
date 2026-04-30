import java.nio.file.Paths

import scala.util.Try

import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Day3Part2Batteries extends IOApp.Simple {

  case class Battery(value: Int) extends AnyVal
  object Battery {
    implicit val ord: Ordering[Battery] = Ordering.by(_.value)
  }

  case class Bank(batteries: Array[Battery]) {
    val size = batteries.size
  }

  private def parseBank(line: String): Option[Bank] = {
    Try(
      line.trim.grouped(1).map(b => Battery(b.toInt)).toArray
    ).toOption.filterNot(_.isEmpty).map(Bank)
  }

  private def maxPower(bank: Bank, digitNumber: Int): Double = {
    def power(batteries: Array[Battery], digitNumber: Int, currentPower: Double): Double = {

      digitNumber match {
        case 0 => currentPower
        case d =>
          val digitCount = d - 1
          val maxBattery = batteries.take(batteries.size - digitCount).max

          power(
            batteries.slice(batteries.indexOf(maxBattery) + 1, batteries.size),
            digitCount,
            currentPower + maxBattery.value * math.pow(10, digitCount)
          )
      }

    }

    power(bank.batteries, digitNumber, 0L)
  }

  def banks(
    filename: String
  ): fs2.Stream[IO, Bank] =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .evalTap(IO.println)
      .evalMap(bank => IO(parseBank(bank)))
      .unNone

  override def run: IO[Unit] =
    banks("day3.txt")
      .parEvalMapUnorderedUnbounded(bank => IO(maxPower(bank, 12).toLong))
      .evalTap(power => IO.println(power))
      .compile
      .foldMonoid
      .flatMap(sum => IO.println(s"max power of banks: $sum"))

}
