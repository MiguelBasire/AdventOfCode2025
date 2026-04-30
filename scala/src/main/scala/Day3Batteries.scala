import java.nio.file.Paths

import scala.util.Try

import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Batteries extends IOApp.Simple {

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

  private def maxPower(bank: Bank) = {
    val maxBattery    = bank.batteries.take(bank.size - 1).max
    val secondBattery = bank.batteries.slice(bank.batteries.indexOf(maxBattery) + 1, bank.size).max

    maxBattery.value * 10 + secondBattery.value
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
      .evalTap(power => IO.println(power))
      .parEvalMapUnorderedUnbounded(bank => IO(maxPower(bank)))
      .compile
      .foldMonoid
      .flatMap(sum => IO.println(s"max power of banks: $sum"))

}
