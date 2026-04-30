import java.nio.file.Paths

import scala.collection.mutable.ArraySeq
import scala.collection.SortedSet
import scala.util.Try

import cats._
import cats.effect._
import cats.effect.std.Mutex
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Day8JunctionBoxes extends IOApp.Simple {

  case class JunctionBox(x: Int, y: Int, z: Int)

  case class Circuit(boxes: Set[JunctionBox]) {

    def contains(box: JunctionBox)             = boxes.contains(box)
    def containsAtLeastOneOf(circuit: Circuit) = boxes.intersect(circuit.boxes).size > 0

  }

  object Circuit {

    implicit val monoidCircuit: Monoid[Circuit] = Monoid
      .instance(Circuit(Set.empty), (c1, c2) => Circuit(c1.boxes |+| c2.boxes))

  }

  def distance2(box1: JunctionBox, box2: JunctionBox) =
    Math.pow(box1.x - box2.x, 2) +
      Math.pow(box1.y - box2.y, 2) +
      Math.pow(box1.z - box2.z, 2)

  def junctionBoxes(
    filename: String
  ) =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_ != "")
      .map(line => line.split(','))
      .map(coordinates =>
        JunctionBox(coordinates(0).toInt, coordinates(1).toInt, coordinates(2).toInt)
      )

  val file = "day8.txt.sample"

  override def run: IO[Unit] =
    junctionBoxes(file)
      .chunkAll
      .map(boxes =>
        (boxes, boxes)
          .flatMapN((b1, b2) =>
            Option.when(b1 != b2)(distance2(b1, b2)).map(d => Chunk(Set(b1, b2) -> d)).orEmpty
          )
          .toList
          .sortBy(_._2)
          .map(_._1)
          .distinct
          .map(Circuit)
      )
      .flatMap(fs2.Stream.emits)
      .evalTap(IO.println)
      .fold(List.empty[Circuit]) { (cs, circuit) =>
        // println(cs)
        val alreadyConnected = circuit.boxes.count(box => cs.exists(_.contains(box)))
        val index1           = cs.indexWhere(c => c.containsAtLeastOneOf(circuit))
        // println(s"alreadyConnected : $alreadyConnected")

        if (alreadyConnected == 2) cs
        else if (index1 == -1)
          cs.appended(circuit)
        else
          cs.updated(index1, cs(index1) |+| circuit)

      }
      .evalTap(IO.println)
      .map(_.map(_.boxes.size).sorted.reverse)
      .evalTap(IO.println)
      .map(_.take(3).fold(1)(_ * _))
      .compile
      .lastOrError
      .flatMap(IO.println)

}
