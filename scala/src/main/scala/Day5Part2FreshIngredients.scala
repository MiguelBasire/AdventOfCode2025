import java.nio.file.Paths

import scala.collection.immutable.NumericRange
import scala.util.Try

import cats._
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Day5Part2FreshIngredients extends IOApp.Simple {

  type IngredientId = Long

  case class FreshIngredientsIds(head: IngredientId, last: IngredientId) {
    def contains(id: IngredientId) = head <= id && id <= last
  }

  object FreshIngredientsIds {

    def mergedOnOverlap(
      ids1: FreshIngredientsIds,
      ids2: FreshIngredientsIds
    ): List[FreshIngredientsIds] =
      if (ids1.contains(ids2.head) || ids1.contains(ids2.last))
        List(
          FreshIngredientsIds(
            Math.min(ids1.head, ids2.head),
            Math.max(ids1.last, ids2.last)
          )
        )
      else
        List(ids2, ids1)

  }

  private def parseFreshIngredientIds(line: String): Option[FreshIngredientsIds] =
    Either
      .catchNonFatal(line.split('-').map(_.toLong))
      .map(ids => FreshIngredientsIds(ids(0), ids(1)))
      .toOption

  def freshIngredientsIdsRanges(
    filename: String
  ): fs2.Stream[IO, FreshIngredientsIds] =
    Files[IO]
      .readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .takeWhile(_.nonEmpty)
      .evalMap(line => IO(parseFreshIngredientIds(line)))
      .unNone

  def merge(chunks: Chunk[FreshIngredientsIds]) = {
    val ordered = chunks.toList.sortBy(_.head)
    def m(
      toMerge: List[FreshIngredientsIds],
      merged: List[FreshIngredientsIds]
    ): List[FreshIngredientsIds] =
      (toMerge, merged) match {
        case (ids :: remainingIds, lastMerged :: alreadyMergedIds) =>
          m(
            remainingIds,
            FreshIngredientsIds.mergedOnOverlap(lastMerged, ids) ::: alreadyMergedIds
          )
        case (ids :: remainingIds, Nil) => m(remainingIds, List(ids))
        case (Nil, merged)              => merged.reverse

      }

    m(ordered, List.empty)
  }

  def isFresh(id: IngredientId, freshIds: List[FreshIngredientsIds]) = freshIds
    .exists(_.contains(id))

  val file = "day5.txt"

  override def run: IO[Unit] =
    freshIngredientsIdsRanges(file)
      .chunkAll
      .map(merge)
      .map(ids => ids.map(r => r.last - r.head + 1).sum)
      .foldMonoid
      .compile
      .lastOrError
      .flatMap(count => IO.println(show"Number of fresh ids: $count"))

}
