package maze.classes

import maze.behaviors.Neighbors

import play.api.libs.json.{ Json, Format }

case class SquareNeighbors(
  north: Option[Coordinates] = None,
  east: Option[Coordinates] = None,
  south: Option[Coordinates] = None,
  west: Option[Coordinates] = None
) extends Neighbors {
  override def toSeq(): Seq[Coordinates] = Seq(north, east, south, west).filter(_.isDefined).map(_.get)
}

object SquareNeighbors {
  implicit val format: Format[SquareNeighbors] = Json.format[SquareNeighbors]
}


