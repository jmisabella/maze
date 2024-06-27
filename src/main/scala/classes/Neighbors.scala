package maze.classes

import play.api.libs.json.{ Json, Format }

case class Neighbors(
  north: Option[Coordinates] = None,
  east: Option[Coordinates] = None,
  south: Option[Coordinates] = None,
  west: Option[Coordinates] = None,
  northeast: Option[Coordinates] = None,
  southeast: Option[Coordinates] = None,
  southwest: Option[Coordinates] = None,
  northwest: Option[Coordinates] = None
)

object Neighbors {
  implicit val format: Format[Neighbors] = Json.format[Neighbors]
}