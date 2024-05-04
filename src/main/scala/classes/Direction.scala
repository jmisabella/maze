package maze.classes

import play.api.libs.json.{ Json, Format }

object Direction extends Enumeration {
  type Direction = Value
  val North, South, East, West = Value

  implicit val format: Format[Direction] = Json.formatEnum(this)

  def fromString(s: String): Option[Value] = values.find(_.toString == s)

}
