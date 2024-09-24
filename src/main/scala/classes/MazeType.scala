package maze.classes

import play.api.libs.json.{ Json, Format }

object MazeType extends Enumeration {
  type MazeType = Value
  val Square, Polar = Value
  
  implicit val format: Format[MazeType] = Json.formatEnum(this)

  def fromString(s: String): Option[MazeType] = values.find(_.toString.toLowerCase == s.toLowerCase())

}
