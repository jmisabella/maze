package maze.classes

import play.api.libs.json.{ Json, Format }

object MazeType extends Enumeration {
  type MazeType = Value
  // val Square, Polar = Value
  // TODO: change maze-web accordingly: instead of Square, it should be Orthogonal; Sigma is maze with hexagon cells; Delta is maze with trianglular cells
  val Orthogonal, Sigma, Delta, Polar = Value

  implicit val format: Format[MazeType] = Json.formatEnum(this)

  def fromString(s: String): Option[MazeType] = values.find(_.toString.toLowerCase == s.toLowerCase())

}
