package maze.classes

import play.api.libs.json.{ Json, Format }

object Algorithm extends Enumeration {
  type Algorithm = Value
  val BinaryTree,
    Sidewinder,
    AldousBroder = Value
  
  implicit val format: Format[Algorithm] = Json.formatEnum(this)

  def fromString(s: String): Option[Algorithm] = values.find(_.toString.toLowerCase == s.toLowerCase())

}
