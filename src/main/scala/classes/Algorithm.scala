package maze.classes

import play.api.libs.json.{ Json, Format }

object Algorithm extends Enumeration {
  type Algorithm = Value
  val BinaryTree,
    Sidewinder = Value
  
  implicit val format: Format[Algorithm] = Json.formatEnum(this)
}
