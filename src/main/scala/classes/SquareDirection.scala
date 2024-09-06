package maze.classes

object SquareDirection extends Enumeration with maze.behaviors.IDirection {
  type SquareDirection = Value
  val North, East, South, West = Value
  
  def fromString(s: String): Option[SquareDirection] = values.find(_.toString == s)

}
