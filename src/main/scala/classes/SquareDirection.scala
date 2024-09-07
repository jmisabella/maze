package maze.classes

import maze.behaviors.Direction

object SquareDirection extends Enumeration with Direction {
  type SquareDirection = Value
  val North, East, South, West = Value
  
  def fromString(s: String): Option[SquareDirection] = values.find(_.toString == s)

}
