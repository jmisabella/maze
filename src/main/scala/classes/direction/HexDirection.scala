package maze.classes.direction

object HexDirection extends Enumeration {
  type HexDirection = Value
  val NorthWest, North, NorthEast, SouthWest, South, SouthEast = Value
  
  def fromString(s: String): Option[HexDirection] = values.find(_.toString.toLowerCase == s.toLowerCase)
}
