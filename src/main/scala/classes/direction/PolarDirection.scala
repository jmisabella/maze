package maze.classes.direction

object PolarDirection extends Enumeration {
  type PolarDirection = Value
  val ClockWise, CounterClockWise, Inward, Outward = Value
  
  def fromString(s: String): Option[PolarDirection] = values.find(_.toString.toLowerCase == s.toLowerCase)
}

