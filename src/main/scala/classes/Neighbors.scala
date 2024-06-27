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
) {
  override def toString(): String = (north, east, south, west) match {
    case (None, None, None, None) => "[]"
    case (Some(n), None, None, None) => s"""["north"]"""
    case (None, Some(e), None, None) => s"""["east"]"""
    case (None, None, Some(s), None) => s"""["south"]"""
    case (None, None, None, Some(w)) => s"""["west"]"""
    case (Some(n), Some(e), None, None) => s"""["north","east"]"""
    case (Some(n), None, Some(s), None) => s"""["north","south"]"""
    case (Some(n), None, None, Some(w)) => s"""["north","west"]"""
    case (None, Some(e), Some(s), None) => s"""["east","south"]"""
    case (None, Some(e), None, Some(w)) => s"""["east","west"]"""
    case (None, None, Some(s), Some(w)) => s"""["south","west"]"""
    case (Some(n), Some(e), Some(s), None) => s"""["north","east","south"]"""
    case (Some(n), Some(e), None, Some(w)) => s"""["north","east","west"]"""
    case (Some(n), None, Some(s), Some(w)) => s"""["north","south","west"]"""
    case (None, Some(e), Some(s), Some(w)) => s"""["east","south","west"]"""
    case (Some(n), Some(e), Some(s), Some(w)) => s"""["north","east","south","west"]"""
  }

  // override def toString(): String = 
  //   s"""north: ${north.getOrElse("NONE")}, east: ${east.getOrElse("NONE")}, south: ${south.getOrElse("NONE")}, west: ${west.getOrElse("NONE")}, northeast: ${northeast.getOrElse("NONE")}, southeast: ${southeast.getOrElse("NONE")}, southwest: ${southwest.getOrElse("NONE")}, northwest: ${northwest.getOrElse("NONE")}"""
}

object Neighbors {
  implicit val format: Format[Neighbors] = Json.format[Neighbors]
}