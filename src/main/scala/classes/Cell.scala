package maze.classes

import maze.classes.Direction._

case class Coordinates(x: Int, y: Int)

case class Neighbors(
  north: Option[Coordinates] = None,
  east: Option[Coordinates] = None,
  south: Option[Coordinates] = None,
  west: Option[Coordinates] = None
)

case class Cell(
  coords: Coordinates, 
  neighbors: Neighbors = Neighbors(), 
  linked: Set[Coordinates] = Set() 
) {
  def link(coordinates: Option[Coordinates]): Cell = coordinates match {
    case None => this
    case Some(c) => this.copy(linked = this.linked + c)
  }
  def unlink(coordinates: Option[Coordinates]): Cell = coordinates match {
    case None => this
    case Some(c) => this.copy(linked = this.linked - c)
  }
  def isLinked(coordinates: Option[Coordinates]): Boolean = coordinates match {
    case None => false
    case Some(c) => linked.contains(c)
  }
}
