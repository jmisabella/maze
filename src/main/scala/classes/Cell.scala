package maze.classes

case class Coordinates(x: Int, y: Int) {
  override def toString(): String = s"($x,$y)"
}

case class Neighbors(
  north: Option[Coordinates] = None,
  east: Option[Coordinates] = None,
  south: Option[Coordinates] = None,
  west: Option[Coordinates] = None
) {
  override def toString(): String = 
    s"""north: ${north.getOrElse("NONE")}, east: ${east.getOrElse("NONE")}, south: ${south.getOrElse("NONE")}, west: ${west.getOrElse("NONE")}"""
}

case class Cell(
  coords: Coordinates, 
  neighbors: Neighbors = Neighbors(), 
  linked: Set[Coordinates] = Set(),
  visited: Boolean = false
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
  
  override def toString(): String = 
    s"""coords: $coords
visited: ${visited}
neighbors: ${neighbors}
linked: ${linked}
"""
}

object Cell {
  def apply(row: Int, column: Int): Cell = Cell(coords = Coordinates(row, column))
}
