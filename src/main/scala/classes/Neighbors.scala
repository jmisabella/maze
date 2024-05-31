package maze.classes

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
  override def toString(): String = 
    s"""north: ${north.getOrElse("NONE")}, east: ${east.getOrElse("NONE")}, south: ${south.getOrElse("NONE")}, west: ${west.getOrElse("NONE")}, northeast: ${northeast.getOrElse("NONE")}, southeast: ${southeast.getOrElse("NONE")}, southwest: ${southwest.getOrElse("NONE")}, northwest: ${northwest.getOrElse("NONE")}"""
}
