package maze.classes

import maze.behaviors.Cell
import maze.classes.{ Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.SquareDirection._
import play.api.libs.json.Json

case class SquareCell (
  coords: Coordinates, 
  // neighbors: SquareNeighbors = SquareNeighbors(), 
  neighborsByDirection: Map[String, Coordinates] = Map(),
  linked: Set[Coordinates] = Set(),
  distance: Int = 0,
  isStart: Boolean = false,
  isGoal: Boolean = false,
  onSolutionPath: Boolean = false, 
  visited: Boolean = false,
  value: String = "   "
) extends Cell() {

  // override type NEIGHBORS = SquareNeighbors

  // override type MAZE_TYPE = Square
  override def mazeType: MazeType = Square 
  
  // override def neighborCoords(): Seq[Coordinates] = (neighbors.north, neighbors.east, neighbors.south, neighbors.west) match {
  //   // 4 
  //   case (Some(n), Some(e), Some(s), Some(w)) => Seq(n, e, s, w)
  //   // 3 
  //   case (Some(n), Some(e), Some(s), None) => Seq(n, e, s)
  //   case (Some(n), Some(e), None, Some(w)) => Seq(n, e, w)
  //   case (Some(n), None, Some(s), Some(w)) => Seq(n, s, w)
  //   case (None, Some(e), Some(s), Some(w)) => Seq(e, s, w)
  //   // 2 
  //   case (Some(n), Some(e), None, None) => Seq(n, e)
  //   case (Some(n), None, Some(s), None) => Seq(n, s)
  //   case (Some(n), None, None, Some(w)) => Seq(n, w)
  //   case (None, Some(e), Some(s), None) => Seq(e, s)
  //   case (None, Some(e), None, Some(w)) => Seq(e, w)
  //   case (None, None, Some(s), Some(w)) => Seq(s, w)
  //   // 1
  //   case (Some(n), None, None, None) => Seq(n)
  //   case (None, Some(e), None, None) => Seq(e)
  //   case (None, None, Some(s), None) => Seq(s)
  //   case (None, None, None, Some(w)) => Seq(w)
  //   // 0 
  //   case (None, None, None, None) => Nil
  // }

  override def isLinked[SquareDirection](direction: SquareDirection): Boolean = direction match {
    // case North => neighbors.north.isDefined && isLinkedCoords(neighbors.north.get)
    // case East => neighbors.east.isDefined && isLinkedCoords(neighbors.east.get)
    // case South => neighbors.south.isDefined && isLinkedCoords(neighbors.south.get)
    // case West => neighbors.west.isDefined && isLinkedCoords(neighbors.west.get)
    case North => neighborsByDirection.get("north").isDefined && isLinkedCoords(neighborsByDirection.get("north"))
    case East => neighborsByDirection.get("east").isDefined && isLinkedCoords(neighborsByDirection.get("east"))
    case South => neighborsByDirection.get("south").isDefined && isLinkedCoords(neighborsByDirection.get("south"))
    case West => neighborsByDirection.get("west").isDefined && isLinkedCoords(neighborsByDirection.get("west"))
  }

  override def toString(): String = {
    val linkedCells: Seq[String] = 
      (linked.contains(neighborsByDirection.get("north").getOrElse(Coordinates(-1, -1))),
       linked.contains(neighborsByDirection.get("east").getOrElse(Coordinates(-1, -1))),
       linked.contains(neighborsByDirection.get("south").getOrElse(Coordinates(-1, -1))),
       linked.contains(neighborsByDirection.get("west").getOrElse(Coordinates(-1, -1))),
      ) match {
        case (true, true, true, true) => Seq("north","east","south","west")
        case (true, true, true, false) => Seq("north","east","south")
        case (true, true, false, true) => Seq("north","east","west")
        case (true, false, true, true) => Seq("north","south","west")
        case (false, true, true, true) => Seq("east","south","west")
        case (true, true, false, false) => Seq("north","east")
        case (true, false, true, false) => Seq("north","south")
        case (true, false, false, true) => Seq("north","west")
        case (false, true, true, false) => Seq("east","south")
        case (false, true, false, true) => Seq("east","west")
        case (false, false, true, true) => Seq("south","west")
        case (true, false, false, false) => Seq("north")
        case (false, true, false, false) => Seq("east")
        case (false, false, true, false) => Seq("south")
        case (false, false, false, true) => Seq("west")
        case (false, false, false, false) => Nil
      }
  (Json.obj(
      "coords" -> coords,
      "linked" -> linkedCells,
      "distance" -> distance,
      "isStart" -> isStart,
      "isGoal" -> isGoal, 
      "onSolutionPath" -> onSolutionPath//,
    )).toString()
  }
}

object SquareCell {
  def apply(x: Int, y: Int): SquareCell = SquareCell(coords = Coordinates(x, y)) // TODO: jmi: this appears to be root of bug where Cell mixes up coords
}
