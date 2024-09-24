package maze.classes

import maze.behaviors.Cell
import maze.classes.{ Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.SquareDirection._
import play.api.libs.json.Json

case class SquareCell (
  coords: Coordinates, 
  neighborsByDirection: Map[String, Coordinates] = Map(),
  linked: Set[Coordinates] = Set(),
  distance: Int = 0,
  isStart: Boolean = false,
  isGoal: Boolean = false,
  onSolutionPath: Boolean = false, 
  visited: Boolean = false,
  value: String = "   "
) extends Cell() {

  override def mazeType: MazeType = Square

  override def neighbors[D <: Enumeration#Value](direction: D): Seq[Coordinates] = direction match {
    case North => Seq(neighborsByDirection("north"))
    case East => Seq(neighborsByDirection("east"))
    case South => Seq(neighborsByDirection("south"))
    case West => Seq(neighborsByDirection("west"))
    case d => throw new IllegalArgumentException(s"Unexpected direction [$d] for SquareCell; expecting one of [North, East, South West]")
  }

  override def isLinked[SquareDirection](direction: SquareDirection): Boolean = direction match {
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
  def apply(x: Int, y: Int): SquareCell = SquareCell(coords = Coordinates(x, y))
}
