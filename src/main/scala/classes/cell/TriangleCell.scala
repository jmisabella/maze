package maze.classes.cell

import maze.behaviors.Cell
import maze.classes.{ Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.direction.TriangleDirection._
import play.api.libs.json.{ Json, Format }

object TriangleOrientation extends Enumeration {
  type TriangleOrientation = Value
  val Upward, Downward = Value
  
  implicit val format: Format[TriangleOrientation] = Json.formatEnum(this)

  def fromString(s: String): Option[TriangleOrientation] = values.find(_.toString.toLowerCase == s.toLowerCase())
}
import TriangleOrientation._


case class TriangleCell (
  coords: Coordinates, 
  neighborsByDirection: Map[String, Coordinates] = Map(),
  linked: Set[Coordinates] = Set(),
  distance: Int = 0,
  isStart: Boolean = false,
  isGoal: Boolean = false,
  onSolutionPath: Boolean = false, 
  visited: Boolean = false,
  value: String = "   ",
  orientation: TriangleOrientation = Upward
) extends Cell() {

  override def mazeType: MazeType = Delta

  override def neighbors[D <: Enumeration#Value](direction: D): Seq[Coordinates] = direction match {
    case UpperLeft => Seq(neighborsByDirection("upperleft"))
    case UpperRight => Seq(neighborsByDirection("upperright"))
    case Down => Seq(neighborsByDirection("down"))
    case Up => Seq(neighborsByDirection("up"))
    case LowerLeft => Seq(neighborsByDirection("lowerleft"))
    case LowerRight => Seq(neighborsByDirection("lowerright"))
    case d => {
        throw new IllegalArgumentException(
            s"Unexpected direction [$d] for TriangleCell; expecting one of [UpperLeft, UpperRight, Down, Up, LowerLeft, LowerRight]")
    } 
  }

  override def isLinked[TriangleDirection](direction: TriangleDirection): Boolean = direction match {
    case UpperLeft => neighborsByDirection.get("upperleft").isDefined && isLinkedCoords(neighborsByDirection.get("upperleft"))
    case UpperRight => neighborsByDirection.get("upperright").isDefined && isLinkedCoords(neighborsByDirection.get("upperright"))
    case Down => neighborsByDirection.get("down").isDefined && isLinkedCoords(neighborsByDirection.get("down"))
    case Up => neighborsByDirection.get("up").isDefined && isLinkedCoords(neighborsByDirection.get("up"))
    case LowerLeft => neighborsByDirection.get("lowerleft").isDefined && isLinkedCoords(neighborsByDirection.get("lowerleft"))
    case LowerRight => neighborsByDirection.get("lowerright").isDefined && isLinkedCoords(neighborsByDirection.get("lowerright"))
  }

  override def toString(): String = {
    val excessiveNeighborCountErrorMsg = "Illegal linkage for TriangleCell: more than 3 neighbors which is not possible for TriangleCell"
    val impossibleNeighborErrorMsg = "Upward triangle cannot have directions Up, LowerLeft, or LowerRight. Downward triangle cannot have directions UpperLeft, UpperRight, Down"
    val linkedCells: Seq[String] = 
      (linked.contains(neighborsByDirection.get("upperleft").getOrElse(Coordinates(-1, -1))),
       linked.contains(neighborsByDirection.get("upperright").getOrElse(Coordinates(-1, -1))), 
       linked.contains(neighborsByDirection.get("down").getOrElse(Coordinates(-1, -1))), 
       linked.contains(neighborsByDirection.get("up").getOrElse(Coordinates(-1, -1))), 
       linked.contains(neighborsByDirection.get("lowerleft").getOrElse(Coordinates(-1, -1))), 
       linked.contains(neighborsByDirection.get("lowerright").getOrElse(Coordinates(-1, -1)))
      ) match {
        case (true, true, true, true, _, _) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
        case (true, true, true, _, true, _) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
        case (true, true, true, _, _, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
        case (true, _, _, true, true, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
        case (_, true, _, true, true, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
        case (_, _, true, true, true, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
        case (true, _, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (true, _, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (true, _, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, true, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, true, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, true, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, _, true, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, _, true, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, _, true, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (true, _, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, true, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, _, true, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (true, _, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, true, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, _, true, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (true, _, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, true, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (_, _, true, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
        case (true, true, true, _, _, _) => Seq("upperleft", "upperright", "down")
        case (true, true, _, _, _, _) => Seq("upperleft", "upperright")
        case (true, _, true, _, _, _) => Seq("upperleft", "down")
        case (_, true, true, _, _, _) => Seq("upperright", "down")
        case (true, _, _, _, _, _) => Seq("upperleft")
        case (_, true, _, _, _, _) => Seq("upperright")
        case (_, _, true, _, _, _) => Seq("down")
        case (_, _, _, true, true, true) => Seq("up", "lowerleft", "lowerright")
        case (_, _, _, true, true, _) => Seq("up", "lowerleft")
        case (_, _, _, true, _, true) => Seq("up", "lowerright")
        case (_, _, _, _, true, true) => Seq("lowerleft", "lowerright")
        case (_, _, _, true, _, _) => Seq("up")
        case (_, _, _, _, true, _) => Seq("lowerleft")
        case (_, _, _, _, _, true) => Seq("lowerright")
        case (_, _, _, _, _, _) => Seq()
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

object TriangleCell {
  def apply(x: Int, y: Int, orientation: TriangleOrientation): TriangleCell = TriangleCell(coords = Coordinates(x, y), orientation = orientation)
  // def apply(x: Int, y: Int): TriangleCell = TriangleCell(coords = Coordinates(x, y))
}

