package maze.classes.cell

import maze.behaviors.Cell
import maze.classes.{ Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.direction.HexDirection._
import play.api.libs.json.Json

case class HexCell (
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

  override def mazeType: MazeType = Sigma

  override def neighbors[D <: Enumeration#Value](direction: D): Seq[Coordinates] = direction match {
    case NorthWest => Seq(neighborsByDirection("northwest"))
    case North => Seq(neighborsByDirection("north"))
    case NorthEast => Seq(neighborsByDirection("northeast"))
    case SouthWest => Seq(neighborsByDirection("southwest"))
    case South => Seq(neighborsByDirection("south"))
    case SouthEast => Seq(neighborsByDirection("southeast"))
    case d => {
        throw new IllegalArgumentException(
            s"Unexpected direction [$d] for HexCell; expecting one of [UpperLeft, UpperRight, Bottom, Top, LowerLeft, LowerRight]")
    } 
  }

  override def isLinked[HexDirection](direction: HexDirection): Boolean = direction match {
    case NorthWest => neighborsByDirection.get("northwest").isDefined && isLinkedCoords(neighborsByDirection.get("northwest"))
    case North => neighborsByDirection.get("north").isDefined && isLinkedCoords(neighborsByDirection.get("north"))
    case NorthEast => neighborsByDirection.get("northeast").isDefined && isLinkedCoords(neighborsByDirection.get("northeast"))
    case SouthWest => neighborsByDirection.get("southwest").isDefined && isLinkedCoords(neighborsByDirection.get("southwest"))
    case South => neighborsByDirection.get("south").isDefined && isLinkedCoords(neighborsByDirection.get("south"))
    case SouthEast => neighborsByDirection.get("southeast").isDefined && isLinkedCoords(neighborsByDirection.get("southeast"))
  }

  override def toString(): String = {
    val linkedCells: Seq[String] =
      (linked.contains(neighborsByDirection.get("northwest").getOrElse(Coordinates(-1, -1))),
      linked.contains(neighborsByDirection.get("north").getOrElse(Coordinates(-1, -1))),
      linked.contains(neighborsByDirection.get("northeast").getOrElse(Coordinates(-1, -1))),
      linked.contains(neighborsByDirection.get("southwest").getOrElse(Coordinates(-1, -1))),
      linked.contains(neighborsByDirection.get("south").getOrElse(Coordinates(-1, -1))),
      linked.contains(neighborsByDirection.get("southeast").getOrElse(Coordinates(-1, -1)))
    ) match {
      case (true, true, true, true, true, true) => Seq("northwest", "north", "northeast", "southwest", "south", "southeast")
      case (true, true, true, true, true, false) => Seq("northwest", "north", "northeast", "southwest", "south")
      case (true, true, true, true, false, true) => Seq("northwest", "north", "northeast", "southwest", "southeast")
      case (true, true, true, false, true, true) => Seq("northwest", "north", "northeast", "south", "southeast")
      case (true, true, false, true, true, true) => Seq("northwest", "north", "southwest", "south", "southeast")
      case (true, false, true, true, true, true) => Seq("northwest", "northeast", "southwest", "south", "southeast")
      case (false, true, true, true, true, true) => Seq("north", "northeast", "southwest", "south", "southeast")
      case (true, true, true, true, false, false) => Seq("northwest", "north", "northeast", "southwest")
      case (true, true, true, false, true, false) => Seq("northwest", "north", "northeast", "south")
      case (true, true, false, true, false, true) => Seq("northwest", "north", "southwest", "southeast")
      case (true, false, true, false, true, true) => Seq("northwest", "northeast", "south", "southeast")
      case (false, true, false, true, true, true) => Seq("north", "southwest", "south", "southeast")
      case (false, false, true, true, true, true) => Seq("northeast", "southwest", "south", "southeast")
      case (true, true, false, true, false, false) => Seq("northwest", "north", "southwest")
      case (false, true, true, false, true, false) => Seq("north", "northeast", "south")
      case (true, false, true, true, false, true) => Seq("northwest", "northeast", "southwest", "southeast")
      case (false, false, true, true, true, false) => Seq("northeast", "southwest", "south")
      case (true, false, false, true, true, false) => Seq("northwest", "southwest", "south")
      case (false, true, false, true, false, true) => Seq("north", "southwest", "southeast")
      case (false, false, false, true, true, true) => Seq("southwest", "south", "southeast")
      case (true, false, true, false, false, true) => Seq("northwest", "northeast", "southeast")
      case (false, true, false, false, true, true) => Seq("north", "south", "southeast")
      case (false, false, true, false, true, true) => Seq("northeast", "south", "southeast")
      case (true, false, false, true, false, false) => Seq("northwest", "southwest")
      case (false, true, false, false, true, false) => Seq("north", "south")
      case (false, false, true, false, false, true) => Seq("northeast", "southeast")
      case (false, false, false, true, true, false) => Seq("southwest", "south")
      case (false, false, false, false, true, true) => Seq("south", "southeast")
      case (true, false, false, false, false, true) => Seq("northwest", "southeast")
      case (true, false, false, false, false, false) => Seq("northwest")
      case (false, true, false, false, false, false) => Seq("north")
      case (false, false, true, false, false, false) => Seq("northeast")
      case (false, false, false, true, false, false) => Seq("southwest")
      case (false, false, false, false, true, false) => Seq("south")
      case (false, false, false, false, false, true) => Seq("southeast")
      // Fallback for no links
      case (_, _, _, _, _, _) => Seq()
    }

    Json.obj(
        "coords" -> coords,
        "linked" -> linkedCells,
        "distance" -> distance,
        "isStart" -> isStart,
        "isGoal" -> isGoal,
        "onSolutionPath" -> onSolutionPath
    ).toString()
  }
}

object HexCell {
  def apply(x: Int, y: Int): HexCell = HexCell(coords = Coordinates(x, y))
}



