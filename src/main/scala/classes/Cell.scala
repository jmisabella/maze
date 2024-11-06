package maze.classes

import maze.classes.{ Coordinates, MazeType, MazeRequest }
import maze.classes.MazeType._
import maze.classes.direction.{ SquareDirection, TriangleDirection, HexDirection }
import maze.classes.direction.SquareDirection._
import maze.classes.direction.TriangleDirection._
import maze.classes.direction.HexDirection._
import play.api.libs.json.{ Json, Format }

object CellOrientation extends Enumeration {
  type CellOrientation = Value
  val Normal, Inverted = Value
  implicit val format: Format[CellOrientation] = Json.formatEnum(this)
  def fromString(s: String): Option[CellOrientation] = values.find(_.toString.toLowerCase == s.toLowerCase())
}
import CellOrientation._

case class Cell (
  coords: Coordinates, 
  mazeType: MazeType,
  neighborsByDirection: Map[String, Coordinates] = Map(),
  linked: Set[Coordinates] = Set(),
  distance: Int = 0,
  isStart: Boolean = false,
  isGoal: Boolean = false,
  onSolutionPath: Boolean = false, 
  orientation: CellOrientation = Normal,
  visited: Boolean = false,
  value: String = "   "
) {

  val x: Int = coords.x
  val y: Int = coords.y

  def neighbors(): Seq[Coordinates] = neighborsByDirection.values.toSeq

  def neighbors[D <: Enumeration#Value](direction: D): Seq[Coordinates] = mazeType match {
    case Orthogonal => direction match {
      case SquareDirection.North => Seq(neighborsByDirection("north"))
      case SquareDirection.East => Seq(neighborsByDirection("east"))
      case SquareDirection.South => Seq(neighborsByDirection("south"))
      case SquareDirection.West => Seq(neighborsByDirection("west"))
      case d => throw new IllegalArgumentException(s"Rejecting illegal direction [$d] for maze type Orthogonal (square cells): expecting one of [North, East, South West]")
    }
    case Delta => direction match {
      case TriangleDirection.UpperLeft => Seq(neighborsByDirection("upperleft"))
      case TriangleDirection.UpperRight => Seq(neighborsByDirection("upperright"))
      case TriangleDirection.Down => Seq(neighborsByDirection("down"))
      case TriangleDirection.Up => Seq(neighborsByDirection("up"))
      case TriangleDirection.LowerLeft => Seq(neighborsByDirection("lowerleft"))
      case TriangleDirection.LowerRight => Seq(neighborsByDirection("lowerright"))
      case d => {
          throw new IllegalArgumentException(
              s"Rejectng illegal direction [$d] for maze type Delta (triangle cells): expecting one of [UpperLeft, UpperRight, Down, Up, LowerLeft, LowerRight]")
      }
    } 
    case Sigma => direction match {
      case HexDirection.NorthWest => Seq(neighborsByDirection("northwest"))
      case HexDirection.North => Seq(neighborsByDirection("north"))
      case HexDirection.NorthEast => Seq(neighborsByDirection("northeast"))
      case HexDirection.SouthWest => Seq(neighborsByDirection("southwest"))
      case HexDirection.South => Seq(neighborsByDirection("south"))
      case HexDirection.SouthEast => Seq(neighborsByDirection("southeast"))
      case d => {
          throw new IllegalArgumentException(
              s"Rejecting illegal directin [$d] for maze type Sigma (hex cells): expecting one of [UpperLeft, UpperRight, Bottom, Top, LowerLeft, LowerRight]")
      }
    }
    case t => throw new IllegalArgumentException(s"Rejecting unexpected MazeType [$t]. Accepted values: [Orthogonal, Delta, Sigma]")
  }


  // def isLinkedDirection[SquareDirection](direction: SquareDirection): Boolean = direction match {
  //   case North => neighborsByDirection.get("north").isDefined && isLinkedCoords(neighborsByDirection.get("north"))
  //   case East => neighborsByDirection.get("east").isDefined && isLinkedCoords(neighborsByDirection.get("east"))
  //   case South => neighborsByDirection.get("south").isDefined && isLinkedCoords(neighborsByDirection.get("south"))
  //   case West => neighborsByDirection.get("west").isDefined && isLinkedCoords(neighborsByDirection.get("west"))
  //   case d => throw new IllegalArgumentException(s"Unexpected direction [$d] for SquareCell; expecting one of [North, East, South West]")
  // }

  override def toString(): String = {
    val linkedCells: Seq[String] = mazeType match {
      case Orthogonal => {
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
      }
      case Delta => {
        val excessiveNeighborCountErrorMsg = "Illegal linkage for Delta maze (triangle cells): More than 3 neighbors which is not possible for triangle cell"
        val impossibleNeighborErrorMsg = "Illegal linkage for Delta maze (triangle cells): Upward triangle cannot have directions Up, LowerLeft, or LowerRight. Downward triangle cannot have directions UpperLeft, UpperRight, Down"
        (linked.contains(neighborsByDirection.get("upperleft").getOrElse(Coordinates(-1, -1))),
        linked.contains(neighborsByDirection.get("upperright").getOrElse(Coordinates(-1, -1))), 
        linked.contains(neighborsByDirection.get("down").getOrElse(Coordinates(-1, -1))), 
        linked.contains(neighborsByDirection.get("up").getOrElse(Coordinates(-1, -1))), 
        linked.contains(neighborsByDirection.get("lowerleft").getOrElse(Coordinates(-1, -1))), 
        linked.contains(neighborsByDirection.get("lowerright").getOrElse(Coordinates(-1, -1)))
        ) match {
          // case (true, true, true, true, _, _) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
          // case (true, true, true, _, true, _) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
          // case (true, true, true, _, _, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
          // case (true, _, _, true, true, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
          // case (_, true, _, true, true, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
          // case (_, _, true, true, true, true) => throw new IllegalStateException(excessiveNeighborCountErrorMsg)
          // case (true, _, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (true, _, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (true, _, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, true, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, true, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, true, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, _, true, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, _, true, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, _, true, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (true, _, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, true, _, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, _, true, true, _, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (true, _, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, true, _, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, _, true, _, true, _) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (true, _, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, true, _, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
          // case (_, _, true, _, _, true) => throw new IllegalStateException(impossibleNeighborErrorMsg)
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
      }
      case Sigma => {
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
      }
      case t => throw new IllegalArgumentException(s"Rejecting unexpected MazeType [$t]. Accepted values: [Orthogonal, Delta, Sigma]")
    } 
  (Json.obj(
      "coords" -> coords,
      "linked" -> linkedCells,
      "distance" -> distance,
      "isStart" -> isStart,
      "isGoal" -> isGoal, 
      "onSolutionPath" -> onSolutionPath,
      "orientation" -> orientation.toString()
    )).toString()
  }
 
  def visit(visited: Boolean): Cell = Cell(this, visited)

  def unlinkedNeighbors(): Seq[Coordinates] = neighbors().filter(c => !isLinkedCoords(c))
  
  def linkedNeighbors(): Seq[Coordinates] = neighbors().filter(c => linked(c))

  def isLinked(cell: Cell, bidi: Boolean = true): Boolean = bidi match {
    case false => linked.contains(cell.coords)
    case true  => linked.contains(cell.coords) && cell.linked(this.coords)
  }
  def isLinked[D <: Enumeration#Value](direction: D): Boolean = {
    // neighborsByDirection.keySet.contains(direction.toString().toLowerCase()) && !neighbors(direction).isEmpty && linked.contains(neighbors(direction).head)
    neighborsByDirection.keySet.contains(direction.toString().toLowerCase()) && linked.contains(neighbors(direction).head)
  }
  def isLinkedCoords(coords: Coordinates): Boolean = linked.contains(coords)
  def isLinkedCoords(coords: Option[Coordinates]): Boolean = coords.isDefined && linked.contains(coords.get)
  
  def setLinked(linked: Set[Coordinates]): Cell = {
    Cell(coords, mazeType, neighborsByDirection, linked, distance, isStart, isGoal, onSolutionPath, orientation, visited, value)
  }

  def padRight(s: String, c: Char, n: Int): String = s.padTo(n, c).mkString
  def padLeft(s: String, c: Char, n: Int): String = n match {
    case 0 => s
    case x if (x < 0) => s
    case _ => padRight(s, c, n).split(s).tail.mkString + s
  }
  // evenly pad left and right; left has 1 extra padding in case of an odd length 
  def pad(s:String, c: Char, n:Int): String = {
    val left = (n - s.length) / 2
    val right = n - left - s.length
    c.toString * left + s + c.toString * right
  }

}

object Cell {
// //   def apply(x: Int, y: Int): Cell = Cell(coords = Coordinates(x, y))
//   def apply(mazeType: MazeType, coords: Coordinates, neighborsByDirection: Map[String, Coordinates], 
//     linked: Set[Coordinates], distance: Int, isStart: Boolean = false, isGoal: Boolean = false, onSolutionPath: Boolean = false, 
//     visited: Boolean = false, value: String = "   "): Cell = {

//       Cell(coords = coords, mazeType = mazeType, neighborsByDirection = neighborsByDirection, linked = linked, distance = distance, isStart = isStart, isGoal = isGoal, onSolutionPath = onSolutionPath, visited = visited, value = value).asInstanceOf[C]
//   }
  def apply(mazeType: MazeType, coords: Coordinates, orientation: CellOrientation, visited: Boolean, neighbors: Map[String, Coordinates], linked: Set[Coordinates]): Cell = {
    apply(coords, mazeType, neighbors, linked, 0, false, false, false, orientation, visited, "   ")
  }
  def apply(mazeType: MazeType, coords: Coordinates, orientation: CellOrientation, isStart: Boolean, isGoal: Boolean): Cell = {
    apply(coords, mazeType, Map(), Set(), 0, isStart, isGoal, false, orientation, false, "   ")
  }
  def apply(mazeType: MazeType, coords: Coordinates, orientation: CellOrientation): Cell = {
    apply(coords, mazeType, Map(), Set(), 0, false, false, false, orientation, false, "   ")
  }
  def apply(cell: Cell, isStart: Boolean, isGoal: Boolean): Cell = {
    apply(cell.coords, cell.mazeType, cell.neighborsByDirection, cell.linked, cell.distance, isStart, isGoal, cell.onSolutionPath, cell.orientation, cell.visited, cell.value)
  }
  def apply(cell: Cell, visited: Boolean): Cell = {
    apply(cell.coords, cell.mazeType, cell.neighborsByDirection, cell.linked, cell.distance, cell.isStart, cell.isGoal, cell.onSolutionPath, cell.orientation, visited, cell.value)
  }
  def apply(cell: Cell, distance: Int, onSolutionPath: Boolean, value: String): Cell = {
    apply(cell.coords, cell.mazeType, cell.neighborsByDirection, cell.linked, distance, cell.isStart, cell.isGoal, onSolutionPath, cell.orientation, cell.visited, value)
  }
}
