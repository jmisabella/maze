package maze.classes

import maze.classes.{ Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.direction.SquareDirection._
import play.api.libs.json.Json

case class Cell (
  coords: Coordinates, 
  mazeType: MazeType,
  neighborsByDirection: Map[String, Coordinates] = Map(),
  linked: Set[Coordinates] = Set(),
  distance: Int = 0,
  isStart: Boolean = false,
  isGoal: Boolean = false,
  onSolutionPath: Boolean = false, 
  visited: Boolean = false,
  value: String = "   "
) {
  
  def neighbors(): Seq[Coordinates] = neighborsByDirection.values.toSeq

  def neighbors[D <: Enumeration#Value](direction: D): Seq[Coordinates] = direction match {
    case North => Seq(neighborsByDirection("north"))
    case East => Seq(neighborsByDirection("east"))
    case South => Seq(neighborsByDirection("south"))
    case West => Seq(neighborsByDirection("west"))
    case d => throw new IllegalArgumentException(s"Unexpected direction [$d] for SquareCell; expecting one of [North, East, South West]")
  }

  def isLinked[SquareDirection](direction: SquareDirection): Boolean = direction match {
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
 
  def visit(visited: Boolean): Cell = Cell(this, visited)

  def unlinkedNeighbors(): Seq[Coordinates] = neighbors().filter(c => !isLinkedCoords(c))
  
  def linkedNeighbors(): Seq[Coordinates] = neighbors().filter(c => linked(c))

  def isLinked[C <: Cell](cell: C, bidi: Boolean = true): Boolean = bidi match {
    case false => linked.contains(cell.coords)
    case true  => linked.contains(cell.coords) && cell.linked(this.coords)
  }
  def isLinkedCoords(coords: Coordinates): Boolean = linked.contains(coords)
  def isLinkedCoords(coords: Option[Coordinates]): Boolean = coords.isDefined && linked.contains(coords.get)
  
  def setLinked(linked: Set[Coordinates]): Cell = {
    Cell(coords, mazeType, neighborsByDirection, linked, distance, isStart, isGoal, onSolutionPath, visited, value)
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
  def apply(mazeType: MazeType, coords: Coordinates, visited: Boolean, neighbors: Map[String, Coordinates], linked: Set[Coordinates]): Cell = {
    apply(coords, mazeType, neighbors, linked, 0, false, false, false, visited, "   ")
  }
  def apply(cell: Cell, isStart: Boolean, isGoal: Boolean): Cell = {
    apply(cell.coords, cell.mazeType, cell.neighborsByDirection, cell.linked, cell.distance, isStart, isGoal, cell.onSolutionPath, cell.visited, cell.value)
  }
  def apply(cell: Cell, visited: Boolean): Cell = {
    apply(cell.coords, cell.mazeType, cell.neighborsByDirection, cell.linked, cell.distance, cell.isStart, cell.isGoal, cell.onSolutionPath, visited, cell.value)
  }
  def apply(cell: Cell, distance: Int, onSolutionPath: Boolean, value: String): Cell = {
    apply(cell.coords, cell.mazeType, cell.neighborsByDirection, cell.linked, distance, cell.isStart, cell.isGoal, onSolutionPath, cell.visited, value)
  }
}
