package maze.behaviors

import maze.classes.{ Coordinates, MazeType }
import maze.classes.cell._
import maze.classes.MazeType._

import scala.reflect.ClassTag

trait Cell {
  // abstract values
  def mazeType: MazeType

  val coords: Coordinates
  def neighborsByDirection: Map[String, Coordinates]
  def linked: Set[Coordinates]
  def distance: Int
  def isStart: Boolean
  def isGoal: Boolean
  def onSolutionPath: Boolean
  def visited: Boolean
  def value: String

  // abstract methods
  def isLinked[D <: Enumeration#Value](direction: D): Boolean

  def neighbors(): Seq[Coordinates] = neighborsByDirection.values.toSeq

  def neighbors[D <: Enumeration#Value](direction: D): Seq[Coordinates]
 
  def visit[C <: Cell](visited: Boolean)(implicit ct: ClassTag[C]): C = Cell.instantiate[C](this.asInstanceOf[C], visited)

  def unlinkedNeighbors(): Seq[Coordinates] = neighbors().filter(c => !isLinkedCoords(c))
  
  def linkedNeighbors(): Seq[Coordinates] = neighbors().filter(c => linked(c))

  def isLinked[C <: Cell](cell: C, bidi: Boolean = true): Boolean = bidi match {
    case false => linked.contains(cell.coords)
    case true  => linked.contains(cell.coords) && cell.linked(this.coords)
  }
  def isLinkedCoords(coords: Coordinates): Boolean = linked.contains(coords)
  def isLinkedCoords(coords: Option[Coordinates]): Boolean = coords.isDefined && linked.contains(coords.get)
  
  def setLinked[C <: Cell](linked: Set[Coordinates])(implicit ct: ClassTag[C]): C = {
    Cell.instantiate[C](mazeType, coords, neighborsByDirection, linked, distance, isStart, isGoal, onSolutionPath, visited, value)
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
  def instantiate[C <: Cell](mazeType: MazeType, coords: Coordinates, neighborsByDirection: Map[String, Coordinates], 
    linked: Set[Coordinates], distance: Int, isStart: Boolean = false, isGoal: Boolean = false, onSolutionPath: Boolean = false, 
    visited: Boolean = false, value: String = "   ")(implicit ct: ClassTag[C]): C = mazeType match {

      case Orthogonal => SquareCell(coords = coords, neighborsByDirection = neighborsByDirection, linked = linked, distance = distance, isStart = isStart, isGoal = isGoal, onSolutionPath = onSolutionPath, visited = visited, value = value).asInstanceOf[C]
      case Delta => TriangleCell(coords = coords, neighborsByDirection = neighborsByDirection, linked = linked, distance = distance, isStart = isStart, isGoal = isGoal, onSolutionPath = onSolutionPath, visited = visited, value = value).asInstanceOf[C]
      case Sigma => HexCell(coords = coords, neighborsByDirection = neighborsByDirection, linked = linked, distance = distance, isStart = isStart, isGoal = isGoal, onSolutionPath = onSolutionPath, visited = visited, value = value).asInstanceOf[C]
      case t => throw new IllegalArgumentException("Unexpected MazeType [" + t + "]")
  }
  def instantiate[C <: Cell](mazeType: MazeType, coords: Coordinates, visited: Boolean, neighbors: Map[String, Coordinates], linked: Set[Coordinates])(implicit ct: ClassTag[C]): C = {
    instantiate[C](mazeType, coords, neighbors, linked, 0, false, false, false, visited, "   ")
  }
  def instantiate[C <: Cell](cell: C, isStart: Boolean, isGoal: Boolean)(implicit ct: ClassTag[C]): C = {
    instantiate[C](cell.mazeType, cell.coords, cell.neighborsByDirection, cell.linked, cell.distance, isStart, isGoal, cell.onSolutionPath, cell.visited, cell.value)
  }
  def instantiate[C <: Cell](cell: C, visited: Boolean)(implicit ct: ClassTag[C]): C = {
    instantiate[C](cell.mazeType, cell.coords, cell.neighborsByDirection, cell.linked, cell.distance, cell.isStart, cell.isGoal, cell.onSolutionPath, visited, cell.value)
  }
  def instantiate[C <: Cell](cell: C, distance: Int, onSolutionPath: Boolean, value: String)(implicit ct: ClassTag[C]): C = {
    instantiate[C](cell.mazeType, cell.coords, cell.neighborsByDirection, cell.linked, distance, cell.isStart, cell.isGoal, onSolutionPath, cell.visited, value)
  }
}