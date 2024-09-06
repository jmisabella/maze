package maze.behaviors

import maze.behaviors.IDirection
import maze.classes.{ Coordinates, SquareNeighbors, SquareCell, MazeType }
import maze.classes.MazeType._

import scala.reflect.ClassTag
// import scala.math.Ordering.Implicits._

trait ICell[N <: INeighbors] {
  // abstract values 
  def mazeType: MazeType  
  val coords: Coordinates
  def neighbors: N
  def linked: Set[Coordinates]
  def distance: Int
  def isStart: Boolean
  def isGoal: Boolean
  def onSolutionPath: Boolean
  def visited: Boolean
  def value: String

  // abstract methods
  def neighborCoords(): Seq[Coordinates]
  def isLinked[D <: IDirection](direction: D): Boolean

  def visit[C <: ICell[N]](visited: Boolean)(implicit ct: ClassTag[C]): C = ICell.instantiate[N, C](this.asInstanceOf[C], visited)

  // def sortList[N <: INeighbors, C <: ICell[N]](list: Seq[C]): Seq[C]

  def unlinkedNeighbors(): Seq[Coordinates] = neighborCoords().filter(c => !isLinked(c))
  
  def linkedNeighbors(): Seq[Coordinates] = neighborCoords().filter(c => !linked(c))

  def isLinked[C <: ICell[N]](cell: C, bidi: Boolean = true): Boolean = bidi match {
    case false => linked.contains(cell.coords)
    case true  => linked.contains(cell.coords) && cell.linked(this.coords)
  }
  def isLinked(coords: Coordinates): Boolean = linked.contains(coords)

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
  
  // implicit def ordering [N <: INeighbors, C <: ICell[N]]: Ordering[C] = Ordering.by(_.coords.inverse())

}

object ICell {
  def instantiate[N <: INeighbors, C <: ICell[N]](mazeType: MazeType, coords: Coordinates, neighbors: N, 
    linked: Set[Coordinates], distance: Int, isStart: Boolean = false, isGoal: Boolean = false, onSolutionPath: Boolean = false, 
    visited: Boolean = false, value: String = "   ")(implicit ct: ClassTag[C]): C = mazeType match {

      case Square => SquareCell(coords = coords, neighbors = neighbors.asInstanceOf[SquareNeighbors], linked = linked, distance = distance, isStart = isStart, isGoal = isGoal, onSolutionPath = onSolutionPath, visited = visited, value = value).asInstanceOf[C]
      case t => throw new IllegalArgumentException("Unexpected MazeType [" + t + "]")
  }
  def instantiate[N <: INeighbors, C <: ICell[N]](mazeType: MazeType, coords: Coordinates, visited: Boolean, neighbors: N, linked: Set[Coordinates])(implicit ct: ClassTag[C]): C = {
    instantiate[N, C](mazeType, coords, neighbors, linked, 0, false, false, false, visited, "   ")
  }
  def instantiate[N <: INeighbors, C <: ICell[N]](cell: C, isStart: Boolean, isGoal: Boolean)(implicit ct: ClassTag[C]): C = {
    instantiate[N, C](cell.mazeType, cell.coords, cell.neighbors, cell.linked, cell.distance, isStart, isGoal, cell.onSolutionPath, cell.visited, cell.value)
  }
  def instantiate[N <: INeighbors, C <: ICell[N]](cell: C, linked: Set[Coordinates])(implicit ct: ClassTag[C]): C = {
    instantiate[N, C](cell.mazeType, cell.coords, cell.neighbors, linked, cell.distance, cell.isStart, cell.isGoal, cell.onSolutionPath, cell.visited, cell.value)
  }
  def instantiate[N <: INeighbors, C <: ICell[N]](cell: C, visited: Boolean)(implicit ct: ClassTag[C]): C = {
    instantiate[N, C](cell.mazeType, cell.coords, cell.neighbors, cell.linked, cell.distance, cell.isStart, cell.isGoal, cell.onSolutionPath, visited, cell.value)
  }
  def instantiate[N <: INeighbors, C <: ICell[N]](cell: C, distance: Int, onSolutionPath: Boolean, value: String)(implicit ct: ClassTag[C]): C = {
    instantiate[N, C](cell.mazeType, cell.coords, cell.neighbors, cell.linked, distance, cell.isStart, cell.isGoal, onSolutionPath, cell.visited, value)
  }

  // // not sure why calling inverse on coords when sorting a list needs to use the inverse to prevent transposing x/y grid
  // implicit def ordering [N <: INeighbors, C <: ICell[N]]: Ordering[C] = Ordering.by(_.coords.inverse())
}