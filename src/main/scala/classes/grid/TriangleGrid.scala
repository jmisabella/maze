package maze.classes.grid

import maze.behaviors.{ Cell, Grid }
import maze.classes.Coordinates
import maze.classes.cell.TriangleCell
import maze.classes.MazeType._
import maze.classes.direction.TriangleDirection._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing
import maze.classes.cell.TriangleOrientation._

case class TriangleGrid(
  height: Int, 
  width: Int, 
  cells: Array[Array[TriangleCell]],
  seed: RNG,
  startCoords: Coordinates,
  goalCoords: Coordinates) extends Grid[TriangleCell] {

  override val mazeType = Orthogonal
}  
object TriangleGrid {
  def apply(height: Int, width: Int, start: Coordinates, goal: Coordinates): TriangleGrid = {
    
    val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))

    val empty: TriangleGrid = 
      TriangleGrid(
        height, 
        width, 
        Array[Array[TriangleCell]](), 
        seed, 
        start, 
        goal
      ).copy(cells = Array.ofDim[TriangleCell](height, width))

    def triangleOrientation(upward: Boolean) = if (upward) Upward else Downward

    var rowStartsWithUpright: Boolean = true 
    val grid: TriangleGrid = empty.copy(cells =
      (for (row <- 0 until empty.height) yield {
        var upright: Boolean = !rowStartsWithUpright
        rowStartsWithUpright = !rowStartsWithUpright
        (for (col <- 0 until empty.width) yield {
          upright = !upright
          TriangleCell(col, row, triangleOrientation(upright))
        }).toArray
      }).toArray
    )
    grid.copy(
      cells = (for (row <- 0 until grid.height) yield {
        var neighborsByDirection = Map[String, Coordinates]() 
        // set cells' neighbors
        (for (col <- 0 until grid.width) yield {
          val coordinates: Coordinates = Coordinates(col, row)
          val cell = grid.cells(row)(col)
          val left: Option[Coordinates] = if (col > 0) Some(Coordinates(col - 1, row)) else None
          val right: Option[Coordinates] = if (col < width - 1) Some(Coordinates(col + 1, row)) else None
          if (left.isDefined) {
            val key = if (cell.orientation == Upward) "upperleft" else "lowerleft"
            neighborsByDirection += (key -> left.get)
          }
          if (right.isDefined) {
            val key = if (cell.orientation == Upward) "upperright" else "lowerright"
            neighborsByDirection += (key -> right.get)
          }
          val up: Option[Coordinates] = if (cell.orientation == Downward && row > 0) Some(Coordinates(col, row - 1)) else None
          val down: Option[Coordinates] = if (cell.orientation == Upward && row < height - 1) Some(Coordinates(col, row + 1)) else None
          if (up.isDefined) {
            neighborsByDirection += ("up" -> up.get)
          }
          if (down.isDefined) {
            neighborsByDirection += ("down" -> down.get)
          }
          cell.copy(
            neighborsByDirection = neighborsByDirection,
            isStart = cell.coords == start,
            isGoal = cell.coords == goal)
        }).toArray
      }).toArray
    )
    // ???
  }
}


