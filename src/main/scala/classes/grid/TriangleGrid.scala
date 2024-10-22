package maze.classes.grid

import maze.behaviors.{ Cell, Grid }
import maze.classes.Coordinates
import maze.classes.cell.TriangleCell
import maze.classes.MazeType._
import maze.classes.direction.TriangleDirection._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing

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
    val empty: TriangleGrid = TriangleGrid(height, width, Array[Array[TriangleCell]](), seed, start, goal).copy(cells = Array.ofDim[TriangleCell](height, width))
    val grid: TriangleGrid = empty.copy(cells =
      (for (row <- 0 until empty.height) yield {
        (for (col <- 0 until empty.width) yield {
          TriangleCell(col, row)
        }).toArray
      }).toArray
    )
    // grid.copy(
    //   cells = (for (row <- 0 until grid.height) yield {
    //     var neighborsByDirection = Map[String, Coordinates]() 
    //     // set cells' neighbors
    //     (for (col <- 0 until grid.width) yield {
    //       val coordinates: Coordinates = Coordinates(col, row)
    //       val cell = grid.cells(row)(col)
    //       if (cell.coords.y != 0) {
    //         neighborsByDirection += ("north" -> grid.cells(cell.coords.y - 1)(cell.coords.x).coords)
    //       }
    //       if (cell.coords.x < grid.width - 1) {
    //         neighborsByDirection += ("east" -> (grid.cells(cell.coords.y)(cell.coords.x + 1)).coords)
    //       }
    //       if (cell.coords.y < grid.height - 1) {
    //         neighborsByDirection += ("south" -> (grid.cells(cell.coords.y + 1)(cell.coords.x)).coords)
    //       }
    //       if (cell.coords.x != 0) {
    //         neighborsByDirection += ("west" -> (grid.cells(cell.coords.y)(cell.coords.x - 1)).coords)
    //       }
    //       cell.copy(
    //         neighborsByDirection = neighborsByDirection,
    //         isStart = cell.coords == start,
    //         isGoal = cell.coords == goal)
    //     }).toArray
    //   }).toArray
    // )
    ???
  }
}


