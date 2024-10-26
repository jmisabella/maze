package maze.classes.grid

import maze.behaviors.{ Cell, Grid }
import maze.classes.Coordinates
import maze.classes.cell.HexCell
import maze.classes.MazeType._
import maze.classes.direction.HexDirection._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing

case class HexGrid(
  height: Int, 
  width: Int, 
  cells: Array[Array[HexCell]],
  seed: RNG,
  startCoords: Coordinates,
  goalCoords: Coordinates) extends Grid[HexCell] {

  override val mazeType = Orthogonal
}  
object HexGrid {
  def apply(height: Int, width: Int, start: Coordinates, goal: Coordinates): HexGrid = {
    val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))
    val empty: HexGrid = HexGrid(height, width, Array[Array[HexCell]](), seed, start, goal).copy(cells = Array.ofDim[HexCell](height, width))
    val grid: HexGrid = empty.copy(cells =
      (for (row <- 0 until empty.height) yield {
        (for (col <- 0 until empty.width) yield {
          HexCell(col, row)
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

          def isEven(value: Int): Boolean = value % 2 == 0
          val (northDiagonal, southDiagonal): (Int, Int) = isEven(col) match {
            case true => (row - 1, row)
            case false => (row, row + 1)
          }
          if (col > 0 && northDiagonal >= 0 && northDiagonal < height) {
            neighborsByDirection += ("northwest" -> grid.get(col - 1, northDiagonal).coords)
          } 
          if (col >= 0 && col < width && row > 0) {
            neighborsByDirection += ("north" -> grid.get(col, row - 1).coords)
          }
          if (col < width - 1 && northDiagonal >= 0 && northDiagonal < height) {
            neighborsByDirection += ("northeast" -> grid.get(col + 1, northDiagonal).coords)
          }
          if (col > 0 && southDiagonal >= 0 && southDiagonal < height) {
            neighborsByDirection += ("southwest" -> grid.get(col - 1, southDiagonal).coords)
          }
          if (row < height - 1 && col >= 0 && col < width) {
            neighborsByDirection += ("south" -> grid.get(col, row + 1).coords)
          }
          if (col < width - 1 && southDiagonal >= 0 && southDiagonal < height) {
            neighborsByDirection += ("southeast" -> grid.get(col + 1, southDiagonal).coords)
          }
          // now filter out the out-of-bounds (non-existent) neighbors
          // neighborsByDirection = grid.removeOutOfBoundsNeighbors(neighborsByDirection)
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

