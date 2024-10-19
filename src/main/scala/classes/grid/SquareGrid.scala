package maze.classes.grid

import maze.behaviors.{ Cell, Grid }
import maze.classes.{ Coordinates, MazeType }
import maze.classes.cell.SquareCell
import maze.classes.MazeType._
import maze.classes.direction.SquareDirection._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing
import scala.annotation.tailrec

case class SquareGrid(
  height: Int, 
  width: Int, 
  cells: Array[Array[SquareCell]],
  seed: RNG,
  startCoords: Coordinates,
  goalCoords: Coordinates) extends Grid[SquareCell] {

  override val mazeType = Orthogonal 
  
  override def asci(): String = {
    var output: String = "+" + "---+" * width + "\n"
    for (row <- cells) {
      var top: String = "|"
      var bottom: String = "+"
      for (cell <- row) {
        val body = cell.value
        val eastBoundary: String = cell.neighborsByDirection.get("east").isDefined match {
          case true if (cell.isLinked(East)) => " "
          case _ => "|"
        }
        top += body + eastBoundary
        val southBoundary: String = cell.neighborsByDirection.get("south").isDefined match {
          case true if (cell.isLinked(South)) => "   "
          case _ => "---"
        }
        val corner: String= "+"
        bottom += southBoundary + corner
      }
      output += top + "\n"
      output += bottom + "\n"
    }
    output 
  }
}

object SquareGrid {
  def apply(height: Int, width: Int, start: Coordinates, goal: Coordinates): SquareGrid = {
    val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))
    val empty: SquareGrid = SquareGrid(height, width, Array[Array[SquareCell]](), seed, start, goal).copy(cells = Array.ofDim[SquareCell](height, width))
    val grid: SquareGrid = empty.copy(cells =
      (for (row <- 0 until empty.height) yield {
        (for (col <- 0 until empty.width) yield {
          SquareCell(col, row)
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
          if (cell.coords.y != 0) {
            neighborsByDirection += ("north" -> grid.cells(cell.coords.y - 1)(cell.coords.x).coords)
          }
          if (cell.coords.x < grid.width - 1) {
            neighborsByDirection += ("east" -> (grid.cells(cell.coords.y)(cell.coords.x + 1)).coords)
          }
          if (cell.coords.y < grid.height - 1) {
            neighborsByDirection += ("south" -> (grid.cells(cell.coords.y + 1)(cell.coords.x)).coords)
          }
          if (cell.coords.x != 0) {
            neighborsByDirection += ("west" -> (grid.cells(cell.coords.y)(cell.coords.x - 1)).coords)
          }
          cell.copy(
            neighborsByDirection = neighborsByDirection,
            isStart = cell.coords == start,
            isGoal = cell.coords == goal)
        }).toArray
      }).toArray
    )
  }
}
