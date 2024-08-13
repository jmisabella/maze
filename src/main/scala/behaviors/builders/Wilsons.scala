package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import maze.classes.Neighbors

import java.util.Random // TODO: remove

trait Wilsons extends Generator {
  type LINKAGE <: Linkage
  val linker: LINKAGE

  private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]], grid: Grid): List[Cell] = {
    // def reverseCoords(coords: Coordinates): Coordinates = Coordinates(coords.y, coords.x) // TODO: remove after the coords reverse bug is fixed
    val neighbors = List(
      (cell.coords.x, cell.coords.y - 1, cell.neighbors.north.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.south.getOrElse(Coordinates(-1, -1)))), // North
      (cell.coords.x, cell.coords.y + 1, cell.neighbors.south.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.north.getOrElse(Coordinates(-1, -1)))), // South
      (cell.coords.x - 1, cell.coords.y, cell.neighbors.west.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.east.getOrElse(Coordinates(-1, -1)))),   // West
      (cell.coords.x + 1, cell.coords.y, cell.neighbors.east.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.west.getOrElse(Coordinates(-1, -1))))   // East
      // (cell.coords.x, cell.coords.y - 1, cell.neighbors.north.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.south.getOrElse(Coordinates(-1, -1))))), // North
      // (cell.coords.x, cell.coords.y + 1, cell.neighbors.south.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.north.getOrElse(Coordinates(-1, -1))))), // South
      // (cell.coords.x - 1, cell.coords.y, cell.neighbors.west.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.east.getOrElse(Coordinates(-1, -1))))),   // West
      // (cell.coords.x + 1, cell.coords.y, cell.neighbors.east.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.west.getOrElse(Coordinates(-1, -1)))))   // East
    ).collect {
      case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < grid.columns && ny < grid.rows && !visited(nx)(ny) =>
        if (wallOpen) {
          connect(grid.cells(nx)(ny))
          // connect(grid.cells(ny)(nx))
        }
        grid.cells(nx)(ny)
        // grid.cells(ny)(nx)
    }
    println("UNVISITED NEIGHBORS: " + neighbors.mkString(","))
    neighbors
  }

  private def connectCells(path: Seq[Cell], grid: Grid): Grid = {
    var nextGrid: Grid = grid
    for (i <- 0 until path.length - 1) {
      var current = path(i)
      var next = path(i + 1)

      val linked = linker.link(Seq(current, next))

      current = linked.head
      next = linked.tail.head
      nextGrid = nextGrid.set(current).set(next)
      println("LINKED")
    }
    nextGrid
  }


  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    val cells: Array[Array[Cell]] = nextGrid.cells
    val random = new Random()
    val (width, height): (Int, Int) = (nextGrid.columns, nextGrid.rows)

    val visited = Array.fill(width, height)(false)
    val stack = scala.collection.mutable.Stack[Cell]()

    // Start from a random cell
    val startX = random.nextInt(width)
    val startY = random.nextInt(height)
    var currentCell = cells(startX)(startY)
    stack.push(currentCell)
    visited(startX)(startY) = true

    while (stack.nonEmpty) {
      // Perform random walks until we find a cell that has already been visited
      currentCell = stack.pop()
      val path = scala.collection.mutable.ListBuffer[Cell]()
      path += currentCell

      var break = false
      while (!break) {
        // Get the neighbors
        val neighbors = getUnvisitedNeighbors(currentCell, visited, nextGrid)

        if (neighbors.isEmpty) {
          // No unvisited neighbors; break the loop
          if (path.length > 1) {
            nextGrid = connectCells(path.toSeq, nextGrid)
          }
          if (stack.isEmpty) {
            break = true
          }
          if (!break) {
            currentCell = stack.pop()
            path.clear()
            path += currentCell
          }
        } else {
          if (!break) {
            // Randomly select a neighbor
            val nextCell = neighbors(random.nextInt(neighbors.length))
            path += nextCell
            stack.push(currentCell)
            currentCell = nextCell
            visited(currentCell.coords.x)(currentCell.coords.y) = true
            // TODO: outstanding bug in which cells' coords are reversed
            // visited(currentCell.coords.y)(currentCell.coords.x) = true
          } 
        }
      }
    }
    nextGrid
  }
}
