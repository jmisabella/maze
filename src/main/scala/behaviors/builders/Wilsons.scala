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

  private def randomWalk(startCell: Cell, unvisited: scala.collection.mutable.Set[Cell], grid: Grid, random: Random): List[Cell] = {
    var currentCell = startCell
    val path = scala.collection.mutable.ListBuffer[Cell](currentCell)
    val visitedDuringWalk = scala.collection.mutable.Set[Cell](currentCell)
    while (true) {
      val neighbors: Seq[Coordinates] = currentCell.availableNeighbors()
      val unvisitedNeighbors: Seq[Cell] = neighbors.flatMap { coords =>
        val neighborCell = grid.get(coords.x, coords.y)
        if (unvisited.contains(neighborCell) && !visitedDuringWalk.contains(neighborCell)) {
          Some(neighborCell)
        } else {
          None
        }
      }
      if (unvisitedNeighbors.isEmpty) {
        return path.toList
      } else {
        currentCell = unvisitedNeighbors(random.nextInt(unvisitedNeighbors.length))
        path += currentCell
        visitedDuringWalk.add(currentCell)
      }
    }
    path.toList
  }


  def generate(grid: Grid): Grid = {
    var nextGrid = grid 
    var unvisited = scala.collection.mutable.Set[Cell]()
    val startCell = nextGrid.get(nextGrid.startCoords.x, nextGrid.startCoords.y)
    // Mark all cells as unvisited except for the starting cell
    nextGrid.flatten().foreach(cell => unvisited.add(cell))
    unvisited.remove(startCell)

    val random = new Random()
    while (unvisited.nonEmpty) {
      val currentCell = unvisited.head
      val path = randomWalk(currentCell, unvisited, nextGrid, random)
      if (path.nonEmpty) {
        var previousCell = path.head
        path.foreach { cell =>
          // if (cell.coords != previousCell.coords) {
            var nextCell = nextGrid.get(cell.coords.x, cell.coords.y)
            // if (!previousCell.isLinked(nextCell)) {
              println("LINKING")
              previousCell = previousCell.copy(linked = previousCell.linked ++  Set(nextCell.coords))
              nextCell = nextCell.copy(linked = nextCell.linked ++ Set(previousCell.coords)) // Link back to the original cell
              nextGrid = nextGrid.set(previousCell).set(nextCell)
              previousCell = nextCell
              if (path.length == 1) {
                var lastCell: Cell = path.head
                nextCell = nextCell.copy(linked = nextCell.linked ++ Set(lastCell.coords))
                lastCell = lastCell.copy(linked = lastCell.linked ++ Set(nextCell.coords))
                nextGrid = nextGrid.set(nextCell).set(lastCell)
              }
            // }
          // }
          // Mark the cell as visited
          if (grid.get(cell).linked.isEmpty) {
            println("CELL " + cell.coords + " IS NOT LINKED, YET IS CONSIDERED VISITED")
          }
          unvisited.remove(cell)
        }
      } else {
        // If no path was found, simply remove the current cell from unvisited
        // unvisited.remove(currentCell)
        // // throw new Exception("**************************** NO PATH WAS FOUND FOR CELL " + currentCell.coords)
      }
    }
    println("RETURNING GRID") 
    println("REMAINING UNVISITED: " + grid.flatten().filter(c => c.linked.isEmpty))
    nextGrid // Return the modified grid
  }


}

