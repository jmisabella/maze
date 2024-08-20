package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import maze.classes.Neighbors

trait Wilsons extends Generator {

  private def randomWalk(startCell: Cell, unvisited: scala.collection.mutable.Set[Cell], grid: Grid): (List[Cell], Grid) = {
    var nextGrid = grid
    var currentCell = startCell
    val path = scala.collection.mutable.ListBuffer[Cell](currentCell)
    val visitedDuringWalk = scala.collection.mutable.Set[Cell](currentCell)
    while (true) {
      val neighbors: Seq[Coordinates] = currentCell.neighborCoords()
      val unvisitedNeighbors: Seq[Cell] = neighbors.flatMap { coords =>
        val neighborCell = grid.get(coords.x, coords.y)
        if (unvisited.contains(neighborCell) && !visitedDuringWalk.contains(neighborCell)) {
          Some(neighborCell)
        } else {
          None
        }
      }
      if (unvisitedNeighbors.isEmpty) {
        return (path.toList, nextGrid)
      } else {
        val (randomIndex, seed): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
        nextGrid = nextGrid.copy(seed = seed)
        currentCell = unvisitedNeighbors(randomIndex)
        path += currentCell
        visitedDuringWalk.add(currentCell)
      }
    }
    (path.toList, nextGrid)
  }

  def generate(grid: Grid): Grid = {
    var nextGrid = grid 
    var unvisited = scala.collection.mutable.Set[Cell]()
    val startCell = nextGrid.get(nextGrid.startCoords.x, nextGrid.startCoords.y)
    // Mark all cells as unvisited except for the starting cell
    nextGrid.flatten().foreach(cell => unvisited.add(cell))
    unvisited.remove(startCell)
    while (unvisited.nonEmpty) {
      val currentCell = unvisited.head
      val (path, updatedGrid) = randomWalk(currentCell, unvisited, nextGrid)
      nextGrid = updatedGrid
      if (path.nonEmpty) {
        var previousCell = path.head
        path.foreach { cell =>
          if (cell.coords != previousCell.coords) {
            var nextCell = nextGrid.get(cell.coords.x, cell.coords.y)
            if (!previousCell.isLinked(nextCell)) {
              previousCell = previousCell.copy(linked = previousCell.linked ++  Set(nextCell.coords))
              nextCell = nextCell.copy(linked = nextCell.linked ++ Set(previousCell.coords)) // Link back to the original cell
              nextGrid = nextGrid.set(previousCell).set(nextCell)
              //// why does the below line yield different results than when linking without linker??
              // nextGrid = linker.link(previousCell, nextCell, nextGrid)
              previousCell = nextCell
              if (path.length == 1) {
                var lastCell: Cell = path.head
                nextCell = nextCell.copy(linked = nextCell.linked ++ Set(lastCell.coords))
                lastCell = lastCell.copy(linked = lastCell.linked ++ Set(nextCell.coords))
                nextGrid = nextGrid.set(nextCell).set(lastCell)
                //// why does the below line yield different results than when linking without linker??
                // nextGrid = linker.link(nextCell, lastCell, nextGrid)
              }
            }
          }
          // Mark the cell as visited
          unvisited.remove(cell)
        }
      }
    }
    nextGrid.linkUnreachables() // return modified grid
  }


}

